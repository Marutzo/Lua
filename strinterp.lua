-- Safe resolver for table fields
local function resolve(name, env)
    local val = env
    for part in name:gmatch("[^.]+") do
        if type(val) == "table" then
            val = val[part]
        else
            return nil
        end
    end
    return val
end

-- Tokenizer (handles multi-char operators, string literals, keywords)
local function tokenize(expr)
    local tokens = {}
    local i = 1
    while i <= #expr do
        local c = expr:sub(i,i)
        if c:match("%s") then
            i = i + 1
        else
            local two = expr:sub(i,i+1)
            local three = expr:sub(i,i+2)

            -- keywords: and/or/not (make sure not part of identifier)
            if three == "and" and not expr:sub(i+3,i+3):match("[%w_]") then
                table.insert(tokens, "and"); i = i + 3
            elseif two == "or" and not expr:sub(i+2,i+2):match("[%w_]") then
                table.insert(tokens, "or"); i = i + 2
            elseif three == "not" and not expr:sub(i+3,i+3):match("[%w_]") then
                table.insert(tokens, "not"); i = i + 3

            -- multi-char operators (order matters)
            elseif two == "//" or two == "==" or two == "!=" or two == "~=" or two == "<=" or two == ">=" or two == ".." then
                table.insert(tokens, two)
                i = i + 2

            -- string literal
            elseif c == "'" or c == '"' then
                local quote = c
                local j = i + 1
                local buf = {}
                while j <= #expr do
                    local ch = expr:sub(j,j)
                    if ch == "\\" and j < #expr then
                        -- escape next char
                        table.insert(buf, expr:sub(j+1,j+1))
                        j = j + 2
                    elseif ch == quote then
                        break
                    else
                        table.insert(buf, ch)
                        j = j + 1
                    end
                end
                table.insert(tokens, {is_str = true, val = table.concat(buf)})
                i = j + 1

            -- single-char operators and punctuation (includes comma)
            elseif c:match("[%+%-%*%%%^/%(%),<>]") then
                table.insert(tokens, c)
                i = i + 1

            -- number
            elseif c:match("%d") then
                local num = expr:match("^%d+%.?%d*", i)
                table.insert(tokens, tonumber(num))
                i = i + #num

            -- identifier (may include dots)
            elseif c:match("[%a_]") then
                local var = expr:match("^[%a_][%w_%.]*", i)
                table.insert(tokens, var)
                i = i + #var

            else
                -- unknown char: skip
                i = i + 1
            end
        end
    end
    return tokens
end

-- Helper: format numbers cleanly (remove .0 if integer)
local function numToString(n)
    if type(n) == "number" then
        if n % 1 == 0 then
            return string.format("%d", n)
        else
            return tostring(n)
        end
    else
        return tostring(n)
    end
end

-- Parser with function-call and concat support
local function parse(tokens, env)
    local pos = 1
    local function peek() return tokens[pos] end
    local function nextToken() pos = pos + 1; return tokens[pos-1] end

    local function toBool(x)
        if type(x) == "boolean" then return x end
        if type(x) == "number" then return x ~= 0 end
        return x ~= nil
    end
    local function toNumber(x)
        if type(x) == "number" then return x end
        if type(x) == "boolean" then return x and 1 or 0 end
        error("expected number in arithmetic, got "..type(x))
    end
    local function toStrForConcat(x)
        if type(x) == "number" then return numToString(x) end
        if type(x) == "boolean" then return tostring(x) end
        if type(x) == "string" then return x end
        return tostring(x)
    end

    local expr, term, factor, power, base, comparison, logic

    -- parse argument list for function calls
    local function parseArgs()
        local args = {}
        if peek() ~= ")" then
            repeat
                table.insert(args, logic()) -- full expression allowed as arg
                if peek() == "," then nextToken() end
            until peek() == ")"
        end
        return args
    end

    base = function()
        local tok = nextToken()
        -- string literal token stored as table {is_str=true, val=...}
        if type(tok) == "table" and tok.is_str then
            return tok.val
        elseif type(tok) == "number" then
            return tok
        elseif type(tok) == "string" then
            if tok == "(" then
                local val = logic()
                if nextToken() ~= ")" then error("Expected )") end
                return val
            elseif tok == "not" then
                return not toBool(base())
            elseif tok == "true" then
                return true
            elseif tok == "false" then
                return false
            else
                local val = resolve(tok, env)
                -- function call?
                if peek() == "(" then
                    nextToken() -- consume "("
                    local args = parseArgs()
                    if nextToken() ~= ")" then error("Expected ) after args") end
                    if type(val) ~= "function" then
                        error("Attempt to call non-function: "..tok)
                    end
                    return val(table.unpack(args))
                end
                -- variable
                if type(val) == "number" or type(val) == "boolean" or type(val) == "string" then
                    return val
                else
                    error("Variable '"..tok.."' is not usable in expression")
                end
            end
        else
            error("Unexpected token: "..tostring(tok))
        end
    end

    factor = function()
        if peek() == "-" then
            nextToken()
            return -toNumber(factor())
        else
            return base()
        end
    end

    power = function()
        local val = factor()
        while peek() == "^" do
            nextToken()
            local rhs = power()
            val = toNumber(val) ^ toNumber(rhs)
        end
        return val
    end

    term = function()
        local val = power()
        while peek() == "*" or peek() == "/" or peek() == "%" or peek() == "//" do
            local op = nextToken()
            local right = power()
            local a, b = toNumber(val), toNumber(right)
            if op == "*" then
                val = a * b
            elseif op == "/" then
                val = a / b
            elseif op == "%" then
                val = a % b
            elseif op == "//" then
                val = math.floor(a / b)
            end
        end
        return val
    end

    expr = function()
        local val = term()
        while peek() == "+" or peek() == "-" do
            local op = nextToken()
            local right = term()
            local a, b = toNumber(val), toNumber(right)
            if op == "+" then val = a + b else val = a - b end
        end
        return val
    end

    -- concatenation .. (lower precedence than + - so we run expr() first)
    local function concatLevel()
        local val = expr()
        while peek() == ".." do
            nextToken()
            local right = expr()
            val = toStrForConcat(val) .. toStrForConcat(right)
        end
        return val
    end

    comparison = function()
        local val = concatLevel()
        while peek() == "==" or peek() == "!=" or peek() == "~=" or peek() == "<" or peek() == ">" or peek() == "<=" or peek() == ">=" do
            local op = nextToken()
            local right = concatLevel()
            if op == "==" then val = (val == right)
            elseif op == "!=" or op == "~=" then val = (val ~= right)
            elseif op == "<" then val = (val < right)
            elseif op == ">" then val = (val > right)
            elseif op == "<=" then val = (val <= right)
            elseif op == ">=" then val = (val >= right) end
        end
        return val
    end

    logic = function()
        local val = comparison()
        while peek() == "and" or peek() == "or" do
            local op = nextToken()
            local right = comparison()
            local b1, b2 = toBool(val), toBool(right)
            if op == "and" then val = (b1 and b2) else val = (b1 or b2) end
        end
        return val
    end

    local result = logic()
    if pos <= #tokens then error("Unexpected token at end") end
    return result
end

-- Interpolator (with escaping, b-flag, function-calls, formatting)
function interp(str, env)
    local out = {}
    local i = 1
    while i <= #str do
        local c = str:sub(i,i)

        -- Escaped literal block: \{ ... \}
        if c == "\\" and str:sub(i+1,i+1) == "{" then
            local close = str:find("\\}", i+2)
            if close then
                local literal = str:sub(i+2, close-1)
                table.insert(out, literal)
                i = close + 2
            else
                -- lone \{ -> literal {
                table.insert(out, "{")
                i = i + 2
            end

        -- Interpolation block: { ... }
        elseif c == "{" then
            local close = str:find("}", i+1)
            if close then
                local original_expr = str:sub(i+1, close-1)
                local expr = original_expr:gsub("^%s*", ""):gsub("%s*$", "") -- trim both ends

                -- detect trailing 'b' inside braces
                local hasB = false
                if expr:sub(-1) == "b" then
                    hasB = true
                    expr = expr:sub(1, -2):gsub("%s*$", "")
                end

                -- detect trailing b after the closing brace, e.g. {(a>b)}b
                local afterB = false
                if not hasB and str:sub(close+1, close+1) == "b" then
                    hasB = true
                    afterB = true
                end

                -- direct lookup
                local direct = nil
                local ok, rd = pcall(function() return resolve(expr, env) end)
                if ok then direct = rd end

                if direct ~= nil and type(direct) ~= "table" then
                    if type(direct) == "boolean" then
                        if hasB then
                            table.insert(out, tostring(direct))
                        else
                            table.insert(out, direct and "1" or "0")
                        end
                    else
                        table.insert(out, numToString(direct))
                    end
                else
                    local success, val = pcall(function()
                        local tokens = tokenize(expr)
                        return parse(tokens, env)
                    end)
                    if success and val ~= nil then
                        if type(val) == "boolean" then
                            if hasB then
                                table.insert(out, tostring(val))
                            else
                                table.insert(out, val and "1" or "0")
                            end
                        elseif type(val) == "number" then
                            table.insert(out, numToString(val))
                        else
                            table.insert(out, tostring(val))
                        end
                    else
                        table.insert(out, "{"..original_expr.."}")
                    end
                end

                if afterB then
                    i = close + 2
                else
                    i = close + 1
                end
            else
                table.insert(out, c)
                i = i + 1
            end

        else
            table.insert(out, c)
            i = i + 1
        end
    end
    return table.concat(out)
end

-- Helper to bind environment
function withVars(env)
    return function(s) return interp(s, env) end
end

---------------------------------------------------------------------------------------------

-- Stress test suite

local env = {
    score = 42,
    bonus = 8,
    multiplier = 2,
    divisor = 5,
    neg = -7,
    exp = { current = 25, nextLevel = 100 },
    player = {
        name = "Retro",
        level = 5,
        stats = { hp = 120, mp = 40 }
    },
    secretLevelCompleted = true,
    didNotCheat = false,
    pi = 3.14159
}

local test = withVars(env)

print("=== Simple Variables & Literals ===")
print(test("Name: {player.name}"))               -- Retro
print(test("Score: {score}"))                    -- 42
print(test("Negative literal: {-123}"))          -- -123
print(test("Unary minus variable: {-score}"))    -- -42
print(test("Double minus: {--score}"))           -- 42
print(test("Negative table lookup: {-exp.current}")) -- -25

print("\n=== Math & PEMDAS ===")
print(test("{score + bonus * multiplier}"))      -- 58 (bonus * multiplier first)
print(test("{(score + bonus) * multiplier}"))    -- 100
print(test("{score ^ 2}"))                       -- 1764
print(test("{score / divisor}"))                 -- 8.4
print(test("{score // divisor}"))                -- 8
print(test("{score % divisor}"))                 -- 2
print(test("{(bonus + 2) ^ multiplier}"))        -- 100
print(test("{(score - bonus) * (multiplier + 3)}")) -- 170

print("\n=== Comparisons ===")
print(test("{(score > bonus)b}"))                 -- true
print(test("{(score < bonus)b}"))                 -- false
print(test("{(exp.current >= exp.nextLevel)b}"))  -- false
print(test("{(player.level == 5)b}"))             -- true
print(test("{(player.level != 10)b}"))            -- true
print(test("{(score > exp.current)}"))          -- 1 (without b flag)
print(test("{(score < exp.current)}"))          -- 0 (without b flag)

print("\n=== Logic ===")
print(test("{(secretLevelCompleted and didNotCheat)b}"))   -- false
print(test("{(secretLevelCompleted or didNotCheat)b}"))    -- true
print(test("{(not didNotCheat)b}"))                        -- true
print(test("{(not secretLevelCompleted)b}"))               -- false
print(test("{(secretLevelCompleted and not didNotCheat)}")) -- 1
print(test("{(secretLevelCompleted and not didNotCheat)b}")) -- true

print("\n=== Mixed Math + Logic ===")
print(test("Total bonus: {(player.level * multiplier) + (30 * (secretLevelCompleted and not didNotCheat))}"))
-- (5 * 2) + (30 * 1) = 40
print(test("{score + (bonus * (secretLevelCompleted and didNotCheat))}"))
-- 42 + (8 * 0) = 42
print(test("{score + (bonus * (secretLevelCompleted or didNotCheat))}"))
-- 42 + (8 * 1) = 50

print("\n=== Escaping ===")
print(test("Literal: \\{not interpolated\\}"))    -- not interpolated
print(test("Show braces: \\{{score} + {bonus}\\}")) -- {score} + {bonus}
print(test("The result of \\{score} is {score}")) -- The result of score is 42
print(test("Weird nested: \\{{player.level} >= {multiplier}\\}")) -- {player.level} >= {multiplier}

print("\n=== Edge Cases ===")
print(test("{((score + bonus) * 2) // (multiplier + 3)}")) -- ((42+8)*2)//5 = 20
print(test("{((exp.nextLevel - exp.current) / exp.nextLevel) * 100}")) -- 75.0
print(test("{pi * (player.stats.hp - player.stats.mp)}"))  -- 3.14159 * 80 ≈ 251.327
print(test("{(score > bonus) * 10}"))             -- true coerces to 1 → 10
print(test("{(score < bonus) * 10}"))             -- false coerces to 0 → 0
print(test("{(score > bonus) + (exp.current < exp.nextLevel)}")) -- 1 + 1 = 2

-- Add some functions to env
env.max = math.max
env.min = math.min
env.pow = math.pow
env.floor = math.floor
env.upper = string.upper
env.concat = function(a, b) return a .. b end

print("\n=== Function Calls ===")
print(test("{max(score, bonus)}"))              -- 42 vs 8 → 42
print(test("{min(score, bonus)}"))              -- 8
print(test("{pow(2, 10)}"))                     -- 1024
print(test("{floor(8.9)}"))                     -- 8
print(test("{upper(player.name)}"))             -- RETRO
print(test("{concat(player.name, '_L' .. player.level)}")) -- Retro_L5
print(test("{max(score, bonus * multiplier)}")) -- max(42, 16) = 42
print(test("{pow(score + bonus, multiplier)}")) -- (42+8)^2 = 2500

