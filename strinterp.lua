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

-- Tokenizer (handles multi-char operators)
local function tokenize(expr)
    local tokens = {}
    local i = 1
    while i <= #expr do
        local c = expr:sub(i,i)
        if c:match("%s") then
            i = i + 1
        else
            local two = expr:sub(i,i+1)
            if two == "//" or two == "==" or two == "!=" or two == "<=" or two == ">=" then
                table.insert(tokens, two)
                i = i + 2
            elseif c:match("[%+%-%*%%%^/%(%)]") or c == "<" or c == ">" then
                table.insert(tokens, c)
                i = i + 1
            elseif c:match("%d") then
                local num = expr:match("^%d+%.?%d*", i)
                table.insert(tokens, tonumber(num))
                i = i + #num
            elseif c:match("[%a_]") then
                local var = expr:match("^[%a_][%w_%.]*", i)
                table.insert(tokens, var)
                i = i + #var
            else
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

-- Recursive descent parser with boolean<->number coercion helpers
local function parse(tokens, env)
    local pos = 1
    local function peek() return tokens[pos] end
    local function nextToken() pos = pos + 1; return tokens[pos-1] end

    -- helpers
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

    local expr, term, factor, power, base, comparison, logic

    base = function()
        local tok = nextToken()
        if type(tok) == "number" then
            return tok
        elseif tok == "(" then
            local val = logic()
            if nextToken() ~= ")" then error("Expected )") end
            return val
        elseif type(tok) == "string" then
            if tok == "not" then
                -- logical not: work on boolean coercion
                return not toBool(base())
            elseif tok == "true" then
                return true
            elseif tok == "false" then
                return false
            else
                local val = resolve(tok, env)
                if type(val) == "number" or type(val) == "boolean" then
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
            val = toNumber(val) ^ toNumber(rhs)  -- coerce booleans to numbers for math
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

    comparison = function()
        local val = expr()
        while peek() == "==" or peek() == "!=" or peek() == "<" or peek() == ">" or peek() == "<=" or peek() == ">=" do
            local op = nextToken()
            local right = expr()
            if op == "==" then val = (val == right)
            elseif op == "!=" then val = (val ~= right)
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

-- Interpolator (merged, with escaping, b-flag for booleans, and num formatting)
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
                table.insert(out, "{")
                i = i + 2
            end

        -- Interpolation block: { ... }
        elseif c == "{" then
            local close = str:find("}", i+1)
            if close then
                local original_expr = str:sub(i+1, close-1)
                local expr = original_expr:gsub("^%s*", ""):gsub("%s*$", "") -- trim both ends

                -- detect trailing 'b' flag inside braces (eg. {(a>b)b} or {(a>b)}b form handled by user)
                local hasB = false
                if expr:sub(-1) == "b" then
                    hasB = true
                    expr = expr:sub(1, -2)
                    expr = expr:gsub("^%s*", ""):gsub("%s*$", "")
                end

                -- Direct table/variable lookup on trimmed inner expr
                local direct = resolve(expr, env)
                if direct ~= nil and type(direct) ~= "table" then
                    -- direct variable: print formatted number/boolean appropriately
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
                    -- Evaluate expression
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
                        else
                            table.insert(out, numToString(val))
                        end
                    else
                        -- fallback: preserve original (untrimmed) expression inside braces
                        table.insert(out, "{"..original_expr.."}")
                    end
                end

                i = close + 1
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
