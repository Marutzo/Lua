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

-- Recursive descent parser
local function parse(tokens, env)
    local pos = 1
    local function peek() return tokens[pos] end
    local function nextToken() pos = pos + 1; return tokens[pos-1] end

    local expr, term, factor, power, base, comparison

    base = function()
        local tok = nextToken()
        if type(tok) == "number" then
            return tok
        elseif tok == "(" then
            local val = comparison()
            if nextToken() ~= ")" then error("Expected )") end
            return val
        elseif type(tok) == "string" then
            local val = resolve(tok, env)
            if type(val) ~= "number" then error("Variable '"..tok.."' is not a number") end
            return val
        else
            error("Unexpected token: "..tostring(tok))
        end
    end

    factor = function()
        if peek() == "-" then
            nextToken()
            return -factor()
        else
            return base()
        end
    end

    power = function()
        local val = factor()
        while peek() == "^" do
            nextToken()
            val = val ^ power()
        end
        return val
    end

    term = function()
        local val = power()
        while peek() == "*" or peek() == "/" or peek() == "%" or peek() == "//" do
            local op = nextToken()
            local right = power()
            if op == "*" then val = val * right
            elseif op == "/" then val = val / right
            elseif op == "%" then val = val % right
            elseif op == "//" then val = math.floor(val / right) end
        end
        return val
    end

    expr = function()
        local val = term()
        while peek() == "+" or peek() == "-" do
            local op = nextToken()
            local right = term()
            if op == "+" then val = val + right else val = val - right end
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

    local result = comparison()
    if pos <= #tokens then error("Unexpected token at end") end
    return result
end

-- Interpolator
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
                -- just "\{" without closing "\}"
                table.insert(out, "{")
                i = i + 2
            end

        -- Interpolation block: { ... }
        elseif c == "{" then
            local close = str:find("}", i+1)
            if close then
                local expr = str:sub(i+1, close-1)

                -- Direct table/variable lookup
                local direct = resolve(expr, env)
                if direct ~= nil and type(direct) ~= "table" then
                    table.insert(out, tostring(direct))
                else
                    -- Handle expression evaluation
                    local hasB = false
                    if expr:sub(-1) == "b" then
                        hasB = true
                        expr = expr:sub(1, -2) -- strip trailing "b"
                    end

                    local success, val = pcall(function()
                        local tokens = tokenize(expr)
                        return parse(tokens, env)
                    end)

                    if success and val ~= nil then
                        if type(val) == "boolean" then
                            if hasB then
                                table.insert(out, val and "1" or "0")
                            else
                                table.insert(out, tostring(val))
                            end
                        else
                            table.insert(out, tostring(val))
                        end
                    else
                        table.insert(out, "{"..expr.."}")
                    end
                end
                i = close + 1
            else
                -- lone "{"
                table.insert(out, c)
                i = i + 1
            end

        else
            -- normal character passthrough
            table.insert(out, c)
            i = i + 1
        end
    end
    return table.concat(out)
end


-- Helper
function withVars(env)
    return function(s) return interp(s, env) end
end


-- Big demo environment
local env = {
    score = 42,
    bonus = 8,
    multiplier = 2,
    divisor = 5,
    exp = {
        current = 25,
        nextLevel = 100
    },
    player = {
        name = "Retro",
        level = 5,
        stats = {
            hp = 120,
            mp = 40
        }
    },
    pi = 3.14159,
    neg = -7
}

local say = withVars(env)

-- Showcase tests
print("=== Simple Variables ===")
print(say("Player: {player.name}"))                  -- Retro
print(say("Score: {score}"))                         -- 42
print(say("Negative literal: {-5}"))                 -- -5
print(say("Unary minus variable: {-score}"))         -- -42

print("\n=== Table Lookup ===")
print(say("EXP: {exp.current}/{exp.nextLevel}"))     -- 25/100
print(say("HP: {player.stats.hp}, MP: {player.stats.mp}")) -- 120, 40

print("\n=== Math & PEMDAS ===")
print(say("{score + bonus}"))                        -- 50
print(say("{score - bonus}"))                        -- 34
print(say("{score * multiplier}"))                   -- 84
print(say("{score / divisor}"))                      -- 8.4
print(say("{score // divisor}"))                     -- 8
print(say("{score % divisor}"))                      -- 2
print(say("{score ^ 2}"))                            -- 1764
print(say("{(score + bonus) * multiplier}"))         -- 100
print(say("{score + bonus * multiplier}"))           -- 58

print("\n=== Comparisons ===")
print(say("{(score > bonus)}"))                      -- true
print(say("{(exp.current >= exp.nextLevel)}"))       -- false
print(say("{(player.level == 5)}"))                  -- true
print(say("{(player.level != 10)}"))                 -- true
print(say("{(score > exp.current)b}"))               --  true as 1 or explicit b suffix? (your choice handling)

print("\n=== Escaping ===")
print(say("Literal brace: \\{this is not interpolated\\}")) -- this is not interpolated
print(say("The result of \\{score} is {score}"))     -- The result of score is 42
print(say("Escaped nested: \\{{score} + {bonus}\\}"))-- {score} + {bonus}
