
require 'lpeg'

--[[
The BOGUS types generally match illegal trailing context so as to avoid
having things like  foo12:22:33bar get split into three tokens.
]]

function parse(code)

-- LPEG shorthands
local Cc = lpeg.Cc
local Cf = lpeg.Cf
local V = lpeg.V
local S = lpeg.S
local P = lpeg.P

-- utility functions, etc.
local function anycase(s)
	local function char(n)
		local c = s:sub(n,n)
		return S(c:upper()..c:lower())
	end
	local result = char(1)
	for i = 2,#s do
		result = result * char(i)
	end
	return result
end
local whitespace = S' \t'^1

-- parser state
local line = 1

-- Grammar
local rebol = P{
	"rebol";           -- name of rule to start with

	rebol = 
		whitespace^0 *
		(P'\n' / function() line = line+1 end)^0 *
		anycase 'rebol' / function() print 'hi' print(line) end,
	--rebol = V'initial' * V'body',
	--initial = 
}

local success = rebol:match(code)
if not success then
	error("parsing error on line "..line)
end

end -- function parse()

------------------------------------
------------------------------------

-- TODO: parse input while reading
function main()
	local data = io.read '*all'
	parse(data)
end

main()
