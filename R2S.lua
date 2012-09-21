
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
local R = lpeg.R

-- parser state
local line = 1
local discretionary_space = 0

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
local function write(s)
	io.write(s)
end
local whitespace = S' \t'^1
local newline = P'\n' / function()
	line = line+1 
	discretionary_space = 0
end
-- Colon is part of symbol constituent so that things like
--  foo/:bar/baz  will match.
local symbol_initial = R'az' + S'/`!&_?|<=>*~.+-'
local symbol_constituent = symbol_initial + R'09' + P':'

-- Grammar
local rebol = P{
	"rebol";           -- name of rule to start with

	rebol = V'preamble' * V'body',
	preamble =
		whitespace^0 *
		newline^0 *
		anycase 'rebol' *
		whitespace^0 *
		newline^0 *
		#P'[',
	body = (
		(P'[' / function()
			write ',(LIST->BLOCK `('
		end) +
		(P']' / function()
			write ')) '    -- Space just in case
			discretionary_space = 1
		end) +
		newline / function()
			write '\n'
		end +
		whitespace / function(s)
			write(s:sub(1, -1-discretionary_space))
			discretionary_space = 0
		end +
		V'symbol' / function(s)
			write(s:gsub("[|.`'\"\\]", function(c)
				return '\\'..c
			end))
			write ' '  -- just in case
			discretionary_space = 1
		end
	)^0,
	symbol =    -- FIXME: add the rest of patterns
		symbol_initial * symbol_constituent^0,
}

write '`(SHERMAN ,(LIST->BLOCK `('
local success = rebol:match(code)
if not success then
	error("parsing error on line "..line)
end
write '\n))) ;; END of SHERMAN CODE'


end -- function parse()

------------------------------------
------------------------------------

-- TODO: parse input while reading
function main()
	local data = io.read '*all'
	parse(data)
end

main()
