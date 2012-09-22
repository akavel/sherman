--
-- Scheme interpreter in Lua
-- This code is in the public domain
-- Alex Sandro Queiroz e Silva <asandroq@gmail.com>
-- https://github.com/AndreasKostler/lua-scheme

local l_pairs = pairs
local l_type = type
local l_error = error
local l_xpcall = xpcall
local l_print = print
local l_unpack = unpack
local l_io_open = io.open
local l_io_input = io.input
local l_io_output = io.output
local l_io_write = io.write
local l_tonumber = tonumber
local l_traceback = debug.traceback
local l_setmetatable = setmetatable
local l_tconcat = table.concat

module("scheme")

-- the global environment
local global_env = {}

-- The end-of-file object
local eof_obj = {}

-- the empty list object
local nil_obj = {}

-- the "undefined" object
local undef_obj = {}

-- booleans
local bool_true = {}
local bool_false = {}

local function is_nil(obj)
	return obj == nil_obj
end
local function is_true(obj)
	return obj ~= bool_false
end
local function is_number(obj)
	return l_type(obj) == "number"
end
local function is_symbol(obj)
	return l_type(obj) == "string"
end


local function is(obj, kind)
	return l_type(obj)=="table" and obj[1]==kind
end
local function is_char(obj)
	return is(obj, "char")
end
local function is_pair(obj)
	return is(obj, "pair")
end
local function is_continuation(obj)
	return is(obj, "continuation")
end
local function is_primitive(obj)
	return is(obj, "primitive")
end
local function is_procedure(obj)
	return is(obj, "procedure")
end
local function is_string(obj)
	return is(obj, "string")
end
local function is_vector(obj)
	return is(obj, "vector")
end

local function make_string(str)
	local strobj = {
		[1] = "string"
	}
	for i = 1, #str do
		strobj[i+1] = str:sub(i,i)
	end
	return strobj
end

local function make_char(c)
	return {[1] = "char", [2] = c}
end

local function make_pair(x, y)
	local pair = {[1] = "pair", [2] = nil_obj, [3] = nil_obj}

	if x then
		pair[2] = x
		if y then
			pair[3] = y
		end
	end

	return pair
end

local function verify(x, kind)
	if not is(x, kind) then
		l_error("Not a " .. kind)
	end
	return x
end

local function make_continuation(cont)
	return {[1] = "continuation", [2] = cont}
end
local function continuation_procedure(cont)
	return verify(cont, "continuation")[2]
end

local function make_primitive(f, arity)
	return {[1] = "primitive", [2] = f, [3] = arity}
end
local function primitive_function(pri)
	return verify(pri, "primitive")[2]
end
local function primitive_arity(pri)
	return verify(pri, "primitive")[3]
end

local function make_procedure(args, env, body)
	return {[1] = "procedure", [2] = args, [3] = env, [4] = body, ["doc"] = ""}
end
local function procedure_args(proc)
	return verify(proc, "procedure")[2]
end
local function procedure_env(proc)
	return verify(proc, "procedure")[3]
end
local function procedure_body(proc)
	return verify(proc, "procedure")[4]
end

--
-- Primitives
--

local function nullp(obj)
	if is_nil(obj) then
		return bool_true
	else
		return bool_false
	end
end

local function make_numerical_comparator(comp)
	return function(...)
		local args = {...}

		if #args < 2 then
			l_error("Comparison of less than two elements")
		end

		local test = args[1]
		if not is_number(test) then
			l_error("Numerical comparator applied to non-number")
		end
		for i = 2, #args do
			local val = args[i]
			if not is_number(val) then
				l_error("Numerical comparator applied to non-number")
			end
			if not comp(test, val) then
				return bool_false
			end
			test = val
		end

		return bool_true
	end
end

local eq = make_numerical_comparator(function(a, b) return a == b end)
local lt = make_numerical_comparator(function(a, b) return a < b end)
local ge = make_numerical_comparator(function(a, b) return a > b end)
local le = make_numerical_comparator(function(a, b) return a <= b end)
local ge = make_numerical_comparator(function(a, b) return a >= b end)

local function add(...)
	local args = {...}

	if #args == 0 then
		return 0
	end

	local sum = 0
	for i = 1, #args do
		if not is_number(args[i]) then
			l_error("Adding non-number")
		end
		sum = sum + args[i]
	end
	return sum
end

local function sub(...)
	local args = {...}

	if #args == 0 then
		l_error("No arguments to subtraction procedure")
	end
	if #args == 1 then
		if not is_number(args[1]) then
			l_error("Subtracting non-number")
		end
		return 0 - args[1]
	end

	local res = args[1]
	for i = 2, #args do
		if not is_number(args[i]) then
			l_error("Subtracting non-number")
		end
		res = res - args[i]
	end
	return res
end

local function mul(...)
	local args = {...}

	if #args == 0 then
		return 1
	end

	local prod = 1
	for i = 1, #args do
		if not is_number(args[i]) then
			l_error("Multiplying non-number")
		end
		prod = prod * args[i]
	end
	return prod
end

local function div(...)
	local args = {...}

	if #args == 0 then
		l_error("No arguments to division procedure")
	end
	if #args == 1 then
		if not is_number(args[1]) then
			l_error("Dividing non-number")
		end
		if args[1] == 0 then
			l_error("Dividing by zero")
		end
		return 1 / args[1]
	end

	local res = args[1]
	for i = 2, #args do
		if not is_number(args[i]) then
			l_error("Dividing non-number")
		end
		if args[i] == 0 then
			l_error("Dividing by zero")
		end
		res = res / args[i]
	end
	return res
end

local function cons(x, y)
	return {[1] = "pair", [2] = x, [3] = y}
end

local function car(obj)
	if not is_pair(obj) then
		l_error("Not a pair passed to CAR")
	end
	return obj[2]
end

local function cdr(obj)
	if not is_pair(obj) then
		l_error("Not a pair passed to CDR")
	end
	return obj[3]
end

local function length(obj)
	local function l(d, n)
		if is_nil(d) then
			return n
		elseif is_pair(d) then
			return l(cdr(d), n + 1)
		else
			l_error("List expected")
		end
	end
	return l(obj, 0)
end

local function doc(obj)
	if not is_procedure(obj) then
		return ""
	end
	return obj["doc"]
end

local function cadr(obj)
	return car(cdr(obj))
end
local function cddr(obj)
	return cdr(cdr(obj))
end
local function caddr(obj)
	return car(cdr(cdr(obj)))
end
local function cadddr(obj)
	return car(cdr(cdr(cdr(obj))))
end

-- The reader
local function read(port)
	local last_char = false
	local port = port or l_io_input()

	local function is_space(c)
		return c == " " or c == "\n" or c == "\t" or c == "\r"
	end

	local function peek_char()
		if last_char == false then
			last_char = port:read(1)
		end
		
		return last_char
	end

	local function get_char()
		local char = peek_char()

		-- set to false so the next time I ask for a char,
		-- it reads another one
		last_char = false

		return char
	end

	local function eat_space()
		local char

		char = peek_char()
		while is_space(char) do
			get_char()
			char = peek_char()
		end
	end

	local function inner_read()

		local function read_till_delimiter()
			local char
			local str = {}
			
			while true do
				char = peek_char()
				if is_space(char) or char == nil
						or char == "(" or char == ")"
						or char == "\"" or char == ";" then
					return l_tconcat(str)
				else
					str[#str+1] = get_char()
				end
			end
		end

		local function read_list()
			local pair = make_pair()
			local p = pair

			while true do
				local exp = inner_read()
				p[2] = exp
				eat_space()
				local c = peek_char()
				if c == ")" then
					get_char()
					break
				elseif c == "." then
					get_char()
					p[3] = inner_read()
					eat_space()
					c = peek_char()
					if c == ")" then
						get_char()
					else
						l_error("Unknown read syntax reading improper list: " .. c)
					end
					break
				else
					local p2 = make_pair()
					p[3] = p2
					p = p2
				end
			end

			return pair
		end

		local function read_string()
			local str = {[1] = "string"}

			local char = get_char()
			while char ~= "\"" do
				str[#str+1] = char
				char = get_char()
			end

			return str
		end

		local function read_symbol(first)
			local rest = read_till_delimiter()

			local str = first .. rest
			if str:match("[%d%+%-%.%a%$%%%*%?%^!&/:<=>_~@]+") then
				return str
			else
				l_error("Unknown read syntax: " .. str)
			end
		end

		local function read_vector()
			local char
			local vec = {[1] = "vector"}
			
			eat_space()
			char = peek_char()
			while char ~= ")" do
				local exp = inner_read()
				if exp == eof_obj then
					l_error("Unterminated vector")
				else
					vec[#vec+1] = exp
				end
				eat_space()
				char = peek_char()
			end
			get_char()

			return vec
		end

		local char = get_char()

		-- discarding white spaces and comments
		while is_space(char) or char == ";" do
			while is_space(char) do
				char = get_char()
			end
			
			if char == ";" then
				while char ~= "\n" do
					char = get_char()
				end
			end
		end

		-- reading
		if char == nil then
			return eof_obj
		elseif char == "'" or char == "`" then
			local exp = inner_read()
			local p1 = make_pair()
			local p2 = make_pair()
			if char == "'" then
				p1[2] = "quote"
			else
				p1[2] = "quasiquote"
			end
			p1[3] = p2
			p2[2] = exp
			p2[3] = nil_obj
			return p1
		elseif char == "," then
			local p1 = make_pair()
			local p2 = make_pair()
			p1[2] = "unquote"
			char = peek_char()
			if char == "@" then
				get_char()
				p1[2] = "unquote-splicing"
			end
			local exp = inner_read()
			p1[3] = p2
			p2[2] = exp
			p2[3] = nil_obj
			return p1
		elseif char == "#" then
			local c = peek_char()
			if c == "(" then
				get_char()
				return read_vector()
			elseif c == "\\" then
				-- characters
				get_char()
				c = read_till_delimiter()
				if c == "space" then
					return make_char(" ")
				elseif c == "newline" then
					return make_char("\n")
				elseif c:len() == 1 then
					return make_char(c)
				else
					l_error("Unknown character: " .. c)
				end
			else
				local str = read_till_delimiter()
				if str == "f" then
					return bool_false
				elseif str == "t" then
					return bool_true
				else
					l_error("Unknown read syntax: #" .. c)
				end
			end
		elseif char == "(" then
			local c
			eat_space()
			c = peek_char()
			if c == ")" then
				get_char()
				return nil_obj
			else
				return read_list()
			end
		elseif char == "\"" then
			return read_string()
		elseif char:match("%d") then
			local rest = read_till_delimiter()
			local str = char .. rest
			local num = l_tonumber(str)
			if num then
				return num
			else
				l_error("Unknown reader syntax: " .. str)
			end
		elseif char:match("[%a%$%%%*%?%^!&/:<=>_~]") then
			return read_symbol(char)
		elseif char == "+" or char == "-" then
			local rest = read_till_delimiter()
			if rest == "" then
				return char
			end
			local num = l_tonumber(rest)
			if not num then
				l_error("Unknown read syntax: " .. char .. rest)
			end
			if char == "+" then
				return num
			else
				return -num
			end
		elseif char == "." then
			local rest = read_till_delimiter()
			if rest == ".." then
				return "..."
			else
				l_error("Unknown read syntax: ." .. rest)
			end
		else
			l_error("Unknown read syntax: "  .. char)
		end
	end

	return inner_read()
end

local function write(exp, port)

	local port = port or l_io_output()

	local function l_write(obj)
		return port:write(obj)
	end

	if exp == eof_obj then
		l_write("#<eof>")
	elseif exp == nil_obj then
		l_write("()")
	elseif exp == undef_obj then
		l_write("#<undefined>")
	elseif exp == bool_true then
		l_write("#t")
	elseif exp == bool_false then
		l_write("#f")
	elseif l_type(exp) == "string" or l_type(exp) == "number" then
		l_write(exp)
	elseif is_char(exp) then
		if exp[2] == " " then
			l_write("#\\space")
		elseif exp[2] == "\n" then
			l_write("#\\newline")
		else
			l_write("#\\")
			l_write(exp[2])
		end
	elseif is_pair(exp) then
		l_write("(")
		local first = true
		while true do
			if not first then
				l_write(" ")
			end
			first = false
			write(exp[2], port)
			if is_pair(exp[3]) then
				exp = exp[3]
			elseif exp[3] == nil_obj then
				break
			else
				l_write(" . ")
				write(exp[3], port)
				break
			end
		end
		l_write(")")
	elseif is_procedure(exp) then
		l_write("#<procedure>")
	elseif is_string(exp) then
		l_write("\""..l_tconcat(exp, "", 2).."\"")
	elseif is_vector(exp) then
		l_write("#(")
		for i = 2, #exp do
			if i ~= 2 then
				l_write(" ")
			end
			write(exp[i], port)
		end
		l_write(")")
	else
		l_error("Unknown Scheme type to write")
	end

	return undef_obj
end

local function newline(port)
	local port = port or l_io_output()

	port:write("\r\n")

	return undef_obj
end

local function eval(exp, env)

	local env = env or global_env

	-- looks for the value of a binding in the environment.
	-- environments are tables chained by their values at [1], since all
	-- bindings are stored with string keys
	local function lookup(exp, env)
		local val = env[exp]
		if val then
			return val
		end

		if env[1] then
			return lookup(exp, env[1])
		end

		l_error("No such binding: " .. exp)
	end

	local function update(var, val, env)
		if env[var] then
			env[var] = val
		elseif env[1] then
			return update(var, val, env[1])
		else
			l_error("Assignment to unitialised variable")
		end
	end

	local function evaluate(exp, env, cont)

		local function evaluate_begin(body, env, cont)
			if is_nil(body) then
				return cont(undef_obj)
			end

			local exp = car(body)
			if is_nil(cdr(body)) then
				return evaluate(exp, env, cont)
			end

			return evaluate(exp, env, function(v)
				return evaluate_begin(
					cdr(body),
					env,
					cont
				)
			end)
		end

		-- this is let, not let*, so the bindings are evaluated in the old env,
		-- which we must carry along
		local function evaluate_let(bindings, new_env, env, body, cont)
			if is_nil(bindings) then
				return evaluate_begin(body, new_env, cont)
			end

			local binding = car(bindings)
			local var = car(binding)
			local exp = cadr(binding)
			if not is_symbol(var) then
				l_error("Not binding to symbol")
			end

			return evaluate(exp, env, function(v)
				new_env[var] = v
				return evaluate_let(
					cdr(bindings),
					new_env,
					env,
					body,
					cont
				)
			end)
		end

		local function evaluate_apply(args, vals, new_env, env, body, cont)
			if is_nil(args) then
				return evaluate_begin(body, new_env, cont)
			end

			local arg = car(args)
			local exp = car(vals)
			if not is_symbol(arg) then
				l_error("Not binding to symbol")
			end

			return evaluate(exp, env, function(v)
				new_env[arg] = v
				return evaluate_apply(
					cdr(args),
					cdr(vals),
					new_env,
					env,
					body,
					cont
				)
			end)
		end

		local function evaluate_primitive(f, args, vals, env, cont)
			if is_nil(vals) then
				return cont(f(l_unpack(args)))
			end

			local val = car(vals)
			return evaluate(val, env, function(v)
				args[#args+1] = v
				return evaluate_primitive(
					f,
					args,
					cdr(vals),
					env,
					cont
				)
			end)
		end

		if not is_pair(exp) then
			if is_symbol(exp) then
				local val = lookup(exp, env)
				return cont(val)
			else
				return cont(exp)
			end
		end

		local op = car(exp)
		if op == "quote" then
			return cont(cadr(exp))
		elseif op == "begin" then
			return evaluate_begin(cdr(exp), env, cont)
		elseif op == "call/cc" then
			return evaluate(cadr(exp), env, function(v)
				local args = procedure_args(v)
				local fenv = procedure_env(v)
				local body = procedure_body(v)
				local proc = make_continuation(cont)
				local vals = cons(proc, nil_obj)
				local new_env = {[1] = fenv}
				return evaluate_apply(args, vals,
					new_env, env,
					body, cont
				)
			end)
		elseif op == "define" then
			local var = cadr(exp)
			local docstring = caddr(exp)
			if is_string(docstring) then
				exp = cdr(exp)
			else
				docstring = make_string("No documentation available.")
			end
			if is_pair(var) then
				local fname = car(var)
				local docstring = cadr(var)
				local args = cdr(var)
				if is_string (docstring) then
					args = cddr(var)
				else
					docstring = make_string("No documentation available.")
				end
				local body = cddr(exp)
				local proc = make_procedure(args, env, body)
				proc["doc"] = docstring
				if not is_symbol(fname) then
					l_error("Procedure name must be a symbol!")
				end

				env[fname] = proc
				return cont(undef_obj)
			else
				if not is_symbol(var) then
					l_error("Assignment to non-symbol!")
				end
				return evaluate(caddr(exp), env, function(v)
					v["doc"] = docstring
					env[var] = v
					return cont(undef_obj)
				end)
			end
		elseif op == "if" then
			return evaluate(cadr(exp),
				env,
				function(v)
					if is_true(v) then
						return evaluate(caddr(exp), env, cont)
					else
						return evaluate(cadddr(exp), env, cont)
					end
				end
			)
		elseif op == "lambda" then
			local args = cadr(exp)
			local body = cddr(exp)

			return cont(make_procedure(args, env, body))
		elseif op == "let" then
			local body = cddr(exp)
			local bindings = cadr(exp)

			if is_nil(bindings) then
				return evaluate_begin(body, env, cont)
			elseif is_pair(bindings) then
				local new_env = {[1] = env}

				return evaluate_let(bindings, new_env, env, body, cont)
			else
				l_error("Invalid bindings in let form")
			end
		elseif op == "set!" then
			local var = cadr(exp)
			if not is_symbol(var) then
				l_error("Assignment to non-symbol!")
			end

			return evaluate(caddr(exp), env, function(v)
				update(var, v, env)
				return cont(undef_obj)
			end)
		else
			-- function application
			return evaluate(op, env, function(v)
				local vals = cdr(exp)
				if is_continuation(v) then
					local k = continuation_procedure(v)

					return evaluate(car(vals),
						env,
						function(v)
							return k(v)
						end
					)
				elseif is_procedure(v) then
					local args = procedure_args(v)
					local fenv = procedure_env(v)
					local body = procedure_body(v)
					local new_env = {[1] = fenv}

					return evaluate_apply(
						args, vals, new_env,
						env, body, cont
					)
				elseif is_primitive(v) then
					local num_vals = length(vals)
					local f = primitive_function(v)
					local a = primitive_arity(v)
					if a >= 0 and a ~= num_vals then
						l_error("Incorrect arity of primitive")
					end
					return evaluate_primitive(
						f, {}, vals,
						env, cont
					)
				else
					l_error("Trying to apply non-procedure")
				end
			end)
		end
	end

	return evaluate(
		exp,
		env,
		function(v)
			return v
		end
	)
end

local function add_primitive(name, f, arity)
	global_env[name] = make_primitive(f, arity)
end
add_primitive("=", eq, -1)
add_primitive("<", lt, -1)
add_primitive(">", gt, -1)
add_primitive("<=", le, -1)
add_primitive(">=", ge, -1)
add_primitive("+", add, -1)
add_primitive("-", sub, -1)
add_primitive("*", mul, -1)
add_primitive("/", div, -1)
add_primitive("car", car, 1)
add_primitive("cdr", cdr, 1)
add_primitive("cons", cons, 2)
add_primitive("null?", nullp, 1)
add_primitive("length", length, 1)
add_primitive("write", write, 1)
add_primitive("newline", newline, 0)
add_primitive("doc", doc, 1)

--
-- Module interface
--

-- loads files
function load(fname)
	local status, exp

	if (not fname) or fname == "" then
		return
	end

	local port, err = l_io_open(fname, "rb")
	if not port then
		l_io_write("Error opening Scheme file: " .. err)
		return
	end

	while true do
		status, exp = l_xpcall(
			function()
				return read(port)
			end,
			l_traceback
		)
		if not status then
			l_io_write("Error: " .. exp .. "\r\n")
			port:close()
			return
		end

		if exp == eof_obj then
			port:close()
			return
		end

		status, exp = l_xpcall(
			function()
				return eval(exp)
			end,
			l_traceback
		)
		if not status then
			l_io_write("Error: " .. exp .. "\r\n")
			port:close()
			return
		end
	end
end

-- the REPL
function repl()
	local function toplevel()
		local status, exp
		l_io_write("scheme> ")

		status, exp = l_xpcall(read, l_traceback)
		if not status then
			l_io_write("Error: " .. exp .. "\r\n")
			return toplevel()
		end

		if exp == eof_obj then
			return true
		end

		status, exp = l_xpcall(
			function()
				return eval(exp)
			end,
			l_traceback
		)
		if not status then
			l_io_write("Error: " .. exp .. "\r\n")
			return toplevel()
		end

		write(exp)
		l_io_write("\r\n")
		return toplevel()
	end
	return toplevel()
end
