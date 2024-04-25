local function req(src, mod)
  mod = mod or ""
  local m = pcall(require, src) and require(src)
  or pcall(require, mod.."."..src) and  require( mod.."."..src)  -- luapower sub module path
  or nil

  assert(m, 'parser-gen depend on "'..src..'" from "'..mod..'". Please check your path')
  return m
end

local pg = require "parser-gen"
local peg = require "peg-parser"
local re = req("relabel", "lpeglabel")
 
local eq = req("equals", "parser-gen")
local equals = eq.equals

local serpent = pcall(require, "serpent") and require "serpent" or function(...) print(...)  end


local pr = peg.print_r
local res,res1,res2, errs

local function cleanGram(s) -- remove leading and ending spaces and tabs
  return (s:gsub("^%s*(.-)%s*$", "%1"):gsub("[\9 ]+", " "))
end

local ast_test_number = 0
local function tst_ast(str, gram, options, test_name)
-- rule round trip
  options = options or {}
  ast_test_number = ast_test_number + 1
  test_name = test_name or 'anon#'..ast_test_number
  print('\nast_tst ('..test_name..') start...')
  
  local ast = peg.pegToAST(gram, options['definitions'])
  local gram2 = pg.astToPEG(ast, {recovery=false, skipspaces=false, nocaptures=true, re_useext=true})

  print('Given grammar: '..serpent.block(cleanGram(gram)))
  print('New grammar: '..serpent.block(cleanGram(gram2)))
  print('Given str: '..serpent.block(str))

  if not options['not_exact_grammar'] then
--    assert(cleanGram(gram) == cleanGram(gram2))
  end
  local c1,e1 = pg.parse(str, gram, options)
  local c2,e2 = pg.parse(str, gram2, options)

  print('Given grammar captures: '..serpent.block(c1))
  print('New grammar captures: '..serpent.block(c2))
  print('Given grammar errors: '..serpent.block(e1))
  print('New grammar errors: '..serpent.block(e2))

  assert(equals(c1, c2)) -- same captures
  assert(equals(e1, e2)) -- same error

  local m1, me1 = pg.parse( str, pg.compile( gram, options))

  local ast3, options2 = pg.genErrorRecovery(ast, options)
  local gram3, options3 = pg.astToPEG(ast3, options2)
  
  if not options3.re_useext then
    local p3 = re.compile(gram3, options3['definitions'])
    local m3, me3 = p3:match(str)
    assert( equals(m1, m3))
  end
  print('ast_tst ('..test_name..') ...finish\n')
  return m1, me1
end


-- test the ast to PEG round trip

-- terminals
-- space allowed
local rule =  [[
rule <-  'a'
]]
local str = "a   a aa "
tst_ast(str, rule) 

-- space not allowed
-- space not allowed
rule =  [[
RULE <- 'a' 'b'
]]
str = "a     b"
tst_ast(str, rule) 

-- space not allowed 2
rule =   [[
rule <- 'a' 'b'
SKIP <- ''
SYNC <- ''
]]
str = "a     b"
tst_ast(str, rule) 

-- custom space
rule =   [[
rule <- 'a' 'b'
SKIP <- DOT
DOT <- '.'
]]
str = "a...b"
tst_ast(str, rule)   --was disabled

-- non terminals
-- space allowed
rule =   [[
rule <- A B
A	<- 'a'
B	<- 'b'
]]
str = "a     b"
tst_ast(str, rule) 

-- no spaces allowed
rule =   [[
RULE <- A B
A	<- 'a'
B	<- 'b'
]]
str = "a     b"
tst_ast(str, rule) 

-- space in the beginning and end of string
rule =  [[
rule <- A B
A	<- 'a'
B	<- 'b'
]]
str = "  a     b  "
tst_ast(str, rule) 

rule = [[ R <- 'a' ( 'b' / 'c' )]]
str = 'ab'
res = tst_ast(str, rule, {not_exact_grammar = true})  --was disabled
-- testing ranges
rule = [[ r <- {[a1b]* } ]]
str = "a1b"
res, err1 = pg.parse(str, rule, {nocaptures=true})
local res2 = re.compile(rule):match(str)
assert(equals(res, res2 )) 

res = pg.parse(str, rule,{nocaptures=false})


-- testing space in class
rule = [[ r <- {[a1b]*} ]]
str = "a 1b"
res,err1 =  tst_ast(str, rule, {skipspaces = true, nocaptures=true} )
assert(equals(res,"a")) 
local res2 = re.compile(rule):match(str)

rule = [[ r <- [a1]*  ]]
str = "a a"
local res = tst_ast(str, rule)
assert(res[1]== "a") 


-- testing quote in class
rule = [[ r <- {[b']*}  ]]
str = "b'b"
res = tst_ast(str, rule)
assert( res[1] =="b'b") 
 
rule = [[ r <- {[a-z_A-Z]*}  ]]
str = "e_E_"
res = tst_ast(str, rule)
assert(res[1] =="e_E_") 


rule = [[ r <- {[^A-Z]*} ]]
str = "aaaA"
res = tst_ast(str, rule)
assert(res[1] =="aaa") 

-- testing quote 

rule =[[
string <- "'"""
]]

res, errs = tst_ast('"""', rule )

rule =[[
string <- ('"'  {[^"]*} '"' / "'"  {[^']*} "'") 
]]

res, errs = tst_ast("'bob'", rule )

-- TESTING CAPTURES

rule = [[ rule <- {| {:'a' 'b':}* |}				]]
str = "ababab"
tst_ast(str, rule, {nocaptures=true})

-- space in capture

rule =[[ rule <- {| {:'a':}* |} 
]]
str = " a a a "
res = tst_ast(str, rule, {nocaptures=true})

assert(equals(res,{"a","a","a"})) 



-- testing fragment

rule = [[rule <- A
fragment A	<- [a-z]*
]]
str = " abc def "
res = tst_ast(str, rule, {re_useext=true})



-- TESTING ERROR LABELS
local labs = {errName = "Error number 1",errName2 = "Error number 2"}

rule = [[ rule <- 'a' / %{errName}
SYNC <- '' 
]]
str = "b"  
local errorcalled = false
res = tst_ast(str, rule, {labels=labs , errorfunction=  function (desc, line, col, sfail, recexp)
      errorcalled = true
      assert(desc == "Error number 1")
    end
    } )
assert(errorcalled)

-- TESTING ERROR RECOVERY

labs = {errName = "Error number 1",errName2 = "Error number 2"}

rule =  [[ 
rule <- As / %{errName}
As <- 'a'* / %{errName2}
errName <- 'b'*
]] 
res1 = tst_ast(" a a a", rule, {labels=labs} )
res2 = tst_ast("b b b ", rule, {labels=labs} )
assert(res1 and res2)

labs = {errName = "Error number 1",errName2 = "Error number 2"} 

rule = [[ 
rule <- As^errName
As <- 'a'* / %{errName2}
errName2 <- 'b'*
]] 
res1 = tst_ast(" a a a", rule, {labels=labs} )
res2 = tst_ast("b b b ", rule, {labels=labs} )
assert(res1 and res2)

-- TESTING ERROR GENERATION

rule =[[
rule <- A B C
A <- 'a'
B <- 'b'
C <- 'c'
]]
res, errs = tst_ast("ab", rule, {generrors=true})

assert(errs[1]["msg"] == "Expected C")

-- TESTING trace
rule =   [[
rule <- A B
A	<- 'a'
B	<- 'b'
]]
str = "a     b"
print( "trace :", str )
local c,e = pg.parse(str, rule, {trace = true})

rule =[[
rule <- A B C
A <- 'a'
B <- 'b'
C <- 'c'
]]
print( "trace :", str )
res, errs = pg.parse("ab", rule, {generrors=true, trace = true})


-- TESTING RECOVERY GENERATION.


local function tableMerge(t1, t2)
    for k,v in pairs(t2) do
        if type(v) == "table" then
            if type(t1[k] or false) == "table" then
                tableMerge(t1[k] or {}, t2[k] or {})
            else
                t1[k] = v
            end
        else
            t1[k] = v
        end
    end
    return t1
end

rule =  [[
rule <- A B
A	<- 'a'
B	<- 'b'
]]


-- SELF-DESCRIPTION  
--local res1, errs = tst_ast(peg.gram, peg.gram, {nocaptures=true, labels=peg.errinfo, definitions = {foldtable = peg.foldtable, concat = peg.concat}, not_exact_grammar = true})

local options ={nocaptures=true, labels=peg.errinfo, definitions = {foldtable = peg.foldtable, concat = peg.concat}} 

local ast = peg.pegToAST(peg.gram, {foldtable = peg.foldtable, concat = peg.concat})

print( "=====================")
print( "peg.gram:\n")
print( peg.gram)
print( "=====================")

print( "=====================")
print("ast:\n")
print(serpent.block(ast))
print( "=====================")

local gram3 = pg.astToPEG(ast, {recovery=false, skipspaces=false, nocaptures=true, re_useext=true})

print( "=====================")
print( "gram3:\n")
print( gram3)
print( "=====================")


local optionstrace = tableMerge({trace=true}, options)
local c1,e1 = pg.parse(rule, peg.gram, options)


print( "====================")
print( "trace :", rule )
print( "====================")
local c1t,e1t = pg.parse(rule, peg.gram, optionstrace)

local c3,e3 = pg.parse(rule, gram3, options)
print( "====================")
print( "trace :", rule )
print( "====================")
local c3t,e3t = pg.parse(rule, gram3, optionstrace)

assert(equals(c1, c3)) -- same captures
assert(equals(e1, e3)) -- same error

assert(res1) -- parse succesful 

print("all tests succesful")