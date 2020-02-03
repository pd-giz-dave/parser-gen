
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
 


local pr = peg.print_r

local rule, res, res1, err1, res2
-- terminals
-- space allowed
local rule = pg.compile [[
rule <-  'a'
]]
local str = "a   a aa "
local res = pg.parse(str,rule)
assert(res)

-- space not allowed
rule = pg.compile [[
RULE <- 'a' 'b'
]]
str = "a     b"
res = pg.parse(str,rule)
assert(not res)

-- space not allowed 2
rule = pg.compile [[
rule <- 'a' 'b'
SKIP <- ''
SYNC <- ''
]]
str = "a     b"
res = pg.parse(str,rule)
assert(not res)

-- custom space
rule = pg.compile [[
rule <- 'a' 'b'
SKIP <- DOT
DOT <- '.'
]]
str = "a...b"
res = pg.parse(str,rule)
assert(res)

-- non terminals
-- space allowed
rule = pg.compile [[
rule <- A B
A	<- 'a'
B	<- 'b'
]]
str = "a     b"
res, err = pg.parse(str,rule)
assert(res)
-- no spaces allowed
rule = pg.compile [[
RULE <- A B
A	<- 'a'
B	<- 'b'
]]
str = "a     b"
res = pg.parse(str,rule)
assert(not res)

-- space in the beginning and end of string
rule = pg.compile [[
rule <- A B
A	<- 'a'
B	<- 'b'
]]
str = "  a     b  "
res = pg.parse(str,rule)
assert(res)

-- testing ranges
rule = [[ r <- {[a1b]* } ]]
str = "a1b"
res, err1 = pg.parse(str, rule, {nocaptures=true})
local res2 = re.compile(rule):match(str)
assert(equals(res, res2 )) 

res = pg.parse(str, rule,{nocaptures=false})


-- testing space in class
rule = [[ r <-{[a1b]*} ]]
str = "a 1b"
res,err1 = pg.parse(str, rule,  {skipspaces = true, nocaptures=true} )
assert(equals(res,"a")) 
local res2 = re.compile(rule):match(str)

rule = [[ r <- [a1]*  ]]
str = "a a"
local res = pg.parse(str, rule )
assert(res[1]== "a") 


-- testing quote in class
rule = [[ r <-{ [b']* }  ]]
str = "b'b"
res = pg.parse(str, rule  )
assert( res[1] =="b'b") 



-- TESTING CAPTURES

local r = pg.compile([[ 
  rule <- {| {:'a' 'b':}* |} 
				]], {nocaptures=true})
res = pg.parse("ababab", r)

assert(equals(res,{"ab","ab","ab"}))
-- space in capture

rule = pg.compile([[ rule <- {| {: 'a' :}* |} 
]], {nocaptures=true})
str = " a a a "
res = pg.parse(str,rule)

assert(equals(res,{"a","a","a"}))  

-- TESTING ERROR LABELS
local labs = {errName = "Error number 1",errName2 = "Error number 2"}

rule = pg.compile ([[ rule <- 'a' / %{errName}
					SYNC <- '' 
					]],  {labels=labs} )
local errorcalled = false
local function err(desc, line, col, sfail, recexp)
  errorcalled = true
  assert(desc == "Error number 1")
end
res,err1 = pg.parse("b",rule,{errorfunction=err})
assert(errorcalled)

-- TESTING ERROR RECOVERY

local labs = {errName = "Error number 1",errName2 = "Error number 2"}


rule = pg.compile( [[ 
rule <- As / %{errName}
As <- 'a'* / %{errName2}
errName <- 'b'*
]],   {labels=labs} )
res1 = pg.parse(" a a a",rule,  {labels=labs})
res2 = pg.parse("b b b ",rule,  {labels=labs})
assert(res1 and res2)

local labs = {errName = "Error number 1", errName2 = "Error number 2"} 

rule = pg.compile ([[ 
rule <- As ^errName
As <- 'a'* / %{errName2}
errName2 <- 'b'*
]],   {labels=labs})
res1 = pg.parse(" a a a",rule)
res2 = pg.parse("b b b ",rule)
assert(res1 and res2)

-- TESTING ERROR GENERATION

rule = pg.compile([[
rule <- A B C 
A <- 'a'
B <- 'b'
C <- 'c'

]], {generrors=true})
res1, err1 = pg.parse("ab",rule)
assert(err1[1]["msg"] == "Expected C")

-- TESTING RECOVERY GENERATION


-- SELF-DESCRIPTION

gram = pg.compile(peg.gram, {nocaptures=true, labels=peg.errinfo, definitions = peg.defs})
local res1, errs = pg.parse(peg.gram,gram)
assert(res1) -- parse succesful



print("all tests succesful")