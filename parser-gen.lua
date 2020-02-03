local function req(src, mod)
  mod = mod or ""
  local m = pcall(require, src) and require(src)
  or pcall(require, mod.."."..src) and  require( mod.."."..src)  -- luapower sub module path
  or nil

  assert(m, 'parser-gen depend on "'..src..'" from "'..mod..'". Please check your path')
  return m
end

local m = req "lpeglabel"
local peg = require "peg-parser"
local eg = req("errorgen", "peg-parser")
local s = req("stack", "peg-parser")
local ast = require "parser-gen.ast"
local isfinal,isaction,isrule,isgrammar,istoken,iscapture,finalNode = ast.isfinal,ast.isaction,ast.isrule,ast.isgrammar,ast.istoken,ast.iscapture,ast.finalNode

local pegdebugtrace = require('pegdebug').trace
local pp = require('pp')

-- create stack for tokens inside captures. nil - not inside capture, 0 - inside capture, 1 - token found inside capture
local tokenstack = s.Stack:Create()

local function warn(...)
  print('Warning: ', ...)
end

--local subject, errors, errorfunc
local opts = {} --  options
local errors = {}

local function resetOptions()
  opts = {}
  opts.generrors = false
  opts.nocaptures = false
  opts.re_useext = false
  opts.re_sortrules = true
  opts.trace = false
  opts.traceoptions = {
    ['-'] = {SKIP=false},
    ['+'] = {SKIP=false}
  }
  opts.skipspaces = true
  opts.usenodes = false
  opts.recovery = true 
  opts.labels = {}
  opts.definitions = {}
  opts.subject = ""
  opts.subjectlines = nil
  opts.filename = ""
  opts.errorfunction = nil
  opts.specialrules = {}  -- will contain SKIP and SYNC rules
  return opts
end


-- check values and fill default to the opts 
local function mergeOptions(options)

  if options ~= nil then  
    for k, v in pairs(opts) do
      if options[k] ~= nil then opts[k] = options[k] end
    end 
    -- special case for errorfunction
    if options.errorfunction ~= nil then opts.errorfunction = options.errorfunction end

    -- check illegal options
    for k, v in pairs(options) do
      if opts[k] == nil then
        warn('options "' ..k.. '" ignored') end
      end  
    end

    -- ensure errorfunction is a function
    if type(opts.errorfunction) ~= 'function' then 
      opts.errorfunction = nil
    end
  end


-- Lua 5.1 compatibility:
  local unpack = unpack or table.unpack


  local Predef = { nl = m.P"\n", cr = m.P"\r", tab = m.P"\t" }


  local function updatelocale()
    m.locale(Predef)
    local any = m.P(1)
    Predef.a = Predef.alpha
    Predef.c = Predef.cntrl
    Predef.d = Predef.digit
    Predef.g = Predef.graph
    Predef.l = Predef.lower
    Predef.p = Predef.punct
    Predef.s = Predef.space
    Predef.u = Predef.upper
    Predef.w = Predef.alnum
    Predef.x = Predef.xdigit
    Predef.A = any - Predef.a
    Predef.C = any - Predef.c
    Predef.D = any - Predef.d
    Predef.G = any - Predef.g
    Predef.L = any - Predef.l
    Predef.P = any - Predef.p
    Predef.S = any - Predef.s
    Predef.U = any - Predef.u
    Predef.W = any - Predef.w
    Predef.X = any - Predef.x
  end

  updatelocale()



  local function defaultsync(patt)
    return (m.P(1)^-1) * (-patt * m.P(1))^0
  end




  local function sync (patt)
    return patt --(-patt * m.P(1))^0 * patt^0 -- skip until we find the pattern and consume it(if we do)
  end


  local function pattspaces (patt)
    if opts.skipspaces then
      return patt * opts.specialrules.SKIP ^0
    else
      return patt
    end
  end

  local function token (patt)
    local incapture = tokenstack:pop() -- returns nil if not in capture
    if not incapture then
      return pattspaces(patt)
    end
    tokenstack:push(1)
    return patt
  end

-- functions used by the tool
  local function iscompiled (gr)
    return m.type(gr) == "pattern"
  end

  local bg = {} -- local variable to keep global function buildgrammar


  local function addspaces (caps)
    local hastoken = tokenstack:pop()
    if hastoken == 1 then
      return pattspaces(caps)
    end
    return caps
  end

  local function applyaction(action, op1, op2, tokenrule)
    if action == "or" then
      return op1 + op2
    elseif action == "and" then
      return op1 * op2
    elseif action == "&" then
      return #op1
    elseif action == "!" then
      return -op1
    elseif action == "+" then
      return op1^1
    elseif action == "*" then
      return op1^0
    elseif action == "?" then
      return op1^-1
    elseif action == "^" then
      return op1^op2
    elseif action == "^LABEL" then
      return op1 + m.T(op2)
    elseif action == "->" then
      return op1 / op2
      -- in captures we add SPACES^0
    elseif action == "=>" then
      return addspaces(m.Cmt(op1,op2))
    elseif action == "tcap" then
      return m.Ct(op1) -- nospaces
    elseif action == "gcap" then
      return addspaces(m.Cg(op1, op2))
    elseif action == "bref" then
      return m.Cb(op1) --m.Cmt(m.Cb(op1), equalcap) -- do we need to add spaces to bcap?
    elseif action == "poscap" then
      return addspaces(m.Cp())
    elseif action == "subcap" then
      return addspaces(m.Cs(op1))
    elseif action == "scap" then
      return addspaces(m.C(op1)) 
    elseif action == "anychar" then
      if not opts.nocaptures and not tokenrule then
        return m.C(m.P(1))
      end
      return m.P(1)
    elseif action == "label" then
      return m.T(op1) -- lpeglabel
    elseif action == "%" then
      if opts.definitions[op1] then
        return opts.definitions[op1]
      elseif Predef[op1] then
        return Predef[op1]
      else
        error("Definition for '%"..op1.."' unspecified(use options.definitions parameter)")
      end
    elseif action == "class" then 
      local res = op1
      if not tokenrule then
        if not opts.nocaptures then
          res = m.C(res)
        end
        res = token(res)
      end
      return res
    elseif action == "invert" then
      return m.P(1) - op1
    elseif action == "range" then
      return m.R(op1)
    elseif action == "classset" then
      return op1 + op2
    else
      error("Unsupported action '"..action.."'")
    end
  end

  local function applyfinal(action, term, tokenterm, tokenrule)

    if action == "t" then
      local res = m.P(term)
      if not tokenrule then
        if not opts.nocaptures  then
          res = m.C(res)
        end
        if opts.skipspaces then
          res = token(res)
        end
      end
      return res
    elseif action == "tchar" then
      return m.P(term)
    elseif action == "nt" then
      if opts.skipspaces and tokenterm and (not tokenrule) then
        return token(m.V(term))
      else
        return m.V(term)
      end
    elseif action == "func" then
      if opts.definitions[term] then
        if opts.trace then
          local ftrace=function(...)
            print('in function: '..term..'('..pp.format(...)..')')
            return opts.definitions[term](...)
          end
          return ftrace
        else        
          return opts.definitions[term]
        end
      else
        error("Definition for function '"..term.."' unspecified (use options.definitions parameter)")
      end
    elseif action == "name" then -- simple name
      return term
    elseif action == "s" then -- simple coted string
      return term
    elseif action == "num" then -- numbered string
      return tonumber(term)
    end
  end


  local function applygrammar(gram, opts)
    if opts.trace then 
      return m.P(pegdebugtrace(gram, opts.traceoptions))
    else 
      return m.P(gram)
    end
  end

  local function traverse (ast, tokenrule)
    if not ast then
      return nil 
    end

    if isfinal(ast) then
      local typefn, fn, tok = finalNode(ast)
      return applyfinal(typefn, fn, tok, tokenrule)

    elseif isaction(ast) then

      local act, op1, op2, ret1, ret2
      act = ast["action"]
      op1 = ast["op1"]
      op2 = ast["op2"]

      -- post-order traversal
      if iscapture(act) then
        tokenstack:push(0) -- not found any tokens yet
      end

      ret1 = traverse(op1, tokenrule)
      ret2 = traverse(op2, tokenrule)


      return applyaction(act, ret1, ret2, tokenrule)

    elseif isgrammar(ast) then
      --
      local g = bg.buildgrammar (ast)
      return applygrammar(g, opts)

    else
      peg.print_r(ast)
      error("Unsupported AST")	
    end

  end

  local function specialrules(ast, builder)
    -- initialize values
    opts.specialrules.SKIP = (Predef.space + Predef.nl)
    opts.skipspaces = true
    opts.specialrules.SYNC = nil
    opts.recovery = true
    -- find SPACE and SYNC rules
    for i, v in ipairs(ast) do
      local name = v["rulename"]
      local rule
      if name == "SKIP" then
        rule = traverse(v["rule"], true)
        if v["rule"]["t"] == '' then
          opts.skipspaces = false
        else
          opts.skipspaces = true
          opts.specialrules.SKIP = rule
        end
        builder[name] = rule
      elseif name == "SYNC" then
        rule = traverse(v["rule"], true)
        if v["rule"]["t"] == '' then-- SYNC <- ''
          opts.recovery=false
        else
          opts.recovery= true
          opts.specialrules.SYNC = rule
        end
        builder[name] = rule
      end
    end
    if not opts.specialrules.SYNC and opts.recovery then
      opts.specialrules.SYNC = defaultsync(opts.specialrules.SKIP)
    end
  end

  local function recorderror(position, label)  
    -- call error function here
    local line, col = peg.calcline( opts.subject, position)
    local desc
    if label == 0 then
      desc = "Syntax error"
    else
      desc = opts.labels[label]
    end
    if not opts["subjectlines"] then opts.subjectlines = peg.splitlines(opts.subject) end

    local fline =  opts.subjectlines[line]
    if opts.errorfunction then   
      opts.errorfunction(desc, line, col, fline, label, opts.filename)
    end

    local err = { line = line, col = col, label=label, msg = desc, failLine = fline }
    table.insert(errors, err)

  end

  local function record(label)
    return (m.Cp() * m.Cc(label)) / recorderror
  end

  local function buildrecoverylabel(builder, ast)

    local synctoken = pattspaces(sync(opts.specialrules.SYNC))

    -- TODO : see if we could iterate over builder instead ? 
    for lbl, desc in pairs(opts.labels) do 
      local found = false
      for i, v in ipairs(ast) do
        local name = v["rulename"]
        local isspecial = name == "SKIP" or name == "SYNC"
        local rule = v["rule"] 

        if name == lbl then 
          found = true
        end
      end

      if not found then 
        builder[lbl] =  record(lbl) * synctoken 
      end
    end
  end


  function bg.buildgrammar (ast)
    local builder = {}

    specialrules(ast, builder)
    local initialrule
    for i, v in ipairs(ast) do
      local istokenrule = v["token"] == "1"
      local isfragment = v["fragment"] == "1"
      local isnode = v["node"] == "1"

      if isnode and not opts.usenodes then
        warn("Node mode disabled - please use options.usenodes=true before compiling the grammar")
      end

      local name = v["rulename"]
      local isspecial = name == "SKIP" or name == "SYNC"
      local rule = v["rule"]
      if i == 1 then
        initialrule = name
        table.insert(builder, name) -- lpeg syntax
      end
      if not builder[name] then -- dont traverse rules for SKIP and SYNC twice
        builder[name] =traverse( rule, istokenrule) -- TODO : protect the traverse call to get the name of the offending rule when something goes wrong
      end
      if not opts.nocaptures  and not isfragment and not isspecial and ((not opts.usenodes) or (opts.usenodes and isnode)) then 
        if istokenrule then
          builder[name] = m.C(builder[name])
        end
        builder[name] = m.Ct(m.Cg(m.Cc(name),"rule") * m.Cg(m.Cp(),"pos") * builder[name]) 
      end
    end

    if  opts.skipspaces then
      builder[initialrule] = opts.specialrules.SKIP^0 * builder[initialrule] -- skip spaces at the beginning of the input
    end
    if  opts.recovery then
      --   builder[initialrule] = buildrecovery(builder[initialrule]) -- build recovery on top of initial rule
      buildrecoverylabel(builder, ast)
    end
    -- TODO : add case insensitive keyword support

    return builder
  end




  local function build(ast, opts)

    if isgrammar(ast) then
      return traverse(ast)
    else -- input is not a grammar - skip spaces and sync by default
      opts.specialrules.SKIP = (Predef.space + Predef.nl)
      opts.skipspaces = true
      opts.specialrules.SYNC = defaultsync()
      opts.recovery = true
      local res =  opts.specialrules.SKIP^0 * traverse(ast)
      if not opts.nocaptures  then
        res = m.Ct(res)
      end
      return res
    end
  end


-- ast to grammar

-- TODO: rename *AstToPeg en *Re

  local function defaultsyncAstToPEG(patt)
    return '.? (!'..patt.. ' .)*' --(m.P(1)^-1) * (-patt * m.P(1))^0
  end

  local function pattspacesAstToPEG (patt)
    if opts.skipspaces then
      return '('.. patt .." SKIP*)"
    else
      return patt
    end
  end

  local function tokenAstToPEG (patt)
    local incapture = tokenstack:pop() -- returns nil if not in capture
    if not incapture then
      return pattspacesAstToPEG(patt)
    end
    tokenstack:push(1)
    return patt
  end

  local function addspacesAstToPEG (caps)
    local hastoken = tokenstack:pop()
    if hastoken == 1 then
      return pattspacesAstToPEG(caps)
    end
    return caps
  end

  local function applyActionAstToPEG(action, op1, op2,   tokenrule)
    if action == "or" then
      return '(' .. op1 ..' / '.. op2 ..')'
    elseif action == "and" then
      return op1 ..' ' .. op2
    elseif action == "&" then
      return '&'..op1
    elseif action == "!" then
      return '!'..op1
    elseif action == "+" then
      return op1..'+'
    elseif action == "*" then
      return op1..'*'
    elseif action == "?" then
      return op1..'?'
    elseif action == "^" then
      return op1..'^'..op2
    elseif action == "^LABEL" then
      --   local lab = op2
--		if not lab then
--			error("Label '"..op2.."' unspecified using setlabels()")
-- end
      return op1 .. '^'..op2 
    elseif action == "->" then
      if op1 == "''" then -- spacial case for empty capture
        return  op1 ..' -> '.. op2  
      else
        return '('.. op1 ..') -> '.. op2
      end 
      -- in captures we add SPACES^0
    elseif action == "=>" then
      -- TODO: action '=>' to check for space
      --return addspaces(m.Cmt(op1,op2))
      return op1 ..'=>'..op2
    elseif action == "tcap" then
      return '{| '..op1..' |}' -- nospaces
    elseif action == "gcap" then

      -- return addspaces(m.Cg(op1, op2))
      if op2 ~= nil then  -- named group capture
        return addspacesAstToPEG('{:'..op2..': '.. op1..' :}')
      else -- anonymous group capture
        return addspacesAstToPEG('{:'..op1..':}')
      end

    elseif action == "bref" then
      -- TODO: action 'bref' to check for space
      -- return m.Cb(op1) --m.Cmt(m.Cb(op1), equalcap) -- do we need to add spaces to bcap?
      return '='..op1  
    elseif action == "poscap" then
      --  return addspaces(m.Cp())
      return addspacesAstToPEG('{}')
    elseif action == "subcap" then
      --  return addspaces(m.Cs(op1))
      return addspacesAstToPEG('{~'..op1..'~}')
    elseif action == "scap" then
      --return addspaces(m.C(op1)) 
      return addspacesAstToPEG('{'..op1..'}') 
    elseif action == "anychar" then
      if not opts.nocaptures and not tokenrule then
        return '{.}' -- return m.C(m.P(1))
      end
      return '.'--return m.P(1)
    elseif action == "label" then
      --   local lab = op1
--		if not lab then
--			error("Label '"..op1.."' unspecified using setlabels()")
--		end
--    return m.T(op1) -- lpeglabel
      return '%{'..op1..'}'
    elseif action == "%" then
      if opts.definitions[op1] then
        return '%'..op1 --return opts.definitions[op1]
      elseif Predef[op1] then
        return '%'..op1  -- return Predef[op1]
      else
        error("Definition for '%"..op1.."' unspecified(use options.definitions parameter)")
      end
    elseif action == "class" then
      local res = '['..op1..']'
      if not tokenrule then
        if not opts.nocaptures then
          res = '{'..res..'}'  
        end
        res = tokenAstToPEG(res)
      end

      return res   
    elseif action == "invert" then  -- in char range
      return '^'..op1
    elseif action == "range" then
      return string.sub(op1,1,1)..'-'..string.sub(op1,2,2) 
    elseif action =="classset" then
      return op1 .. op2
    else
      error("Unsupported action '"..action.."'")
    end
  end

  local function applyfinalAstToPEG(action, term, tokenterm, tokenrule)

    if action == "t" then 
      local res = term
      if term:find("'") then  -- term contains single quote
        if term:find('"') then -- both style of quote: error out
          error("Unsupported terminal with both quote style ("..term..")")  
        end
        res = '"'..term..'"' -- use double quote
      else -- use single quote
        res = "'"..term.."'" -- m.P(term)  
      end 
      if not tokenrule then
        if not opts.nocaptures  then
          res = '{'..res..'}' -- m.C(res)
        end
        if opts.skipspaces then
          res = tokenAstToPEG(res)
        end
      end
      return res
    elseif action == "tchar" then
      --  if term == "'" then 
      --    return [[\']]
      -- end
      return term
    elseif action == "nt" then
      if opts.skipspaces and tokenterm and (not tokenrule) then
        return tokenAstToPEG(term)-- m.V(term))
      else
        return term -- m.V(term)
      end
    elseif action == "func" then
      return term --opts.definitions[term]
    elseif action == "name" then -- simple name
      return term
    elseif action == "s" then -- simple cotted string
      return "'"..term.."'"
    elseif action == "num" then -- numbered string
      return tonumber(term)
    end
  end

  local function traverseAstToPEG(ast, tokenrule)
    if not ast then
      return nil 
    end

    if isfinal(ast) then
      local typefn, fn, tok = finalNode(ast)
      return applyfinalAstToPEG(typefn, fn, tok, tokenrule)

    elseif isaction(ast) then

      local act, op1, op2, labs, ret1, ret2
      act = ast["action"]
      op1 = ast["op1"]
      op2 = ast["op2"] 

      -- post-order traversal
      if iscapture(act) then
        tokenstack:push(0) -- not found any tokens yet
      end

      ret1 = traverseAstToPEG(op1, tokenrule)
      ret2 = traverseAstToPEG(op2, tokenrule)


      return applyActionAstToPEG(act, ret1, ret2, tokenrule)

    elseif isgrammar(ast) then
      --
      local g =  bg.grammarAstToPEG (ast)
      return  g

    else
      peg.print_r(ast)
      error("Unsupported AST")	
    end
  end

  local function specialrulesAstToPEG(ast, builder)
    -- initialize values
    opts.specialrules.SKIP_re = '(%s / %nl)' --(Predef.space + Predef.nl)
--  opts.skipspaces = true
    opts.specialrules.SYNC_re = nil
    --  opts.recovery = true 

    -- find SPACE and SYNC rules
    for i, v in ipairs(ast) do
      local name = v["rulename"]
      local rule
      if name == "SKIP" then
        rule = traverseAstToPEG(v["rule"], true)
        if v["rule"]["t"] == '' then
          opts.skipspaces = false
        else
          opts.skipspaces = true
          opts.specialrules.SKIP_re = rule
        end
        builder[name] = rule
      elseif name == "SYNC" then
        rule = traverseAstToPEG(v["rule"], true)
        if v["rule"]["t"] == '' then-- SYNC <- ''
          opts.recovery=false
          opts.specialrules.SYNC_re = "''"
        else
          opts.recovery= true
          opts.specialrules.SYNC_re = rule
        end
        builder[name] = rule
      end
    end
    if not opts.specialrules.SYNC_re and opts.recovery then
      opts.specialrules.SYNC_re = defaultsyncAstToPEG("SKIP")
    end

    if opts.skipspaces and not  builder["SKIP"] then 
      builder["SKIP"] = opts.specialrules.SKIP_re
    end
    if opts.recovery and not  builder["SYNC"] then 
      builder["SYNC"] = opts.specialrules.SYNC_re
    end


  end  

  local function recordAstToPEG(label)
    --return (m.Cp() * m.Cc(label)) / recorderror
    return "({} {'' -> '"..label.."'}) -> recorderror"
  end

  local function buildrecoverylabelAstToPEG(builder, ast)
    local synctoken = pattspacesAstToPEG('SYNC')

    -- TODO : see if we could iterate over builder instead ? 
    for lbl, desc in pairs(opts.labels) do 
      local found = false
      for i, v in ipairs(ast) do
        local name = v["rulename"]
        local isspecial = name == "SKIP" or name == "SYNC"
        local rule = v["rule"] 

        if name == lbl then 
          found = true
        end
      end

      if not found then 
        builder[lbl] =  recordAstToPEG(lbl) ..' '.. synctoken 
      end
    end 
  end

  function bg.grammarAstToPEG(ast)
    local builder = {}
    local fragments = {}
    local nodes = {}

    specialrulesAstToPEG(ast, builder)
    local initialrule
    for i, v in ipairs(ast) do
      local istokenrule = v["token"] == "1"
      local isfragment = v["fragment"] == "1"
      local isnode = v["node"] == "1"

      if isnode and not opts.usenodes then
        warn("Node mode disabled - please use options.usenodes = true before compiling the grammar")
      end


      local name = v["rulename"]
      local isspecial = name == "SKIP" or name == "SYNC"
      local rule = v["rule"]
      if isfragment then fragments[name] = true end
      if isnode then nodes[name] = true end
      if i == 1 then
        initialrule = name
        table.insert(builder, name) -- lpeg syntax
      end
      if not builder[name] then -- dont traverse rules twice
        builder[name] = traverseAstToPEG(rule, istokenrule)
      end
      if not opts.nocaptures  and not isfragment and not isspecial and ((not opts.usenodes) or (opts.usenodes and isnode)) then 
        if istokenrule then
          -- builder[name] = m.C(builder[name])
          builder[name] = '{'..builder[name]..'}' 
        end
        --builder[name] = m.Ct(m.Cg(m.Cc(name),"rule") * m.Cg(m.Cp(),"pos") * builder[name]) 
        builder[name] = "{| {:rule: '' -> '"..name.."' :} {:pos: {} :} " ..builder[name] .." |}"

        if istokenrule then
          builder[name] = builder[name] .. ' -- token'
        end 
        if isnode then
          builder[name] = builder[name] .. ' -- node'
        end
      end
    end 

    if opts.skipspaces then
      -- builder[initialrule] = opts.SKIP^0 * builder[initialrule] -- skip spaces at the beginning of the input
      builder[initialrule] =  'SKIP* '.. builder[initialrule] -- skip spaces at the beginning of the input
    end
    if opts.recovery then
      opts.definitions.recorderror = recorderror -- add recording function in definitions

      -- builder[initialrule] = buildrecoveryAstToPEG(builder[initialrule]) -- build recovery on top of initial rule
      -- assert(false)
      buildrecoverylabelAstToPEG(builder, ast)
    end

-- TODO : add case insensitive keyword support
    local regrammar = {}
    local done = {}
    local rules = {}
    for k, v in pairs(builder) do
      if type(k) == 'string' then table.insert(rules, k) end
    end
    if opts.re_sortrules then table.sort(rules) end

    table.insert(regrammar, initialrule .. ' <- ' .. builder[initialrule]) -- initial rule
    done[initialrule] = true

    for i, k  in ipairs(rules) do
      if type(k) == 'string' then 
        if not done[k] then 
          local rule = k
          if opts.re_useext and fragments[k] then rule = 'fragment ' ..k end 
          if opts.re_useext and nodes[k] then rule = 'node ' ..k end 
          table.insert(regrammar, rule ..' <- '..builder[k]) 
          done[k] = true
        end
      end
    end

    return table.concat(regrammar, '\n' )
  end

  local function astToPEG(ast, options)
    resetOptions()
    mergeOptions(options)

    if isgrammar(ast) then
      return traverseAstToPEG(ast), opts
    end
  end


-- end ast to grammar

-- recovery grammar




-- end 




-- Compile the grammar to a LPEG objet.
-- 
-- @param p : a grammar to compile
-- @param  pre-defined sets and functions
-- @param options table may be nil, default are
--        {generrors = false, nocaptures = false}
-- available fields are 
--      nocaptures = false -- do not put capture around terminals
--      skipspaces = true  -- add space skiping in rules
--      re_useext = false -- output grammar with parser-gen extentions (fragment, node)
--      re_sortrules = true -- output grammar with rules sorted 
--      recovery = true    -- add automatic recovery when applicable
--      usenodes = false   -- node mode to generate AST on node rules only
--      labels = {err1="Error message1"}        -- nice error messages for your labels (or generated recovery )   
--      definitions = {}   -- additional functions for the grammar
--      errorfunction = function(desc, line, col, fline, label, filename) ... end  -- function called when a label is thrown
--      subject = ""       -- _internal_ the input string that will be parsed and allow for better errors messages. Will be overvitten by parse()
-- @return the compiled grammar as a LPEG userdata
  local function compile(p, options)
    if iscompiled(p) then return p end
    resetOptions()
    mergeOptions(options)

    --re.setlabels(tlabels)
    --re.compile(input,defs)
    -- build ast
    local ast = peg.pegToAST(p, opts.defs)
    if opts.generrors then
      local follow = eg.follow(ast)
      local errors = eg.adderrors(ast, follow)

--    setlabels (errors, true) -- add errors generated by errorgen
      for k,v in pairs(errors) do
        opts.labels[k] = v  
        print ("generate error "..k..': '..v)
      end
    end
    local gram = build(ast, opts)
    if not gram then
      -- TODO : find error using relabel module
    end
    return gram
  end

  local function genErrorRecovery(ast, options)
    mergeOptions(options)
    if not ast then   return nil, opts   end
    if not isgrammar(ast) then      return nil, opts end
    
    if opts.generrors then
      local follow = eg.follow(ast)
      local errors = eg.adderrors(ast, follow)

      for k,v in pairs(errors) do
        opts.labels[k] = v  
        print ("ast generate error "..k..': '..v)
      end
    end
    return ast, opts
  end


--- Compile grammar and parse input in one go.
-- Compile the grammar
-- @param input the string to parse
-- @param grammar the relabel style grammar to use
-- @param options see compile() 
-- @return (res, errors) a table containing result of match and errors 
-- or nil if everything goes as planned 
  local function parse (input, grammar, options)

    if not iscompiled(grammar) then
      local cp = compile(grammar, options)
      grammar = cp
    else
      mergeOptions(options)
    end
    opts.subject = input  
    opts.subjectlines = nil -- reset lines

-- set up recovery table
    errors = {}
    -- end
    local r, e, sfail = m.match(grammar,input)
    if not r then
      recorderror(#input - sfail, e)
    end
    if #errors == 0 then errors=nil end
    return r, errors
  end


  local pg = {
    compile=compile,   
    parse=parse,
--  follow=follow, 
    calcline = peg.calcline,  
    astToPEG=astToPEG, 
    genErrorRecovery=genErrorRecovery
  }
  return pg
