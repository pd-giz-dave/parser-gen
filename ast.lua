-- common abstract syntax tree (ast) functions

local function isfinal(t)
  return t["t"] or t['tchar'] or t["nt"] or t["func"] or t["s"] or t["name"] or t["num"]
end

local function isaction(t)
  return t["action"]
end



local function isrule(t)
  return t and t["rulename"]
end

local function isgrammar(t)
  if type(t) == "table" and not(t["action"]) then
    return isrule(t[1])
  end
  return false
end 

local function istoken (t)
  return t["token"] == "1"
end

local function iscapture (action)
  return action == "=>" or action == "gcap" or action == "scap" or action == "subcap" or action == "poscap"
end

local function finalNode (t)
  if t["t"] then
    return"t",t["t"] -- terminal
  elseif t["tchar"] then
    return "tchar", t["tchar"]
  elseif t["nt"] then
    return "nt", t["nt"], istoken(t) -- nonterminal
  elseif t["func"] then
    return "func", t["func"] -- function
  elseif t["name"] then
    return "name", t["name"]
  elseif t["s"] then
    return "s", t["s"]
  elseif t["num"] then
    return "num", t["num"]
  end
  return nil
end

return {
  isfinal=isfinal,
  isaction=isaction,
  isrule=isrule,
  isgrammar=isgrammar,
  istoken=istoken,
  iscapture=iscapture,
  finalNode=finalNode
}