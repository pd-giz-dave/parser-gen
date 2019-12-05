package = "parser-gen"
version = "1.3-0"
source = {
   url = "git://github.com/chowette/parser-gen",
   tag = "v1.3"
}
description = {
   summary = "A PEG parser generator that handles space characters, generates ASTs and adds error labels automatically.",
   homepage = "https://github.com/chowette/parser-gen",
   license = "MIT/X11"
}
dependencies = {
	 "lua >= 5.1, < 5.4",
	 "lpeglabel >= 1.6.0"
}
build = {
   type = "builtin",
   modules = {
		["parser-gen"] = "parser-gen.lua",
		["peg-parser"] = "peg-parser.lua",
		["stack"] = "stack.lua",
		["equals"] = "equals.lua",
		["errorgen"] = "errorgen.lua",
		

   }
}