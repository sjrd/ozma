% Context:
%   BatchCompiler: the Compiler object
%   ReadFile: FileName => FileContents

local
   fun {ParseASTVirtualString VS Narrator TestSwitch Macros}
      [{Compiler.evalExpression VS env() _}]
   end

   fun {ParseASTFile FileName Narrator TestSwitch Macros}
      {ParseASTVirtualString {ReadFile FileName} Narrator TestSwitch Macros}
   end
in
   {BatchCompiler enqueue(setFrontEnd(ParseASTFile ParseASTVirtualString))}
end
