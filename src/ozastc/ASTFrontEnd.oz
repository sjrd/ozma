% Front-end for the AST compiler
% @author sjrd

\switch +gump

\switch +gumpparserverbose

functor

export
   ASTScanner
   ASTParser

   ParseASTFile
   ParseASTVirtualString

import
   GumpScanner
   GumpParser

define

   \insert ASTScanner.ozg
   \insert ASTParser.ozg

   proc {ParseASTSource ScannerMsg Reporter TestSwitch Macros Tree}
      Scanner = {New ASTScanner init(reporter: Reporter)}
      Parser = {New ASTParser init('scanner': Scanner
                                   reporter: Reporter)}
      AST
      Status
   in
      {Scanner ScannerMsg}
      {Parser parse(program(AST) ?Status)}
      {Scanner close()}

      if Status then
	 Tree = AST
      else
	 Tree = parseError
      end
   end

   fun {ParseASTFile FileName Reporter TestSwitch Macros}
      {ParseASTSource scanFile(FileName) Reporter TestSwitch Macros}
   end

   fun {ParseASTVirtualString VS Reporter TestSwitch Macros}
      {ParseASTSource scanVirtualString(VS) Reporter TestSwitch Macros}
   end

end
