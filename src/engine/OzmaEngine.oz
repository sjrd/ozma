% Ozma engine - main program
% @author Sébastien Doeraene
% @version 1.0
functor

import
   System
   Module
   Application

prepare

   OptSpecs = record(
                 %help(char:[&h &?] type:bool default:false)
                 classpath(single char:"p" type:string default:"."))

   Usage =
   'Usage:\n'#
   '  ozma -h\n'#
   '  ozma [--classpath PATH] OBJECT\n'

define

   ClassPath

   proc {ShowUsage ExitCode}
      {System.printInfo Usage}
      {Application.exit ExitCode}
   end

   fun {ClassNameToFileName ClassName}
      Ext = ".ozf"
   in
      case ClassName
      of nil then Ext
      [] &.|nil then Ext
      [] &.|&$|nil then Ext
      [] &$|_ then Ext
      [] &.|Tail then &/|{ClassNameToFileName Tail}
      [] H|Tail then H|{ClassNameToFileName Tail}
      end
   end

   proc {RunMainObject MainObject Args}
      FileName = {ClassNameToFileName MainObject}
      URL = ClassPath#'/'#FileName
      StringURL = ClassPath#'/java/lang/String.ozf'
      RuntimeURL = ClassPath#'/scala/ozma/OzmaRuntime.ozf'
      [Mod StringMod RuntimeMod] = {Module.link [URL StringURL RuntimeURL]}
      ObjID = {VirtualString.toAtom 'module:'#MainObject#'$'}
      Obj OzmaArgs
   in
      try
         StringClass = StringMod.'class:java.lang.String'
         StringLiteral = RuntimeMod.'StringLiteral'
      in
         OzmaArgs = {MakeOzmaArgs Args StringClass StringLiteral}
         Obj = Mod.ObjID
         {Obj 'main#1806194953'(OzmaArgs _)}
      catch E andthen {IsObject E} then
         {DumpException E}
      end
   end

   proc {MakeOzmaArgs Args StringClass StringLiteral Result}
      Result = {StringClass newArrayOfThisClass({Length Args} $)}

      {List.forAllInd Args
       proc {$ Index RawString}
          ArrIndex = Index-1
          String = {StringLiteral RawString}
       in
          {Result put(ArrIndex String)}
       end}
   end

   proc {DumpException Exception}
      {System.showError 'Application terminated with an exception:'}
      {System.showError {{Exception toString($)} toRawVS($)}}
   end

   try
      % Parse command-line arguments
      Args = {Application.getArgs OptSpecs}
   in
      %if Args.help then
      %   {ShowUsage 0}
      %end

      % Fill global variables from command-line arguments
      ClassPath = Args.classpath

      % Run the program
      case Args.1 of MainObject|AppArgs then
         {RunMainObject MainObject AppArgs}
      else
         raise
            error(ap(usage 'Main object required.')
                  debug:unit)
         end
      end

      {Application.exit 0}

   catch error(ap(usage Message) debug:_) then
      {System.showError Message}
      {ShowUsage 1}
   end

end
