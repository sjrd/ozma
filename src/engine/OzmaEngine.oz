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
      [Mod] = {Module.link [URL]}
      ObjID = {VirtualString.toAtom 'module:'#MainObject#'$'}
      Obj
   in
      try
         Obj = Mod.ObjID
         {Obj 'main#-1181422703'(Args _)}
      catch E andthen {IsObject E} then
         {DumpException E}
      end
   end

   proc {DumpException Exception}
      {System.showError 'Application terminated with an exception:'}
      {System.showError {{Exception toString($)} toRawString($)}}
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
