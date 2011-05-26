% Ozma engine - main program
% @author Sébastien Doeraene
% @version 1.0
functor

import
   System
   Module
   Application
   Resolve

prepare

   OptSpecs = record(
                 %help(char:[&h &?] type:bool default:false)
                 classpath(single char:"p" type:string default:"./")
                 bootclasspath(single type:string))

   Usage =
   'Usage:\n'#
   '  ozma -h\n'#
   '  ozma [--classpath PATH] OBJECT\n'

define

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
      URL = 'x-ozma://root/'#FileName
      StringURL = 'x-ozma://root/java/lang/String.ozf'
      RuntimeURL = 'x-ozma://root/scala/ozma/OzmaRuntime.ozf'
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

   fun {MakeResolveHandlers ClassPath}
      case ClassPath of nil then
         nil
      else
         Path Tail
      in
         {String.token ClassPath &: Path Tail}
         {MakeResolveHandler Path}|{MakeResolveHandlers Tail}
      end
   end

   fun {MakeResolveHandler Path}
      {Resolve.handler.prefix "x-ozma://root/" Path}
   end

   try
      % Parse command-line arguments
      Args = {Application.getArgs OptSpecs}
   in
      %if Args.help then
      %   {ShowUsage 0}
      %end

      % Set up classpath
      local
         BootHandlers = {MakeResolveHandlers Args.bootclasspath}
         Handlers = {MakeResolveHandlers Args.classpath}
      in
         {Resolve.pickle.setHandlers {Append Handlers BootHandlers}}
      end

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
   [] error(Err debug:d(info:Info stack:Stack)) then
      {System.show Err}
      {System.show Info}
      for Entry in Stack do
         {System.show Entry}
      end
      {Application.exit 1}
   end

end
