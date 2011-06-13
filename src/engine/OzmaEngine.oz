% Ozma engine - main program
% @author Sébastien Doeraene
% @version 1.0
functor

import
   System
   Module
   Application
   Resolve
   Property

prepare

   OptSpecs = record(mode:start
                 help(single char:[&h &?] type:bool default:false)
                 version(single char:[&v] type:bool default:false)
                 systempath(single type:string)
                 classpath(single char:"p" type:string default:"./")
                 bootclasspath(single type:string))

   VersionString = 'Ozma code runner version 0.1 -- Copyright 2011, S. Doeraene'

   Usage =
   'Usage:\n'#
   '  ozma -h\n'#
   '  ozma [--classpath PATH] OBJECT [ARGS]\n'

   HelpText = VersionString#'\n\n'#Usage

define

   proc {ShowUsage ExitCode}
      {System.printInfo Usage}
      {Application.exit ExitCode}
   end

   proc {ShowHelp ExitCode}
      {System.printInfo HelpText}
      {Application.exit ExitCode}
   end

   proc {ShowVersion ExitCode}
      {System.showInfo VersionString}
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
      RuntimeURL = 'x-ozma://system/OzmaRuntime.ozf'
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
         {Obj 'main#-1565094369'(OzmaArgs _)}
      catch error(throwable(E) debug:D) then
         {DumpException E D}
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

   proc {DumpException Exception Debug}
      {System.showError 'Application terminated with an exception:'}
      {System.showError {{Exception toString($)} toRawVS($)}}
      {System.showError ''}

      for StackItem in Debug.stack do
         {System.show StackItem}
      end
   end

   fun {MakeResolveHandlers Prefix ClassPath}
      case ClassPath of nil then
         nil
      else
         Path Tail
      in
         {String.token ClassPath &: Path Tail}
         {MakeResolveHandler Prefix Path}|{MakeResolveHandlers Prefix Tail}
      end
   end

   fun {MakeResolveHandler Prefix Path}
      {Resolve.handler.prefix Prefix Path}
   end

   try
      % Parse command-line arguments
      Args = {Application.getArgs OptSpecs}
   in
      if Args.help then
         {ShowHelp 0}
      elseif Args.version then
         {ShowVersion 0}
      end

      % Set up classpath
      local
         Sys = "x-ozma://system/"
         Root = "x-ozma://root/"
         SystemHandlers = {MakeResolveHandlers Sys Args.systempath}
         BootHandlers = {MakeResolveHandlers Root Args.bootclasspath}
         Handlers = {MakeResolveHandlers Root Args.classpath}
         AllHandlers = {Append SystemHandlers
                        {Append Handlers BootHandlers}}
      in
         {Resolve.pickle.setHandlers AllHandlers}
      end

      % Set up properties
      {Property.put 'errors.depth' 10}
      {Property.put 'print.depth' 10}

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
