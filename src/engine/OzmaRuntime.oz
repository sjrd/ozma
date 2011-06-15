functor

import
   `functor:java.lang.String`('type:java.lang.String':`type:java.lang.String`
                              'class:java.lang.String':`class:java.lang.String`) at 'x-ozma://root/java/lang/String.ozf'
   `functor:java.lang.ClassCastException`('type:java.lang.ClassCastException':`type:java.lang.ClassCastException`
                                          'class:java.lang.ClassCastException':`class:java.lang.ClassCastException`) at 'x-ozma://root/java/lang/ClassCastException.ozf'
   `functor:scala.Tuple2`('type:scala.Tuple2':`type:scala.Tuple2`
                          'class:scala.Tuple2':`class:scala.Tuple2`) at 'x-ozma://root/scala/Tuple2.ozf'
   `functor:scala.collection.immutable.$colon$colon`('type:scala.collection.immutable.$colon$colon':`type:scala.collection.immutable.$colon$colon`
                                                     'class:scala.collection.immutable.$colon$colon':`class:scala.collection.immutable.$colon$colon`) at 'x-ozma://root/scala/collection/immutable.ozf'
   `functor:scala.runtime.BoxesRunTime`('module:scala.runtime.BoxesRunTime$':`module:scala.runtime.BoxesRunTime$`) at 'x-ozma://root/scala/runtime/BoxesRunTime.ozf'
   `functor:ozma.Port`('type:ozma.Port':`type:ozma.Port`
                       'class:ozma.Port':`class:ozma.Port`) at 'x-ozma://root/ozma/Port.ozf'
   `functor:ozma.ResultPort`('type:ozma.ResultPort':`type:ozma.ResultPort`
                             'class:ozma.ResultPort':`class:ozma.ResultPort`) at 'x-ozma://root/ozma/ResultPort.ozf'
   `functor:ozma.ResultPort.Item`('type:ozma.ResultPort.Item':`type:ozma.ResultPort.Item`
                                  'class:ozma.ResultPort.Item':`class:ozma.ResultPort.Item`) at 'x-ozma://root/ozma/ResultPort/Item.ozf'

export
   'InitObject':InitObject
   'NewObject':NewObject
   'NewArrayObject':NewArrayObject
   'ArrayValue':ArrayValue
   'IsInstance':IsInstance
   'AsInstance':AsInstance
   'ArrayClassOf':ArrayClassOf
   'MultiArrayClassOf':MultiArrayClassOf
   'StringLiteral':StringLiteral
   'NewOzmaPort':NewOzmaPort
   'NewOzmaResultPort':NewOzmaResultPort
   'AnyEqEq':AnyEqEq
   'AnyRefEqEq':AnyRefEqEq
   'NewActiveObject':NewActiveObject
   'ModuleAccessor':ModuleAccessor
   'Throw':Throw
   'BinNot':BinNot
   'BinOr':BinOr
   'BinXor':BinXor
   'BinAnd':BinAnd
   'LSL':BinLSL
   'LSR':BinLSR
   'ASR':BinASR

define

   InitObject = {NewName}

   fun {NewObject Type Class Init}
      {New Type InitObject(Class Init)}
   end

   proc {NewArrayObject ComponentClass Dims Lengths Result}
      ActualComponentClass = {MultiArrayClassOf ComponentClass Dims-1}
   in
      case Lengths of Length|LenTail then
         Result = {ActualComponentClass newArrayOfThisClass(Length $)}
         {InitializeSubArrays Result ActualComponentClass LenTail}
      end
   end

   proc {InitializeSubArrays Array ElementClass Lengths}
      case Lengths of Length|LenTail then
         ComponentClass = {ElementClass componentClass($)}
      in
         for Index in 0..({Array length($)}-1) do
            SubArray = {ComponentClass newArrayOfThisClass(Length $)}
         in
            {Array put(Index SubArray)}
            {InitializeSubArrays SubArray ComponentClass LenTail}
         end
      else
         skip
      end
   end

   proc {ArrayValue ComponentClass Length Elements Result}
      RawResult
   in
      Result = {NewArrayObject ComponentClass 1 [Length]}
      RawResult = {Result toRawArray($)}
      {List.forAllInd Elements
       proc {$ Index Element}
          Idx = Index-1
       in
          RawResult.Idx := Element
       end}
   end

   fun {IsInstance Obj Class}
      {Class 'isInstance#-1875011758'(Obj $)}
   end

   fun {AsInstance Obj Class}
      case {Value.status Obj}
      of det(_) then
         {ActualAsInstance Obj Class}
      [] failed then
         Obj
      else
         local
            R
         in
            thread
               try {Value.waitQuiet Obj} catch _ then skip end
               {Value.makeNeeded R}
            end
            R = {ByNeedFuture fun {$} {ActualAsInstance Obj Class} end}
            R
         end
      end
   end

   fun {ActualAsInstance Obj Class}
      if (Obj == null) orelse {IsInstance Obj Class} then
         Obj
      else
         {Throw {NewObject `type:java.lang.ClassCastException`
                 `class:java.lang.ClassCastException`
                 '<init>#-37663348'(_)}}
         Obj
      end
   end

   fun {ArrayClassOf ComponentClass}
      {ComponentClass arrayOfThisClass($)}
   end

   fun {MultiArrayClassOf ComponentClass Dims}
      if Dims == 0 then
         ComponentClass
      else
         {MultiArrayClassOf {ArrayClassOf ComponentClass} Dims-1}
      end
   end

   fun {StringLiteral RawString}
      {NewObject `type:java.lang.String`
       `class:java.lang.String`
       '<init>'(RawString _)}
   end

   fun {NewOzmaPort}
      RawStream RawPort
      Stream Port
   in
      {NewPort RawStream RawPort}
      thread {OzmaPortHandler RawStream Stream} end
      Port = {NewObject `type:ozma.Port` `class:ozma.Port`
              '<init>#-855160462'(RawPort _)}
      {Pair !!Stream Port}
   end

   proc {OzmaPortHandler RawStream Stream}
      case RawStream of Head|Tail then
         NewStream
      in
         Stream = {Cons Head !!NewStream}
         {OzmaPortHandler Tail NewStream}
      end
   end

   fun {NewOzmaResultPort}
      RawStream RawPort
      Stream Port
   in
      {NewPort RawStream RawPort}
      thread {OzmaResultPortHandler RawStream Stream} end
      Port = {NewObject `type:ozma.ResultPort` `class:ozma.ResultPort`
              '<init>#1894474825'(RawPort _)}
      {Pair !!Stream Port}
   end

   proc {OzmaResultPortHandler RawStream Stream}
      case RawStream of (In#Out)|Tail then
         NewStream
         Item = {NewObject `type:ozma.ResultPort.Item` `class:ozma.ResultPort.Item`
                 '<init>#1317412069'(In Out _)}
      in
         Stream = {Cons Item !!NewStream}
         {OzmaResultPortHandler Tail NewStream}
      end
   end

   fun {Pair X1 X2}
      {NewObject `type:scala.Tuple2` `class:scala.Tuple2`
       '<init>#667154047'(X1 X2 _)}
   end

   fun {Cons Head Tail}
      {NewObject `type:scala.collection.immutable.$colon$colon`
       `class:scala.collection.immutable.$colon$colon`
       '<init>#835746335'(Head Tail _)}
   end

   fun {AnyEqEq Left Right}
      {{`module:scala.runtime.BoxesRunTime$`}
       'equals#521295670'(Left Right $)}
   end

   fun {AnyRefEqEq Left Right}
      if Left == null then
         Right == null
      else
         {Left 'equals#-1875011758'(Right $)}
      end
   end

   fun {NewActiveObject Obj}
      L P
   in
      {NewPort L P}
      thread
         {ForAll L proc {$ M} {Obj M} end}
      end
      proc {$ M}
         {Send P M}
      end
   end

   fun {ModuleAccessor ModuleVar Type Class Init}
      Lock = {ByNeed NewLock}
   in
      fun {$}
         lock Lock then
            if {IsFree ModuleVar} then
               ModuleVar = {NewObject Type Class Init}
            end
            ModuleVar
         end
      end
   end

   proc {Throw Throwable}
      {Exception.raiseError throwable(Throwable)}
   end

   %%%%%%%%%%%%%%%%%%%%%%
   % Bitwise operations %
   %%%%%%%%%%%%%%%%%%%%%%

   local
      BitCount = 64
      MaxBit = BitCount-1

      AllSet = {BitArray.new 0 MaxBit}

      local
         proc {InternalI2BA Bits Int Index}
            if Index < BitCount andthen Int > 0 then
               if Int mod 2 \= 0 then
                  {BitArray.set Bits Index}
               end
               {InternalI2BA Bits Int div 2 Index+1}
            end
         end
      in
         proc {IntToBitArray Int ?Bits}
            Bits = {BitArray.new 0 MaxBit}
            {InternalI2BA Bits Int 0}
         end

         fun {BitArrayToInt Bits}
            Sets = {BitArray.toList Bits}
         in
            {List.foldL Sets
             fun {$ Prev Index}
                Prev + {Pow 2 Index}
             end 0}
         end
      end
   in
      for I in 0..MaxBit do
         {BitArray.set AllSet I}
      end
      
      fun {BinNot X}
         A = {IntToBitArray X}
      in
         {BitArray.nimpl A AllSet}
         {BitArrayToInt A}
      end

      fun {BinOr X Y}
         A = {IntToBitArray X}
         B = {IntToBitArray Y}
      in
         {BitArray.disj A B}
         {BitArrayToInt A}
      end

      fun {BinXor X Y}
         A = {IntToBitArray X}
         B = {IntToBitArray Y}
      in
         {BitArray.nimpl A B}
         {BitArrayToInt A}
      end

      fun {BinAnd X Y}
         A = {IntToBitArray X}
         B = {IntToBitArray Y}
      in
         {BitArray.conj A B}
         {BitArrayToInt A}
      end

      fun {BinLSL X Y}
         X * {Pow 2 Y}
      end

      fun {BinLSR X Y}
         X div {Pow 2 Y}
      end

      fun {BinASR X Y}
         X div {Pow 2 Y}
      end
   end

end
