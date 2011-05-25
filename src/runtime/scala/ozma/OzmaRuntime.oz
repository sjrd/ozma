functor

import
   `functor:java.lang.String`('type:java.lang.String':`type:java.lang.String`
                              'class:java.lang.String':`class:java.lang.String`) at 'x-ozma://root/java/lang/String.ozf'
   `functor:java.lang.Number`('class:java.lang.Number':`class:java.lang.Number`) at 'x-ozma://root/java/lang/Number.ozf'
   `functor:java.lang.Character`('class:java.lang.Character':`class:java.lang.Character`) at 'x-ozma://root/java/lang/Character.ozf'
   `functor:java.lang.ClassCastException`('type:java.lang.ClassCastException':`type:java.lang.ClassCastException`
                                          'class:java.lang.ClassCastException':`class:java.lang.ClassCastException`) at 'x-ozma://root/java/lang/ClassCastException.ozf'

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
   'AnyEqEq':AnyEqEq
   'AnyRefEqEq':AnyRefEqEq
   'NewActiveObject':NewActiveObject

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
         raise
            {NewObject `type:java.lang.ClassCastException`
             `class:java.lang.ClassCastException`
             '<init>'(_)}
         end
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

   fun {AnyEqEq Left Right}
      if Left == Right then
         true
      elseif Left == null then
         false
      elseif {IsInstance Left `class:java.lang.Number`} then
         {Left 'scala_$eq$eq#-1875011758'(Right $)}
      elseif {IsInstance Left `class:java.lang.Character`} then
         {Left 'scala_$eq$eq#-1875011758'(Right $)}
      else
         {Left 'equals#-1875011758'(Right $)}
      end
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

end
