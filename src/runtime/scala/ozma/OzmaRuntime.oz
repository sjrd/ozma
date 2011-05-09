functor

import
   `functor:java.lang.String`('type:java.lang.String':`type:java.lang.String`
                              'class:java.lang.String':`class:java.lang.String`) at '../../java/lang/String.ozf'
   `functor:java.lang.Number`('class:java.lang.Number':`class:java.lang.Number`) at '../../java/lang/Number.ozf'
   `functor:java.lang.Character`('class:java.lang.Character':`class:java.lang.Character`) at '../../java/lang/Character.ozf'
   `functor:java.lang.ClassCastException`('type:java.lang.ClassCastException':`type:java.lang.ClassCastException`
                                          'class:java.lang.ClassCastException':`class:java.lang.ClassCastException`) at '../../java/lang/ClassCastException.ozf'

export
   'InitObject':InitObject
   'NewObject':NewObject
   'NewArrayObject':NewArrayObject
   'IsInstance':IsInstance
   'AsInstance':AsInstance
   'ArrayClassOf':ArrayClassOf
   'MultiArrayClassOf':MultiArrayClassOf
   'StringLiteral':StringLiteral
   'AnyEqEq':AnyEqEq
   'AnyRefEqEq':AnyRefEqEq

define

   InitObject = {NewName}

   fun {NewObject Type Class Init}
      {New Type InitObject(Class Init)}
   end

   fun {NewArrayObject ComponentClass Dims Length}
      ActualComponentClass = {MultiArrayClassOf ComponentClass Dims-1}
   in
      {ActualComponentClass newArrayOfThisClass(Length $)}
   end

   fun {IsInstance Obj Class}
      {Class 'isInstance#-1875011758'(Obj $)}
   end

   fun {AsInstance Obj Class}
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

end
