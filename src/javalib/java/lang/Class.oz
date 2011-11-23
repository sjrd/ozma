functor

import
   OzmaRuntime('NewObject':NewObject
               'NewArrayObject':MultiNewArrayObject
               'StringLiteral':StringLiteral) at 'x-ozma://system/OzmaRuntime.ozf'
   `functor:java.lang.Object`('type:java.lang.Object':Object
                              'class:java.lang.Object':ObjectClass) at 'x-ozma://root/java/lang/Object.ozf'
   `functor:java.lang.Array`(makeNewArrayClass:MakeNewArrayClass
                             newArrayObject:NewArrayObject) at 'x-ozma://root/java/lang/Array.ozf'

export
   'type:java.lang.Class':Class
   'class:java.lang.Class':ClassClass
   'type:java.lang.Class$PrimitiveClass':`type:java.lang.Class$PrimitiveClass`

define

   class Class from Object
      attr
         name
         superClass
         mixins
         ancestors
         arrayOfThisClass

      meth '<init>'(ARawName ASuperClass AMixins AAncestors $)
         Object, '<init>():java.lang.Object'(_)
         name := {StringLiteral ARawName}
         superClass := ASuperClass
         mixins := AMixins
         ancestors := AAncestors
         arrayOfThisClass := {ByNeed fun {$}
                                        {MakeNewArrayClass self}
                                     end}
         unit
      end

      meth 'getName():java.lang.String'($)
         @name
      end

      % For convenience in other Oz functors
      meth getName($)
         @name
      end

      meth 'toString():java.lang.String'($)
         @name
      end

      meth 'isInstance(java.lang.Object):scala.Boolean'(Obj $)
         if Obj == null then
            false
         else
            {self 'isAssignableFrom(java.lang.Class):scala.Boolean'({Obj getClass($)} $)}
         end
      end

      meth 'isAssignableFrom(java.lang.Class):scala.Boolean'(SubClass $)
         {SubClass isAncestor(self $)}
      end

      meth isAncestor(Cls $)
         (Cls == self) orelse {Member Cls @ancestors}
      end

      meth 'isPrimitive():scala.Boolean'($)
         false
      end

      meth 'isArray():scala.Boolean'($)
         false
      end

      meth arrayOfThisClass($)
         @arrayOfThisClass
      end

      meth newArrayOfThisClass(Length $)
         InitValue = {self zeroOfThisClass($)}
      in
         {NewArrayObject @arrayOfThisClass Length InitValue}
      end

      meth multiNewArrayOfThisClass(Dimensions $)
         Rec = {Array.toRecord array {Dimensions toRawArray($)}}
         Dims = {Record.width Rec}
         Lengths = {Record.toList Rec}
      in
         {MultiNewArrayObject @arrayOfThisClass Dims Lengths}
      end

      meth arrayEncodedName($)
         'L'#{(@name) toRawVS($)}#';'
      end

      meth zeroOfThisClass($)
         null
      end
   end

   ClassClass = {ByNeed fun {$}
                           {NewObject Class ClassClass
                            '<init>'("java.lang.Class"
                                     ObjectClass
                                     nil
                                     [ObjectClass]
                                     _)}
                        end}

   class `type:java.lang.Class$PrimitiveClass` from Class
      attr
         arrayEncodedName
         zeroOfThisClass

      meth '<init>'(RawName ArrayEncodedName Zero $)
         arrayEncodedName := ArrayEncodedName
         zeroOfThisClass := Zero
         Class, '<init>'(RawName null nil nil _)
         unit
      end

      meth arrayEncodedName($)
         @arrayEncodedName
      end

      meth zeroOfThisClass($)
         @zeroOfThisClass
      end

      meth 'isPrimitive():scala.Boolean'($)
         true
      end
   end

end
