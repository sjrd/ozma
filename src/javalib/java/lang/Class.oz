functor

import
   OzmaRuntime('NewObject':NewObject
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
         Object, '<init>#1063877011'(_)
         name := {StringLiteral ARawName}
         superClass := ASuperClass
         mixins := AMixins
         ancestors := AAncestors
         arrayOfThisClass := {ByNeed fun {$}
                                        {MakeNewArrayClass self}
                                     end}
         unit
      end

      meth 'getName#1195259493'($)
         @name
      end

      % For convenience in other Oz functors
      meth getName($)
         @name
      end

      meth 'toString#1195259493'($)
         @name
      end

      meth 'isInstance#-1875011758'(Obj $)
         if Obj == null then
            false
         else
            {self 'isAssignableFrom#1326077541'({Obj getClass($)} $)}
         end
      end

      meth 'isAssignableFrom#1326077541'(SubClass $)
         {SubClass isAncestor(self $)}
      end

      meth isAncestor(Cls $)
         (Cls == self) orelse {Member Cls @ancestors}
      end

      meth 'isPrimitive#-676694176'($)
         false
      end

      meth 'isArray#-676694176'($)
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

      meth 'isPrimitive#-676694176'($)
         true
      end
   end

end
