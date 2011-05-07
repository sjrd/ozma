functor

import
   OzmaRuntime('NewObject':NewObject
               'InitObject':InitObject
               'StringLiteral':StringLiteral) at '../../scala/ozma/OzmaRuntime.ozf'
   `functor:java.lang.Class`('type:java.lang.Class':Class
                             'class:java.lang.Class':ClassClass) at 'Class.ozf'

export
   'type:java.lang.Object':Object
   'class:java.lang.Object':ObjectClass

define

   class Object from BaseObject
      attr
         'class'

      meth !InitObject(ObjClass InitMessage)
         'class' := ObjClass
         {self InitMessage}
      end

      meth '<init>'($)
         unit
      end

      meth getClass($)
         @'class'
      end

      % In the JavaDoc, defined as:
      %   getClass().getName() + '@' + Integer.toHexString(hashCode())
      meth toString($)
         ClassName = {{@'class' getName($)} toRawString($)}
         HashCode = {self hashCode($)}
      in
         {StringLiteral ClassName#'@'#HashCode}
      end

      meth hashCode($)
         0
      end

      meth 'equals#1063877011'(Other $)
         self == Other
      end
   end

   ObjectClass = {ByNeed fun {$}
                            {NewObject Class ClassClass
                             '<init>'("java.lang.Object"
                                      unit
                                      nil
                                      nil
                                      _)}
                         end}

end
