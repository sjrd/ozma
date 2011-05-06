functor

import
   OzmaRuntime('NewObject':NewObject
               'StringLiteral':StringLiteral) at '../../scala/ozma/OzmaRuntime.ozf'
   `functor:java.lang.Object`('type:java.lang.Object':Object
                              'class:java.lang.Object':ObjectClass) at 'Object.ozf'
   `functor:java.lang.Class`('type:java.lang.Class':Class
                             'class:java.lang.Class':ClassClass) at 'Class.ozf'

export
   'type:java.lang.Integer':`type:java.lang.Integer`
   'class:java.lang.Integer':`class:java.lang.Integer`
   'type:java.lang.Integer$':`type:java.lang.Integer$`
   'module:java.lang.Integer$':`module:java.lang.Integer$`
   'class:java.lang.Integer$':`class:java.lang.Integer$`

define

   class `type:java.lang.Integer` from Object
      attr
         value

      meth '<init>#-1882783961'(Value $)
         Object, '<init>'(_)
         value := Value
         unit
      end

      meth intValue($)
         @value
      end

      meth toString($)
         Raw = {IntToString @value}
      in
         case Raw of &~|Tail then
            {StringLiteral &-|Tail}
         else
            {StringLiteral Raw}
         end
      end
   end

   class `type:java.lang.Integer$` from Object
      meth 'valueOf#-1882783961'(Value $)
         {NewObject `type:java.lang.Integer`
          `class:java.lang.Integer`
          '<init>#-1882783961'(Value _)}
      end
   end

   `module:java.lang.Integer$` = {ByNeed fun {$}
                                            {NewObject `type:java.lang.Integer$`
                                             `class:java.lang.Integer$`
                                             '<init>'(_)}
                                         end}

   `class:java.lang.Integer` = {ByNeed fun {$}
                                          {NewObject Class ClassClass
                                           '<init>'("java.lang.Integer"
                                                    ObjectClass
                                                    nil
                                                    [ObjectClass]
                                                    _)}
                                       end}

   `class:java.lang.Integer$` = {ByNeed fun {$}
                                           {NewObject Class ClassClass
                                            '<init>'("java.lang.Integer$"
                                                     ObjectClass
                                                     nil
                                                     [ObjectClass]
                                                     _)}
                                        end}

end
