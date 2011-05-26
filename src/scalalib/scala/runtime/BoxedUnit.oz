functor

import
   OzmaRuntime('NewObject':NewObject
               'StringLiteral':StringLiteral) at 'x-ozma://system/OzmaRuntime.ozf'
   `functor:java.lang.Object`('type:java.lang.Object':`type:java.lang.Object`
                              'class:java.lang.Object':`class:java.lang.Object`) at 'x-ozma://root/java/lang/Object.ozf'
   `functor:java.lang.Class`('type:java.lang.Class':`type:java.lang.Class`
                             'class:java.lang.Class':`class:java.lang.Class`) at 'x-ozma://root/java/lang/Class.ozf'

export
   'type:scala.runtime.BoxedUnit':`type:scala.runtime.BoxedUnit`
   'class:scala.runtime.BoxedUnit':`class:scala.runtime.BoxedUnit`
   'type:scala.runtime.BoxedUnit$':`type:scala.runtime.BoxedUnit$`
   'module:scala.runtime.BoxedUnit$':`module:scala.runtime.BoxedUnit$`
   'class:scala.runtime.BoxedUnit$':`class:scala.runtime.BoxedUnit$`
   'static:scala.runtime.BoxedUnit.UNIT':UNIT

define

   class `type:scala.runtime.BoxedUnit` from `type:java.lang.Object`
      meth '<init>#-1877334288'($)
         `type:java.lang.Object`, '<init>#1063877011'(_)
         unit
      end

      meth 'toString#1195259493'($)
         {StringLiteral "()"}
      end
   end

   class `type:scala.runtime.BoxedUnit$`
      from `type:java.lang.Object`

      meth '<init>#-1877334288'($)
         _ = `type:java.lang.Object`, '<init>#1063877011'($)
         unit
      end

      meth 'UNIT'($)
         @UNIT
      end
   end

   `module:scala.runtime.BoxedUnit$` = {ByNeed fun {$}
                                                  {NewObject `type:scala.runtime.BoxedUnit$`
                                                   `class:scala.runtime.BoxedUnit$`
                                                   '<init>#-1877334288'(_)}
                                               end}

   `class:scala.runtime.BoxedUnit` = {ByNeed fun {$}
                                                {NewObject `type:java.lang.Class` `class:java.lang.Class`
                                                 '<init>'("scala.runtime.BoxedUnit"
                                                          `class:java.lang.Object`
                                                          nil
                                                          [`class:java.lang.Object`]
                                                          _)}
                                             end}

   `class:scala.runtime.BoxedUnit$` = {ByNeed fun {$}
                                                 {NewObject `type:java.lang.Class` `class:java.lang.Class`
                                                  '<init>'("scala.runtime.BoxedUnit$"
                                                           `class:java.lang.Object`
                                                           nil
                                                           [`class:java.lang.Object`]
                                                           _)}
                                              end}

   UNIT = {NewCell {ByNeed fun {$}
                              {NewObject `type:scala.runtime.BoxedUnit`
                               `class:scala.runtime.BoxedUnit`
                               '<init>#-1877334288'(_)}
                           end}}

end
