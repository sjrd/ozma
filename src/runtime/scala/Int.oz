functor

import
   `functor:scala.ScalaObject`('class:scala.ScalaObject':`class:scala.ScalaObject`
                               'type:scala.ScalaObject':`type:scala.ScalaObject`) at '../scala/ScalaObject.ozf'
   `functor:java.lang.Class`('class:java.lang.Class':`class:java.lang.Class`
                             'type:java.lang.Class':`type:java.lang.Class`) at '../java/lang/Class.ozf'
   `functor:scala.ozma.OzmaRuntime`('AsInstance':AsInstance
                                    'NewObject':NewObject
                                    'StringLiteral':StringLiteral) at '../scala/ozma/OzmaRuntime.ozf'
   `functor:java.lang.Integer`('module:java.lang.Integer$':`module:java.lang.Integer$`
                               'class:java.lang.Integer':`class:java.lang.Integer`) at '../java/lang/Integer.ozf'
   `functor:java.lang.Object`('class:java.lang.Object':`class:java.lang.Object`
                              'type:java.lang.Object':`type:java.lang.Object`) at '../java/lang/Object.ozf'

export
   'type:scala.Int$':`type:scala.Int$`
   'module:scala.Int$':`module:scala.Int$`
   'class:scala.Int$':`class:scala.Int$`

define
   class `type:scala.Int$`
      from `type:java.lang.Object` `type:scala.ScalaObject`

      meth 'MinValue'($)
         ~2147483648
      end

      meth 'MaxValue'($)
         2147483647
      end

      meth 'box#-1882783961'(`x` $)
         {`module:java.lang.Integer$` 'valueOf#-1882783961'(`x`
                                                            $)}
      end

      meth 'unbox#1063877011'(`x` $)
         {{AsInstance `x`
            `class:java.lang.Integer`} 'intValue'($)}
      end

      meth 'toString'($)
         {StringLiteral "object scala.Int"}
      end

      meth '<init>'($)
         _ = `type:java.lang.Object`, '<init>'($)
         unit
      end
   end
   `module:scala.Int$` = {ByNeed fun {$}
                                    {NewObject `type:scala.Int$`
                                               `class:scala.Int$`
                                               '<init>'(_)}
                                 end}
   `class:scala.Int$` = {ByNeed fun {$}
                                   {NewObject `type:java.lang.Class`
                                              `class:java.lang.Class`
                                              '<init>'("scala.Int$"
                                                       `class:java.lang.Object`
                                                       [`class:scala.ScalaObject`]
                                                       [`class:scala.ScalaObject`
                                                        `class:java.lang.Object`]
                                                       _)}
                                end}

end
