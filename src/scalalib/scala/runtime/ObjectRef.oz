functor

import
   OzmaRuntime('NewObject':NewObject) at 'x-ozma://system/OzmaRuntime.ozf'
   `functor:java.lang.Object`('type:java.lang.Object':`type:java.lang.Object`
                              'class:java.lang.Object':`class:java.lang.Object`) at 'x-ozma://root/java/lang/Object.ozf'
   `functor:java.lang.Class`('type:java.lang.Class':`type:java.lang.Class`
                             'class:java.lang.Class':`class:java.lang.Class`) at 'x-ozma://root/java/lang/Class.ozf'

export
   'type:scala.runtime.ObjectRef':`type:scala.runtime.ObjectRef`
   'class:scala.runtime.ObjectRef':`class:scala.runtime.ObjectRef`

define

   class `type:scala.runtime.ObjectRef` from `type:java.lang.Object`
      attr
         ' elem'

      meth '<init>(java.lang.Object):scala.runtime.ObjectRef'(Value $)
         ' elem' := Value
         `type:java.lang.Object`, '<init>():java.lang.Object'($)
      end

      meth 'toString():java.lang.String'($)
         {@' elem' 'toString():java.lang.String'($)}
      end
   end

   `class:scala.runtime.ObjectRef` = {ByNeed fun {$}
                                                {NewObject `type:java.lang.Class`
                                                 `class:java.lang.Class`
                                                 '<init>'("scala.runtime.ObjectRef"
                                                          `class:java.lang.Object`
                                                          nil
                                                          [`class:java.lang.Object`]
                                                          _)}
                                             end}

end
