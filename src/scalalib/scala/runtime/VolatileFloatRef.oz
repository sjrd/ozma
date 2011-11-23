functor

import
   ValueRef(makeValueRefClass) at 'x-ozma://root/scala/runtime/ValueRef.ozf'
   ModFunctor('module:java.lang.Float$':Module) at 'x-ozma://root/java/lang/Float.ozf'

export
   'type:scala.runtime.VolatileFloatRef':Type
   'class:scala.runtime.VolatileFloatRef':Class

define
   Type Class
in
   {ValueRef.makeValueRefClass "Float" true Module Type Class}
end
