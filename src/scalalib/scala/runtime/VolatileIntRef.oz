functor

import
   ValueRef(makeValueRefClass) at 'x-ozma://root/scala/runtime/ValueRef.ozf'
   ModFunctor('module:java.lang.Integer$':Module) at 'x-ozma://root/java/lang/Integer.ozf'

export
   'type:scala.runtime.VolatileIntRef':Type
   'class:scala.runtime.VolatileIntRef':Class

define
   Type Class
in
   {ValueRef.makeValueRefClass "Int" true Module Type Class}
end
