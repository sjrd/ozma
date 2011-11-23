functor

import
   ValueRef(makeValueRefClass) at 'x-ozma://root/scala/runtime/ValueRef.ozf'
   ModFunctor('module:java.lang.Short$':Module) at 'x-ozma://root/java/lang/Short.ozf'

export
   'type:scala.runtime.VolatileShortRef':Type
   'class:scala.runtime.VolatileShortRef':Class

define
   Type Class
in
   {ValueRef.makeValueRefClass "Short" true Module Type Class}
end
