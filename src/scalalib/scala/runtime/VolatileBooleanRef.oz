functor

import
   ValueRef(makeValueRefClass) at 'x-ozma://root/scala/runtime/ValueRef.ozf'
   ModFunctor('module:java.lang.Boolean$':Module) at 'x-ozma://root/java/lang/Boolean.ozf'

export
   'type:scala.runtime.VolatileBooleanRef':Type
   'class:scala.runtime.VolatileBooleanRef':Class

define
   Type Class
in
   {ValueRef.makeValueRefClass "Boolean" true Module Type Class}
end
