functor

import
   ValueRef(makeValueRefClass) at 'x-ozma://root/scala/runtime/ValueRef.ozf'
   ModFunctor('module:java.lang.Long$':Module) at 'x-ozma://root/java/lang/Long.ozf'

export
   'type:scala.runtime.VolatileLongRef':Type
   'class:scala.runtime.VolatileLongRef':Class

define
   Type Class
in
   {ValueRef.makeValueRefClass "Long" true Module Type Class}
end
