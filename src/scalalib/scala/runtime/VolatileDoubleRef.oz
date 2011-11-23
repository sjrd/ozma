functor

import
   ValueRef(makeValueRefClass) at 'x-ozma://root/scala/runtime/ValueRef.ozf'
   ModFunctor('module:java.lang.Double$':Module) at 'x-ozma://root/java/lang/Double.ozf'

export
   'type:scala.runtime.VolatileDoubleRef':Type
   'class:scala.runtime.VolatileDoubleRef':Class

define
   Type Class
in
   {ValueRef.makeValueRefClass "Double" true Module Type Class}
end
