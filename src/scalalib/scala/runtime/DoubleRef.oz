functor

import
   ValueRef(makeValueRefClass) at 'x-ozma://root/scala/runtime/ValueRef.ozf'
   ModFunctor('module:java.lang.Double$':Module) at 'x-ozma://root/java/lang/Double.ozf'

export
   'type:scala.runtime.DoubleRef':Type
   'class:scala.runtime.DoubleRef':Class

define
   Type Class
in
   {ValueRef.makeValueRefClass "Double" false Module Type Class}
end
