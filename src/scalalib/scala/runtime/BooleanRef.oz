functor

import
   ValueRef(makeValueRefClass) at 'x-ozma://root/scala/runtime/ValueRef.ozf'
   ModFunctor('module:java.lang.Boolean$':Module) at 'x-ozma://root/java/lang/Boolean.ozf'

export
   'type:scala.runtime.BooleanRef':Type
   'class:scala.runtime.BooleanRef':Class

define
   Type Class
in
   {ValueRef.makeValueRefClass "Boolean" false Module Type Class}
end
