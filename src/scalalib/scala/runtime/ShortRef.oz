functor

import
   ValueRef(makeValueRefClass) at 'x-ozma://root/scala/runtime/ValueRef.ozf'
   ModFunctor('module:java.lang.Short$':Module) at 'x-ozma://root/java/lang/Short.ozf'

export
   'type:scala.runtime.ShortRef':Type
   'class:scala.runtime.ShortRef':Class

define
   Type Class
in
   {ValueRef.makeValueRefClass "Short" false Module Type Class}
end
