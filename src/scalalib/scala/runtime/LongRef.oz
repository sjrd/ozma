functor

import
   ValueRef(makeValueRefClass) at 'x-ozma://root/scala/runtime/ValueRef.ozf'
   ModFunctor('module:java.lang.Long$':Module) at 'x-ozma://root/java/lang/Long.ozf'

export
   'type:scala.runtime.LongRef':Type
   'class:scala.runtime.LongRef':Class

define
   Type Class
in
   {ValueRef.makeValueRefClass "Long" false Module Type Class}
end
