functor

import
   ValueRef(makeValueRefClass) at 'x-ozma://root/scala/runtime/ValueRef.ozf'
   ModFunctor('module:java.lang.Integer$':Module) at 'x-ozma://root/java/lang/Integer.ozf'

export
   'type:scala.runtime.IntRef':Type
   'class:scala.runtime.IntRef':Class

define
   Type Class
in
   {ValueRef.makeValueRefClass "Int" false Module Type Class}
end
