functor

import
   System
   OzmaRuntime('NewObject':NewObject) at 'x-ozma://root/scala/ozma/OzmaRuntime.ozf'
   `functor:java.lang.Object`('type:java.lang.Object':Object
                              'class:java.lang.Object':ObjectClass) at 'x-ozma://root/java/lang/Object.ozf'
   `functor:java.lang.Class`('type:java.lang.Class':Class
                             'class:java.lang.Class':ClassClass) at 'x-ozma://root/java/lang/Class.ozf'

export
   'type:scala.Console$':`type:scala.Console$`
   'module:scala.Console$':`module:scala.Console$`
   'class:scala.Console$':`class:scala.Console$`

define

   class `type:scala.Console$` from Object
      meth '<init>'($)
         Object, '<init>#1063877011'(_)
         unit
      end

      meth 'print#-1794996995'(Str $)
         {System.printInfo {Str toRawVS($)}}
         unit
      end

      meth 'print#-313144565'(Obj $)
         {self 'print#-1794996995'({Obj toString($)} $)}
      end

      meth 'println#-1794996995'(Str $)
         {System.showInfo {Str toRawVS($)}}
         unit
      end

      meth 'println#-313144565'(Obj $)
         {self 'println#-1794996995'({Obj toString($)} $)}
      end
   end

   `module:scala.Console$` = {ByNeed fun {$}
                                        {NewObject `type:scala.Console$`
                                         `class:scala.Console$`
                                         '<init>'(_)}
                                     end}

   `class:scala.Console$` = {ByNeed fun {$}
                                       {NewObject Class ClassClass
                                        '<init>'("scala.Console$"
                                                 ObjectClass
                                                 nil
                                                 [ObjectClass]
                                                 _)}
                                    end}

end
