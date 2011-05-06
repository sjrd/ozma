functor

import
   OzmaRuntime('NewObject':NewObject
               'StringLiteral':StringLiteral) at '../../scala/ozma/OzmaRuntime.ozf'
   `functor:java.lang.Object`('type:java.lang.Object':Object
                              'class:java.lang.Object':ObjectClass) at 'Object.ozf'

export
   'type:java.lang.Class':Class
   'class:java.lang.Class':ClassClass

define

   class Class from Object
      attr
         name
         superClass
         mixins
         ancestors

      meth '<init>'(ARawName ASuperClass AMixins AAncestors $)
         Object, '<init>'(_)
         name := {StringLiteral ARawName}
         superClass := ASuperClass
         mixins := AMixins
         ancestors := AAncestors
         unit
      end

      meth toString($)
         @name
      end

      meth 'isInstance#1063877011'(Obj $)
         {self 'isAssignableFrom#-530663260'({Obj getClass($)} $)}
      end

      meth 'isAssignableFrom#-530663260'(SubClass $)
         {SubClass IsAncestor(self $)}
      end

      meth IsAncestor(Cls $)
         (Cls == self) orelse {Member Cls @ancestors}
      end
   end

   ClassClass = {ByNeed fun {$}
                           {NewObject Class ClassClass
                            '<init>'("java.lang.Class"
                                     ObjectClass
                                     nil
                                     [ObjectClass]
                                     _)}
                        end}

end
