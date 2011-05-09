functor

import
   OzmaRuntime('NewObject':NewObject) at '../../scala/ozma/OzmaRuntime.ozf'
   `functor:java.lang.Object`('type:java.lang.Object':`type:java.lang.Object`
                              'class:java.lang.Object':`class:java.lang.Object`) at 'Object.ozf'
   `functor:java.lang.Class`('type:java.lang.Class':`type:java.lang.Class`
                             'class:java.lang.Class':`class:java.lang.Class`) at 'Class.ozf'

export
   MakeNewArrayClass
   NewArrayObject

define

   fun {MakeNewArrayClass ComponentClass}
      {ByNeed fun {$}
                 {NewObject ArrayClassType `class:java.lang.Class`
                  '<init>'(ComponentClass _)}
              end}
   end

   fun {NewArrayObject ArrayClass Length InitValue}
      {NewObject ArrayType ArrayClass '<init>'(Length InitValue _)}
   end
   
   class ArrayType from `type:java.lang.Object`
      attr
         length
         rawArray

      meth '<init>'(Length InitValue $)
         length := Length
         rawArray := {NewArray 0 Length-1 InitValue}
         unit
      end

      meth length($)
         @length
      end

      meth toRawArray($)
         @rawArray
      end

      meth put(Index Value)
         (@rawArray).Index := Value
      end

      meth get(Index $)
         (@rawArray).Index
      end
   end

   class ArrayClassType from `type:java.lang.Class`
      attr
         componentClass

      meth '<init>'(ComponentClass $)
         RawName = '['#{ComponentClass arrayEncodedName($)}
      in
         componentClass := ComponentClass
         `type:java.lang.Class`, '<init>'(RawName
                                          `class:java.lang.Object`
                                          nil
                                          [`class:java.lang.Object`]
                                          _)
         unit
      end

      meth componentClass($)
         @componentClass
      end

      meth isAncestor(Cls $)
         if (Cls == self) orelse (Cls == `class:java.lang.Object`) then
            true
         elseif {Cls 'isArray#-676694176'($)} then
            local
               ClsComp = {Cls componentClass($)}
            in
               {ClsComp 'isAssignableFrom#1326077541'(@componentClass $)}
            end
         else
            false
         end
      end

      meth 'isArray#-676694176'($)
         true
      end

      meth arrayEncodedName($)
         {(@name) toRawVS($)}
      end
   end

end
