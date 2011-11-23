functor

import
   OzmaRuntime('NewObject':NewObject) at 'x-ozma://system/OzmaRuntime.ozf'
   `functor:java.lang.Object`('type:java.lang.Object':`type:java.lang.Object`
                              'class:java.lang.Object':`class:java.lang.Object`) at 'x-ozma://root/java/lang/Object.ozf'
   `functor:java.lang.Class`('type:java.lang.Class':`type:java.lang.Class`
                             'class:java.lang.Class':`class:java.lang.Class`) at 'x-ozma://root/java/lang/Class.ozf'

export
   MakeValueRefClass

define

   proc {MakeValueRefClass TypeName IsVolatile Module ?Type ?Class}
      TypeFullName = {Append "scala." TypeName}

      VolatilePrefix = if IsVolatile then "Volatile" else nil end
      FullName = {VirtualString.toString
                  "scala.runtime."#VolatilePrefix#TypeName#"Ref"}

      InitNameVS = '<init>('#TypeFullName#'):'#FullName
      InitName = {String.toAtom {VirtualString.toString InitNameVS}}

      ToStringNameVS = 'toString('#TypeFullName#'):java.lang.String'
      ToStringName = {String.toAtom {VirtualString.toString ToStringNameVS}}
   in
      class Type from `type:java.lang.Object`
         attr
            ' elem'

         meth !InitName(Value $)
            ' elem' := Value
            `type:java.lang.Object`, '<init>():java.lang.Object'($)
         end

         meth 'toString():java.lang.String'($)
            {Module ToStringName(@' elem' $)}
         end
      end

      Class = {ByNeed fun {$}
                         {NewObject `type:java.lang.Class`
                          `class:java.lang.Class`
                          '<init>'(FullName
                                   `class:java.lang.Object`
                                   nil
                                   [`class:java.lang.Object`]
                                   _)}
                      end}
   end

end
