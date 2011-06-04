package scala.tools.nsc
package backend
package ozcode

import scala.collection.mutable.HashMap

trait Natives { self: OzCodes =>
  import global.{ Symbol, definitions, stringToTermName }
  import ast._

  abstract class NativeMethod(val fullName: String, val resultTypeName: String,
      val argDefs: Pair[String, String]*) {
    def this(fullName: String, argDefs: Pair[String, String]*) =
      this(fullName, null:String, argDefs:_*)

    val hash = paramsHash(argDefs.toList map (_._2), resultTypeName)

    def body: ast.Phrase
  }

  object nativeMethods {
    private val methods = new HashMap[Pair[String, Int], NativeMethod]

    private[this] def register(native: NativeMethod) {
      methods += (native.fullName, native.hash) -> native
    }

    def init() {
      methods.clear()

      register(ScalaOzma_newUnbound)
      register(ScalaOzma_thread)
      register(ScalaOzma_waitBound)
      register(ScalaOzma_waitQuiet)
      register(ScalaOzma_waitNeeded)
      register(ScalaOzma_byNeed)
      register(ScalaOzma_byNeedFuture)
      register(ScalaOzma_makeFailedValue)
      register(ScalaOzma_sleep)

      register(Integer_toString)
      register(Float_toString)
      register(Character_toString)
      register(Integer_parseInt)
      register(Float_parseFloat)

      register(Boolean_box)
      register(Char_box)
      register(Byte_box)
      register(Short_box)
      register(Int_box)
      register(Long_box)
      register(Float_box)
      register(Double_box)

      register(Boolean_unbox)
      register(Char_unbox)
      register(Byte_unbox)
      register(Short_unbox)
      register(Int_unbox)
      register(Long_unbox)
      register(Float_unbox)
      register(Double_unbox)

      register(StandardOutPrintStream_writeString)
      register(StandardErrPrintStream_writeString)

      register(Port_newPort)
      register(Port_send)
      register(Port_newActiveObject)
    }

    def getBodyFor(symbol: Symbol) = {
      val key = (symbol.fullName, paramsHash(symbol))

      methods.get(key) match {
        case Some(method) => method.body
        case None =>
          Console.println("Warning: could not find native definition for")
          Console.println(key)
          null
      }
    }
  }

  object astDSL {
    implicit def char2literal(value: Char) = IntLiteral(value)
    implicit def int2literal(value: Int) = IntLiteral(value)
    implicit def float2literal(value: Double) = FloatLiteral(value)
    implicit def bool2literal(value: Boolean) = Bool(value)
    implicit def symbol2atom(value: scala.Symbol) = Atom(value.name)

    implicit def pair2colonInt(pair: Pair[Int, Phrase]) = pair match {
      case Pair(feature, phrase) => Colon(feature, phrase)
    }

    implicit def pair2colonSym(pair: Pair[scala.Symbol, Phrase]) = pair match {
      case Pair(feature, phrase) => Colon(feature, phrase)
    }

    def unit = ast.UnitVal()
    def nil = ast.Atom("nil")

    def __ = ast.Wildcard()
    def $ = ast.Dollar()

    // Method

    class MethodWrapper(val obj: Phrase, val name: String,
        val paramTypes: String*) {
      val hash = paramsHash(paramTypes.toList)

      def apply(args: Phrase*) = {
        val label = Atom(if (hash == 0) name else name + "#" + hash)
        val message = buildMessage(label, args.toList)

        Apply(obj, List(message))
      }
    }

    // Wrapper

    class PhraseWrapper(phrase: Phrase) {
      def ===(right: Phrase) = Eq(phrase, right)

      def ==>(right: Phrase) = CaseClause(phrase, right)

      def ::(left: Phrase) = Tuple(Atom("|"), left, phrase)

      def ~>(right: Phrase) = BinaryOpApply(".", phrase, right)

      def toRawVS = new MethodWrapper(phrase, "toRawVS")
      def toRawString = new MethodWrapper(phrase, "toRawString")

      def doApply = new MethodWrapper(phrase, "apply", "java.lang.Object")
    }

    implicit def wrapPhrase(phrase: Phrase) = new PhraseWrapper(phrase)
    implicit def wrapSymbol(symbol: scala.Symbol) =
      new PhraseWrapper(symbol2atom(symbol))

    // Builtin functions

    case class BuiltinFunction(fun: Phrase) {
      def apply(params: Phrase*) = Apply(fun, params.toList)
    }

    def Wait = BuiltinFunction(Variable("Wait"))
    def WaitNeeded = BuiltinFunction(Variable("WaitNeeded"))
    def ByNeed = BuiltinFunction(Variable("ByNeed"))
    def ByNeedFuture = BuiltinFunction(Variable("ByNeedFuture"))
    def IsDet = BuiltinFunction(Variable("IsDet"))
    def Delay = BuiltinFunction(Variable("Delay"))
    def IntToString = BuiltinFunction(Variable("IntToString"))
    def FloatToString = BuiltinFunction(Variable("FloatToString"))
    def StringToInt = BuiltinFunction(Variable("StringToInt"))
    def StringToFloat = BuiltinFunction(Variable("StringToFloat"))
    def StringLiteral = BuiltinFunction(Variable("StringLiteral"))
    def NewPort = BuiltinFunction(Variable("NewPort"))
    def Send = BuiltinFunction(Variable("Send"))
    def NewOzmaPort = BuiltinFunction(Variable("NewOzmaPort"))
    def NewActiveObject = BuiltinFunction(Variable("NewActiveObject"))

    // System module

    object System {
      @inline private[this] def System = Variable("System")

      def printInfo = BuiltinFunction(System ~> 'printInfo)
      def showInfo = BuiltinFunction(System ~> 'showInfo)
      def printError = BuiltinFunction(System ~> 'printError)
      def showError = BuiltinFunction(System ~> 'showError)
    }

    // Value module

    object Value {
      @inline private[this] def Value = Variable("Value")

      def waitQuiet = BuiltinFunction(Value ~> 'waitQuiet)
      def status = BuiltinFunction(Value ~> 'status)
      def makeNeeded = BuiltinFunction(Value ~> 'makeNeeded)
      def failed = BuiltinFunction(Value ~> 'failed)
    }

    // New constructor call

    def New(clazz: Symbol, arguments: Pair[Phrase, String]*) = {
      val arguments1 = arguments.toList
      val actualArgs = arguments1 map (_._1)
      val paramTypeNames = arguments1 map (_._2)
      genNew(clazz, actualArgs,
          atomForSymbol("<init>", paramsHash(paramTypeNames, clazz.fullName)))
    }

    def New(className: String, arguments: Pair[Phrase, String]*): Phrase =
      New(definitions.getClass(className toTypeName), arguments:_*)
  }

  import astDSL._
  import astDSL.StringLiteral

  object ScalaOzma_newUnbound extends
      NativeMethod("scala.ozma.package.newUnbound", "java.lang.Object") {
    def body = __
  }

  object ScalaOzma_thread extends
      NativeMethod("scala.ozma.package.thread", "java.lang.Object",
          "`stat`" -> "scala.Function0") {
    def body = {
      val stat = QuotedVar("stat")
      Thread(stat.doApply())
    }
  }

  object ScalaOzma_waitBound extends NativeMethod(
      "scala.ozma.package.waitBound", "scala.Unit",
      "`value`" -> "java.lang.Object") {
    def body = {
      And(
          Wait(QuotedVar("value")),
          unit
      )
    }
  }

  object ScalaOzma_waitQuiet extends NativeMethod(
      "scala.ozma.package.waitQuiet", "scala.Unit",
      "`value`" -> "java.lang.Object") {
    def body = {
      And(
          Value.waitQuiet(QuotedVar("value")),
          unit
      )
    }
  }

  object ScalaOzma_waitNeeded extends NativeMethod(
      "scala.ozma.package.waitNeeded", "scala.Unit",
      "`value`" -> "java.lang.Object") {
    def body = {
      And(
          WaitNeeded(QuotedVar("value")),
          unit
      )
    }
  }

  object ScalaOzma_byNeed extends NativeMethod(
      "scala.ozma.package.byNeed", "java.lang.Object",
      "`value`" -> "scala.Function0") {
    def body = {
      val value = QuotedVar("value")
      ByNeed(Fun($, Nil, value.doApply()))
    }
  }

  object ScalaOzma_byNeedFuture extends NativeMethod(
      "scala.ozma.package.byNeedFuture", "java.lang.Object",
      "`value`" -> "scala.Function0") {
    def body = {
      val value = QuotedVar("value")
      ByNeedFuture(Fun($, Nil, value.doApply()))
    }
  }

  object ScalaOzma_makeFailedValue extends NativeMethod(
      "scala.ozma.package.makeFailedValue", "java.lang.Object",
      "`throwable`" -> "java.lang.Throwable") {
    def body = {
      Value.failed(QuotedVar("throwable"))
    }
  }

  object ScalaOzma_sleep extends NativeMethod(
      "scala.ozma.package.sleep", "scala.Unit",
      "`ms`" -> "scala.Int") {
    def body = {
      val ms = QuotedVar("ms")
      And(
          Delay(ms),
          unit
      )
    }
  }

  abstract class Number_toString(fullName: String) extends
      NativeMethod(fullName, "java.lang.String") {
    def ValueToString: BuiltinFunction

    def body = {
      def ValueField = QuotedVar("value ")
      def Raw = Variable("Raw")
      def Tail = Variable("Tail")

      LocalDef(
          Raw === ValueToString(At(ValueField)),
      // in
          Case(Raw, List(
              '~' :: Tail ==> StringLiteral('-' :: Tail)),
          // else
              StringLiteral(Raw)
          )
      )
    }
  }

  object Integer_toString extends Number_toString(
      "java.lang.Integer.toString") {
    def ValueToString = IntToString
  }

  object Float_toString extends Number_toString(
      "java.lang.Float.toString") {
    def ValueToString = FloatToString
  }

  object Character_toString extends NativeMethod(
      "java.lang.Character.toString", "java.lang.String") {
    def body = {
      val ValueField = QuotedVar(" value")

      StringLiteral(At(ValueField) :: nil)
    }
  }

  abstract class Number_parse(fullName: String, resultType: String)
      extends NativeMethod(fullName, resultType, "`s`" -> "java.lang.String") {
    def StringToValue: BuiltinFunction
    def ErrorIdentifier: Atom

    def body = {
      def S = QuotedVar("s")
      def Raw = Variable("Raw")
      def Tail = Variable("Tail")

      val tryBlock = LocalDef(
          Raw === S.toRawString(),
      // in
          Case(Raw, List(
              '-' :: Tail ==> StringToValue('~' :: Tail)),
          // else
              StringToValue(Raw)
          )
      )

      Try(
          tryBlock,
      Catch(List(
          'error('kernel(ErrorIdentifier, __), 'debug -> __) ==>
              Raise(New("java.lang.NumberFormatException"))
      )), NoFinally())
    }
  }

  object Integer_parseInt extends Number_parse(
      "java.lang.Integer.parseInt", "scala.Int") {
    def StringToValue = StringToInt
    def ErrorIdentifier = Atom("stringNoInt")
  }

  object Float_parseFloat extends Number_parse(
      "java.lang.Float.parseFloat", "scala.Float") {
    def StringToValue = StringToFloat
    def ErrorIdentifier = Atom("stringNoFloat")
  }

  abstract class Primitive_box(primitiveType: String, boxedType: String)
      extends NativeMethod(primitiveType+".box", boxedType,
          "`x`" -> primitiveType) {

    def body = {
      def BoxedType = QuotedVar("module:"+boxedType+"$")

      implicit def wrapForValueOf(phrase: Phrase) = new {
        def valueOf = new MethodWrapper(phrase, "valueOf",
            primitiveType, boxedType)
      }

      def x = QuotedVar("x")
      def R = Variable("R")

      Case(Value.status(x), List(
          'det(__) ==> BoxedType.valueOf(x),
          'failed ==> x
      ), // else
          LocalDef(
              R,
          And(// in
              Thread(And(
                  WaitNeeded(x),
                  Value.makeNeeded(R)
              )),
              R === ByNeedFuture(Fun($, Nil,
                  And(Wait(x), BoxedType.valueOf(x))
              )),
              R
          ))
      )
    }
  }

  object Boolean_box extends Primitive_box("scala.Boolean", "java.lang.Boolean")
  object Char_box extends Primitive_box("scala.Char", "java.lang.Character")
  object Byte_box extends Primitive_box("scala.Byte", "java.lang.Byte")
  object Short_box extends Primitive_box("scala.Short", "java.lang.Short")
  object Int_box extends Primitive_box("scala.Int", "java.lang.Integer")
  object Long_box extends Primitive_box("scala.Long", "java.lang.Long")
  object Float_box extends Primitive_box("scala.Float", "java.lang.Float")
  object Double_box extends Primitive_box("scala.Double", "java.lang.Double")

  abstract class Primitive_unbox(primitiveType: String,
      theValueMethod: String)
      extends NativeMethod(primitiveType+".unbox", primitiveType,
          "`x`" -> "java.lang.Object") {

    def body = {
      implicit def wrapForTheValue(phrase: Phrase) = new {
        def theValue = new MethodWrapper(phrase, theValueMethod, primitiveType)
      }

      def x = QuotedVar("x")
      def R = Variable("R")

      Case(Value.status(x), List(
          'det(__) ==> x.theValue(),
          'failed ==> x
      ), // else
          LocalDef(
              R,
          And(// in
              Thread(And(
                  WaitNeeded(x),
                  Value.makeNeeded(R)
              )),
              R === ByNeedFuture(Fun($, Nil, x.theValue())),
              R
          ))
      )
    }
  }

  object Boolean_unbox extends Primitive_unbox("scala.Boolean", "booleanValue")
  object Char_unbox extends Primitive_unbox("scala.Char", "charValue")
  object Byte_unbox extends Primitive_unbox("scala.Byte", "byteValue")
  object Short_unbox extends Primitive_unbox("scala.Short", "shortValue")
  object Int_unbox extends Primitive_unbox("scala.Int", "intValue")
  object Long_unbox extends Primitive_unbox("scala.Long", "longValue")
  object Float_unbox extends Primitive_unbox("scala.Float", "floatValue")
  object Double_unbox extends Primitive_unbox("scala.Double", "doubleValue")

  object StandardOutPrintStream_writeString extends NativeMethod(
      "java.lang.StandardOutPrintStream.writeString",
      "scala.Unit", "`s`" -> "java.lang.String") {
    def body = {
      And(System.printInfo(QuotedVar("s").toRawVS()), unit)
    }
  }

  object StandardErrPrintStream_writeString extends NativeMethod(
      "java.lang.StandardErrPrintStream.writeString",
      "scala.Unit", "`s`" -> "java.lang.String") {
    def body = {
      And(System.printError(QuotedVar("s").toRawVS()), unit)
    }
  }

  object Port_newPort extends NativeMethod("ozma.Port.newPort",
      "scala.Tuple2") {
    def body = {
      NewOzmaPort()
    }
  }

  object Port_send extends NativeMethod("ozma.Port.send",
      "scala.Unit", "`element`" -> "java.lang.Object") {
    def body = {
      And(
          Send(At(QuotedVar("rawPort ")), QuotedVar("element")),
          unit
      )
    }
  }

  object Port_newActiveObject extends NativeMethod(
      "ozma.Port.newActiveObject",
      "java.lang.Object", "`obj`" -> "java.lang.Object") {
    def body = {
      NewActiveObject(QuotedVar("obj"))
    }
  }
}
