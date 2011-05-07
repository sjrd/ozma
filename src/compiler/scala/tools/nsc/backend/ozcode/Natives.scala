package scala.tools.nsc
package backend
package ozcode

import scala.collection.mutable.HashMap

trait Natives { self: OzCodes =>
  import global.{ Symbol, definitions, stringToTermName }
  import ast._

  abstract class NativeMethod(val fullName: String,
      val argDefs: Pair[String, String]*) {
    val hash = paramsHash(argDefs.toList map (_._2))

    def body: ast.Phrase
  }

  object nativeMethods {
    private val methods = new HashMap[Pair[String, Int], NativeMethod]

    private[this] def register(native: NativeMethod) {
      methods += (native.fullName, native.hash) -> native
    }

    def init() {
      methods.clear()

      register(Integer_toString)
      register(Float_toString)
      register(Integer_parseInt)
      register(Float_parseFloat)
    }

    def getBodyFor(symbol: Symbol) = {
      val key = (symbol.fullName, paramsHash(symbol))

      methods.get(key) match {
        case Some(method) => method.body
        case None => null
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

    def __ = ast.Wildcard()

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

      def toRawString = new MethodWrapper(phrase, "toRawString")
    }

    implicit def wrapPhrase(phrase: Phrase) = new PhraseWrapper(phrase)

    // Builtin functions

    case class BuiltinFunction(fun: Phrase) {
      def apply(params: Phrase*) = Apply(fun, params.toList)
    }

    def IntToString = BuiltinFunction(Variable("IntToString"))
    def FloatToString = BuiltinFunction(Variable("FloatToString"))
    def StringToInt = BuiltinFunction(Variable("StringToInt"))
    def StringToFloat = BuiltinFunction(Variable("StringToFloat"))
    def StringLiteral = BuiltinFunction(Variable("StringLiteral"))

    // New constructor call

    def New(clazz: Symbol, arguments: Pair[Phrase, String]*) = {
      val arguments1 = arguments.toList
      val actualArgs = arguments1 map (_._1)
      val paramTypeNames = arguments1 map (_._2)
      genNew(clazz, actualArgs,
          atomForSymbol("<init>", paramsHash(paramTypeNames)))
    }

    def New(className: String, arguments: Pair[Phrase, String]*): Phrase =
      New(definitions.getClass(className toTypeName), arguments:_*)
  }

  import astDSL._
  import astDSL.StringLiteral

  abstract class Number_toString(fullName: String) extends
      NativeMethod(fullName) {
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

  abstract class Number_parse(fullName: String)
      extends NativeMethod(fullName, "`s`" -> "java.lang.String") {
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
              '-' :: Tail ==> StringToValue('-' :: Tail)),
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
      "java.lang.Integer.parseInt") {
    def StringToValue = StringToInt
    def ErrorIdentifier = Atom("stringNoInt")
  }

  object Float_parseFloat extends Number_parse(
      "java.lang.Float.parseFloat") {
    def StringToValue = StringToFloat
    def ErrorIdentifier = Atom("stringNoFloat")
  }
}
