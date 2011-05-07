package scala.tools.nsc
package backend
package ozcode

import scala.collection.mutable.HashMap

trait Natives { self: OzCodes =>
  import global.{ Symbol }
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

    implicit def pair2colon(pair: Pair[Feature, Phrase]) = pair match {
      case Pair(feature, phrase) => Colon(feature, phrase)
    }

    def unit = ast.UnitVal()

    // Wrapper

    class PhraseWrapper(phrase: Phrase) {
      def ===(right: Phrase) = Eq(phrase, right)

      def ==>(right: Phrase) = CaseClause(phrase, right)

      def :|:(left: Phrase) = Tuple(Atom("|"), left, phrase)
    }

    implicit def wrapPhrase(phrase: Phrase) = new PhraseWrapper(phrase)

    // Builtin functions

    case class BuiltinFunction(fun: Phrase) {
      def apply(params: Phrase*) = Apply(fun, params.toList)
    }

    def IntToString = BuiltinFunction(Variable("IntToString"))
    def FloatToString = BuiltinFunction(Variable("FloatToString"))
    def StringLiteral = BuiltinFunction(Variable("StringLiteral"))
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
              '~' :|: Tail ==> StringLiteral('-' :|: Tail)),
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
}
