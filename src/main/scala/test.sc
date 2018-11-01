import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.universe._
object TestEnu extends Enumeration {
    type TestEnu = Value
    val hahha = Value
}
def getTypeTag[T: ru.TypeTag](obj: T) = ru.typeTag[T].tpe
class C
class D extends C
class E extends D
case class Person(name: String) {
    def ds(x: TestEnu.TestEnu, y: Int)(a:String) = println(x)
}

val paras = getTypeTag(Person("ds")).decl(TermName("ds")).typeSignature.paramLists.head

val xtype = paras.head.asTerm.typeSignature
paras.head.asTerm.typeSignature
val mirror = ru.runtimeMirror(getClass.getClassLoader)
ru.typeOf[TestEnu.type].typeSymbol.isModuleClass
mirror.reflect(this).reflectClass(ru.typeOf[TestEnu.type].typeSymbol.asClass).
//val enumClz = mirror.runtimeClass(paras.head.asTerm.typeSignature)
//paras.head.asTerm.typeSignature.dealias.decl(mirror)
////mirror.reflectModule(ru.typeOf[Int].decl(TermName("d"))).instance.asInstanceOf[enumClz]
//paras.head.asTerm.typeSignature.typeSymbol
//val isOption = mirror.runtimeClass(xtype).isAssignableFrom(classOf[Enumeration#Value])
////isOption
//

