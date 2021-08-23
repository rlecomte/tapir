package codegen

import codegen.BasicGenerator.{indent, mapSchemaSimpleTypeToType}
import codegen.openapi.models.OpenapiModels.OpenapiDocument
import codegen.openapi.models.OpenapiSchemaType.{OpenapiSchemaArray, OpenapiSchemaObject, OpenapiSchemaSimpleType, OpenapiSchemaOneOf}
import cats.data.State
import codegen.openapi.models.OpenapiSchemaType

case class ComponentName(value: String) extends AnyVal
case class ComponentClassName(value: String) extends AnyVal
case class GeneratedClasses(map: Map[ComponentName, ComponentClassName]) extends AnyVal

class ClassDefinitionGenerator {

  type S[A] = State[GeneratedClasses, A]
  import cats.implicits._
  def classDefs(doc: OpenapiDocument): (GeneratedClasses, String) = {
    val (ref, classes) = doc.components.schemas.toList
      .foldMapM[S, List[String]] {
        case (name, oneOf: OpenapiSchemaOneOf) =>
          val (className, sealedTrait) = generateCoproduct(name, oneOf)
          State.modify[GeneratedClasses](s => GeneratedClasses(s.map + (ComponentName(name) -> className))).as(List(sealedTrait))
        case (name, obj: OpenapiSchemaObject) =>
          val (className, genCode) = generateClass(name, obj)
          State.modify[GeneratedClasses](s => GeneratedClasses(s.map + (ComponentName(name) -> className))).as(genCode.toList)
        case (name, obj: OpenapiSchemaSimpleType) =>
          val (ref, alias) = generateAlias(name, obj)
          State.modify[GeneratedClasses](s => GeneratedClasses(s.map + (ComponentName(name) -> ref))).as(List(alias))
        case (n, _) => throw new NotImplementedError(s"Only objects supported! $n")
      }
      .run(GeneratedClasses(Map.empty))
      .value

    (ref, classes.mkString("\n"))
  }

  private[codegen] def generateClass(name: String, obj: OpenapiSchemaObject): (ComponentClassName, Seq[String]) = {
    def addName(parentName: String, key: String) = parentName + key.replace('_', ' ').replace('-', ' ').capitalize.replace(" ", "")
    def rec(name: String, obj: OpenapiSchemaObject, acc: List[String]): Seq[String] = {
      val innerClasses = obj.properties
        .collect { case (propName, st: OpenapiSchemaObject) =>
          val newName = addName(name, propName)
          rec(newName, st, Nil)
        }
        .flatten
        .toList
      val fs = obj.properties.collect {
        case (k, st: OpenapiSchemaSimpleType) =>
          val t = mapSchemaSimpleTypeToType(st)
          innerTypePrinter(k, t._1, t._2 || !obj.required.contains(k))
        case (k, st: OpenapiSchemaObject) =>
          val t = addName(name, k)
          innerTypePrinter(k, t, st.nullable || !obj.required.contains(k))
        case (k, OpenapiSchemaArray(inner: OpenapiSchemaSimpleType, nullable)) =>
          val innerT = mapSchemaSimpleTypeToType(inner)._1
          innerTypePrinter(k, s"Seq[$innerT]", nullable || !obj.required.contains(k))
      }
      require(fs.size == obj.properties.size, s"We can't serialize some of the properties yet! $name $obj")
      s"""|case class $name (
          |${indent(2)(fs.mkString(",\n"))}
          |)""".stripMargin :: innerClasses ::: acc
    }

    val refName = addName("", name)
    (ComponentClassName(refName), rec(refName, obj, Nil))
  }

  private[codegen] def generateAlias(name: String, simpleType: OpenapiSchemaSimpleType): (ComponentClassName, String) = {
    val n = name.replace('_', ' ').replace('-', ' ').capitalize.replace(" ", "")
    val t = mapSchemaSimpleTypeToType(simpleType)
    (ComponentClassName(n), s"type $n = ${t._1}")
  }

  private[codegen] def generateCoproduct(name: String, oneOf: OpenapiSchemaType.OpenapiSchemaOneOf): (ComponentClassName, String) = {
    def addName(parentName: String, key: String) = parentName + key.replace('_', ' ').replace('-', ' ').capitalize.replace(" ", "")

    val refs: Seq[OpenapiSchemaType.OpenapiSchemaRef] = oneOf.types.collect {
      case ref: OpenapiSchemaType.OpenapiSchemaRef => ref
      case _                                       => throw new RuntimeException("Unsupported type in oneOf")
    }

    val refName = addName("", name)
    val refsType = refs.map(_.name).map(e => addName("", e.trim.stripPrefix("#/components/schemas/")))
    val subDefs = refsType.map(t => s"case class ${t}_(value: $t) extends $refName")

    val circeEncoders = refsType.map(t => s"case ${t}_(v) => v.asJson")

    val encoder = s"""|implicit val encode$refName: Encoder[$refName] = Encoder.instance {
                      |${indent(2)(circeEncoders.mkString("\n"))}
                      |}""".stripMargin

    val circeDecoders = refsType.map(t => s"Decoder[$t].widen")

    //TODO deal with discriminator
    val decoder = s"""|implicit val decode$refName: Decoder[$refName] = List[Decoder[$refName]](
                      |${indent(2)(circeDecoders.mkString(",\n"))}
                      |).reduceLeft(_ or _)""".stripMargin

    val gen = s"""|sealed trait $refName
                  |object $refName {
                  |${indent(2)(subDefs.mkString("\n"))}
                  |
                  |${indent(2)(decoder)}
                  |
                  |${indent(2)(encoder)}
                  |}""".stripMargin

    (ComponentClassName(refName), gen)
  }

  private val reservedKeys = scala.reflect.runtime.universe.asInstanceOf[scala.reflect.internal.SymbolTable].nme.keywords.map(_.toString)

  private def fixKey(key: String) = {
    if (reservedKeys.contains(key))
      s"`$key`"
    else
      key
  }

  private def innerTypePrinter(key: String, tp: String, optional: Boolean) = {
    val fixedType = if (optional) s"Option[$tp]" else tp
    val fixedKey = fixKey(key)
    s"$fixedKey: $fixedType"
  }
}
