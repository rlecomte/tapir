package codegen

import codegen.BasicGenerator.{indent, mapSchemaSimpleTypeToType}
import codegen.openapi.models.OpenapiModels.OpenapiDocument
import codegen.openapi.models.OpenapiSchemaType.{OpenapiSchemaArray, OpenapiSchemaObject, OpenapiSchemaSimpleType}
import cats.data.State

case class ComponentName(value: String) extends AnyVal
case class ComponentClassName(value: String) extends AnyVal
case class GeneratedClasses(map: Map[ComponentName, ComponentClassName]) extends AnyVal

class ClassDefinitionGenerator {

  type S[A] = State[GeneratedClasses, A]
  import cats.implicits._
  def classDefs(doc: OpenapiDocument): (GeneratedClasses, String) = {
    val (ref, classes) = doc.components.schemas.toList
      .foldMapM[S, List[String]] {
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

  private[codegen] def generateCoproduct(name: String, objects: Seq[])

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
