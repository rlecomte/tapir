package codegen

import codegen.openapi.models.OpenapiModels.OpenapiDocument
import codegen.openapi.models.OpenapiSchemaType.{
  OpenapiSchemaBoolean,
  OpenapiSchemaDouble,
  OpenapiSchemaFloat,
  OpenapiSchemaInt,
  OpenapiSchemaLong,
  OpenapiSchemaRef,
  OpenapiSchemaSimpleType,
  OpenapiSchemaString
}
import codegen.openapi.models.OpenapiSchemaType

object BasicGenerator {

  val classGenerator = new ClassDefinitionGenerator()
  val endpointGenerator = new EndpointGenerator()

  def generateObjects(doc: OpenapiDocument, packagePath: String, objName: String): String = {
    val (ref, gen) = classGenerator.classDefs(doc)
    s"""|
        |package $packagePath
        |
        |object $objName {
        |
        |${indent(2)(imports)}
        |
        |${indent(2)(gen)}
        |
        |${indent(2)(endpointGenerator.endpointDefs(ref, doc))}
        |
        |}
        |""".stripMargin
  }

  private[codegen] def imports: String =
    """import sttp.tapir._
      |import sttp.tapir.json.circe._
      |import sttp.tapir.generic.auto._
      |import io.circe.generic.auto._
      |import io.circe.syntax._
      |import cats.syntax.functor._
      |""".stripMargin

  def indent(i: Int)(str: String): String = {
    str.linesIterator.map(" " * i + _).mkString("\n")
  }

  def mapSchemaSimpleTypeToType(osst: OpenapiSchemaSimpleType): (String, Boolean) = {
    osst match {
      case OpenapiSchemaDouble(nb) =>
        ("Double", nb)
      case OpenapiSchemaFloat(nb) =>
        ("Float", nb)
      case OpenapiSchemaInt(nb) =>
        ("Int", nb)
      case OpenapiSchemaLong(nb) =>
        ("Long", nb)
      case OpenapiSchemaString(nb) =>
        ("String", nb)
      case OpenapiSchemaBoolean(nb) =>
        ("Boolean", nb)
      case OpenapiSchemaRef(t) =>
        (t.split('/').last, false)
      case OpenapiSchemaType.OpenapiSchemaDate(nb) =>
        ("String", nb)
      case OpenapiSchemaType.OpenapiSchemaDateTime(nb) =>
        ("String", nb)
      case t => throw new NotImplementedError(s"Not all simple types supported! $t")
    }
  }
}
