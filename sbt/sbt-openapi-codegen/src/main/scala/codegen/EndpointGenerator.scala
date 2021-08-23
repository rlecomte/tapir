package codegen

import codegen.BasicGenerator.{indent, mapSchemaSimpleTypeToType}
import codegen.openapi.models.OpenapiModels.{OpenapiDocument, OpenapiParameter, OpenapiPath, OpenapiRequestBody, OpenapiResponse}
import codegen.openapi.models.OpenapiSchemaType
import codegen.openapi.models.OpenapiSchemaType.{OpenapiSchemaArray, OpenapiSchemaSimpleType}
import _root_.codegen.openapi.models.OpenapiModels

class EndpointGenerator {

  private[codegen] def allEndpoints: String = "generatedEndpoints"

  def endpointDefs(ref: GeneratedClasses, doc: OpenapiDocument): String = {
    val ge = doc.paths.flatMap(generatedEndpoints(ref, _))
    val definitions = ge
      .map { case (name, definition) =>
        s"""|val $name =
            |${indent(2)(definition)}
            |""".stripMargin
      }
      .mkString("\n")
    val allEP = s"val $allEndpoints = List(${ge.map(_._1).mkString(", ")})"

    s"""|$definitions
        |
        |$allEP
        |""".stripMargin
  }

  private[codegen] def generatedEndpoints(ref: GeneratedClasses, p: OpenapiPath): Seq[(String, String)] = {
    p.methods.map { m =>
      val definition =
        s"""|endpoint
            |  .${m.methodType}
            |  ${urlMapper(ref, p.url, m.parameters)}
            |${indent(2)(ins(ref, m.parameters, m.requestBody))}
            |${indent(2)(outs(ref, m.responses))}
            |""".stripMargin

      val name = m.methodType + p.url.split('/').map(_.replace("{", "").replace("}", "").toLowerCase.capitalize).mkString
      (name, definition)
    }
  }

  private def urlMapper(ref: GeneratedClasses, url: String, parameters: Seq[OpenapiParameter]): String = {
    //.in(("books" / path[String]("genre") / path[Int]("year")).mapTo[BooksFromYear])
    val inPath = url.split('/').filter(_.nonEmpty) map { segment =>
      if (segment.startsWith("{")) {
        val name = segment.drop(1).dropRight(1)
        val param = parameters.find(_.name == name)
        param.fold(throw new Error("URLParam not found!")) { p =>
          p.schema match {
            case st: OpenapiSchemaSimpleType =>
              val (t, _) = mapSchemaSimpleTypeToType(st)
              val desc = p.description.fold("")(d => s""".description("$d")""")
              s"""path[$t]("$name")$desc"""
            case _ => throw new NotImplementedError("Can't create non-simple params to url yet")
          }
        }
      } else {
        '"' + segment + '"'
      }
    }
    ".in((" + inPath.mkString(" / ") + "))"
  }

  private def ins(ref: GeneratedClasses, parameters: Seq[OpenapiParameter], requestBody: Option[OpenapiRequestBody]): String = {
    //.in(query[Limit]("limit").description("Maximum number of books to retrieve"))
    //.in(header[AuthToken]("X-Auth-Token"))
    val params = parameters
      .filter(_.in != "path")
      .map { param =>
        param.schema match {
          case st: OpenapiSchemaSimpleType =>
            val (t, _) = mapSchemaSimpleTypeToType(st)
            val desc = param.description.fold("")(d => s""".description("$d")""")
            s""".in(${param.in}[$t]("${param.name}")$desc)"""
          case _ => throw new NotImplementedError("Can't create non-simple params to input")
        }
      }
      .mkString("\n")

    val rqBody = requestBody.fold("") { b =>
      if (b.content.size != 1) throw new NotImplementedError("We can handle only one requestBody content!")
      s"\n.in(${contentTypeMapper(ref, b.content.head.contentType, b.content.head.schema, b.required)})"
    }

    params + rqBody
  }

  private def outs(ref: GeneratedClasses, responses: Seq[OpenapiResponse]) = {
    //.errorOut(stringBody)
    //.out(jsonBody[List[Book]])
    responses
      .map { resp =>
        resp.code match {
          case "200" =>
            val content = resp.content.headOption.getOrElse(OpenapiModels.OpenapiResponseContent("text/plain", OpenapiSchemaType.OpenapiSchemaString(true)))
            s".out(${contentTypeMapper(ref, content.contentType, content.schema)})"
          case _ =>
            val content = resp.content.headOption.getOrElse(OpenapiModels.OpenapiResponseContent("text/plain", OpenapiSchemaType.OpenapiSchemaString(true)))
            s".errorOut(${contentTypeMapper(ref, content.contentType, content.schema)})"
        }
      }
      .sorted
      .mkString("\n")
  }

  private def contentTypeMapper(ref: GeneratedClasses, contentType: String, schema: OpenapiSchemaType, required: Boolean = true) = {
    contentType match {
      case "text/plain" =>
        "stringBody"
      case "application/json" =>
        val outT = schema match {
          case OpenapiSchemaType.OpenapiSchemaRef(name) => getObjectType(name, ref)
          case st: OpenapiSchemaSimpleType =>
            val (t, _) = mapSchemaSimpleTypeToType(st)
            t
          case OpenapiSchemaArray(st: OpenapiSchemaSimpleType, _) =>
            val (t, _) = mapSchemaSimpleTypeToType(st)
            s"List[$t]"
          case e => throw new NotImplementedError(s"Can't create non-simple or array params as output $e")
        }
        val req = if (required) outT else s"Option[$outT]"
        s"jsonBody[$req]"
      case _ => throw new NotImplementedError("We only handle json and text!")
    }
  }

  private def getObjectType(refName: String, ref: GeneratedClasses): String = {
    ref.map
      .get(ComponentName(refName.stripPrefix("#/components/schemas/")))
      .getOrElse(throw new RuntimeException(s"Can't find ref $refName"))
      .value
  }
}
