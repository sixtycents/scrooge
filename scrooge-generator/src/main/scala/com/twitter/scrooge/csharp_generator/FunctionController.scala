package com.twitter.scrooge.csharp_generator

import com.twitter.scrooge.ast.{Function => TFunction, _}
import com.twitter.scrooge.ast.Field
import com.twitter.scrooge.ast.SimpleID
import com.twitter.scrooge.ast.Struct

class FunctionController(function: TFunction, generator: CsharpGenerator, ns: Option[Identifier])
  extends BaseController(generator, ns) {
  val return_type = new FieldTypeController(function.funcType, generator)
  val name = function.funcName.name.capitalize
  val argument_list = function.args map { a =>
    a.sid.name
  } mkString ", "
  val argument_list_with_types = function.args map { a =>
    generator.typeNameOption(a) + " " + a.sid.name
  } mkString ", "
  val argument_list_with_args = function.args map { a =>
    "args." + a.sid.name.capitalize
  } mkString ", "

  val has_args = function.args.size > 0
  val fields = function.args map { a =>
    new FieldController(a, generator, ns)
  }
  val exceptions_string = {
    val exceptions = function.throws map (a => generator.typeName(a.fieldType))
    if (exceptions.size > 0) {
      exceptions.mkString(", ") + ", "
    } else {
      ""
    }
  }

  val exceptions = function.throws.zipWithIndex map {
    case (e, i) => new FieldController(e, generator, ns) {
      val first = i == 0
    }
  }

  val has_exceptions = exceptions.size > 0

  val is_oneway = function.funcType == OnewayVoid
  val is_oneway_or_void = is_oneway || return_type.is_void

  def i_if_has_exceptions = newHelper { input =>
    if (exceptions.size > 0) indent(input, 4, false) else input
  }

  def arg_struct = {
    val args = function.args map { a =>
      //val requiredness = if (a.requiredness.isRequired) Requiredness.Required else Requiredness.Default
      Field(a.index, a.sid, a.originalName, a.fieldType, a.default, a.requiredness)
    }
    val structName = function.funcName.name.capitalize + "_args"
    val struct = Struct(SimpleID(structName), structName, args, function.docstring, Map.empty)
    val controller = new StructController(struct, true, generator, ns)
    generator.renderMustache("struct_inner.mustache", controller)
  }

  def result_struct = {
    val fields = (if (function.funcType == Void) {
      Seq()
    } else {
      val fieldType = function.funcType.asInstanceOf[FieldType]
      Seq(Field(0, SimpleID("success"), "success", fieldType, None, Requiredness.Optional))
    }) ++ function.throws.map {
      _.copy(requiredness = Requiredness.Optional)
    }
    val struct = Struct(SimpleID(function.funcName.name.capitalize + "_result"), function.originalName + "_result", fields, None, Map.empty)
    val controller = new StructController(struct, true, generator, ns, is_result = true)
    generator.renderMustache("struct_inner.mustache", controller)
  }
}
