package com.twitter.scrooge.csharp_generator

import com.twitter.scrooge.ast.{Field, Identifier}

class StructFieldController(
    f: Field,
    val i: Int,
    total: Int,
    generator: CsharpGenerator,
    ns: Option[Identifier],
    serializePrefix: String)
  extends BaseController(generator, ns) {
  val field = new FieldController(f, generator, ns)
  val fieldType = f.fieldType
  val field_type = field.field_type
  val optional_or_nullable = field.optional || field_type.nullable
  val required_nullable = field.required && field_type.nullable
  val name = f.sid.name
  val deepCopyIndentLevel = if (field_type.nullable) 6 else 4
  val generate_deep_copy_container =
    indent(generator.deepCopyContainer("other_" + name, "", "__this__" + name, fieldType, ns), 4)
  val generate_deep_copy_non_container = generator.deepCopyNonContainer("other_" + name, fieldType, ns)
  val key = f.index
  val field_metadata =  indent(generator.fieldValueMetaData(fieldType, ns), 6, addLast = false) 
  def deserialize_field = indent(generator.deserializeField(fieldType, name, ns, field.optional, serializePrefix), 12)
  def serialize_field = indent(generator.serializeField(fieldType, name, ns, serializePrefix), 4)
  def print_const =
    indent(generator.printConstValue("this." + name, fieldType, f.default.get, ns, in_static = true, defval = true), 4)
  def last = i == total - 1
  def first = i == 0
}
