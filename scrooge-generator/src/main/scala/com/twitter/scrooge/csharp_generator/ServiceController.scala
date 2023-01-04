package com.twitter.scrooge.csharp_generator

import com.twitter.scrooge.ast._
import com.twitter.scrooge.ast.Service

class ServiceController(service: Service, generator: CsharpGenerator, ns: Option[Identifier])
  extends TypeController(service, generator, ns) {
  val extends_iface = service.parent match {
    case Some(parent) => Map("name" -> generator.qualifyNamedType(parent.sid, parent.filename).fullName)
    case None => false
  }
  val functions = service.functions map { f => new FunctionController(f, generator, ns) }
}