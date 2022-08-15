package edg_ide.dse

import edgir.schema.schema
import edgrpc.hdl.{hdl => edgrpc}



case class DseResult(input: edgrpc.Refinements, result: schema.Design)
