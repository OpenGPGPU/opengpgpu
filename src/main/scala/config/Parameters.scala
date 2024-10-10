package ogpu.config

import org.chipsalliance.cde.config._

case object XLen extends Field[Int]
case object DTSModel extends Field[String]
case object DTSCompat extends Field[String]
case object ThreadNum extends Field[Int]
case object WarpNum extends Field[Int]
case object RegNum extends Field[Int]
case object RegIDWidth extends Field[Int]
case object WarpIDWidth extends Field[Int]
case object AddrWidth extends Field[Int]
case object StackDepth extends Field[Int]
case object DimWidth extends Field[Int]
