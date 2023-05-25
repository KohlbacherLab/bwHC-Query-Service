package de.bwhc.mtb.query.api


import scala.util.Either
import scala.concurrent.{
  ExecutionContext,
  Future
}
import de.bwhc.util.spi.{
  SPI,
  SPILoader
}

trait QueryService
extends DataOps
   with QueryOps
   with PreparedQueryOps
   with QCReportingOps
   with TherapyReportingOps
   with PeerToPeerOps
   with StatusOps


trait QueryServiceProvider extends SPI[QueryService]

object QueryService extends SPILoader[QueryServiceProvider]

