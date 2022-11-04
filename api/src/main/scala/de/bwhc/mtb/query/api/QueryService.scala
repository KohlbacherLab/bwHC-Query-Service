package de.bwhc.mtb.query.api


import scala.util.Either
import scala.concurrent.{
  ExecutionContext,
  Future
}
import de.bwhc.util.spi._
import de.bwhc.mtb.data.entry.impl.{
  QueryServiceProxy,
  QueryServiceProxyProvider
}


trait QueryService
extends DataOps
   with QueryOps
   with QCReportingOps
   with PeerToPeerOps
   with StatusOps
   with QueryServiceProxy
{
  self =>

  def process(
    cmd: QueryServiceProxy.Command
  )(
    implicit ec: ExecutionContext
  ): Future[Either[String,QueryServiceProxy.Response]] = {

    import QueryServiceProxy.Command._
    import QueryServiceProxy.Response._

    cmd match {

      case Upload(mtbfile) => {

        for {
          r <- self ! DataOps.Command.Upload(mtbfile)
        } yield r.map(_ => Imported)

      }

      case Delete(patId) => {

        for {
          r <- self ! DataOps.Command.Delete(patId)
        } yield r.map(_ => Deleted)

      }

    }

  }

}


trait QueryServiceProvider extends SPI[QueryService]

object QueryService extends SPILoader[QueryServiceProvider]


class QueryServiceProxyProviderImpl extends QueryServiceProxyProvider
{
  def getInstance: QueryServiceProxy = {
    QueryService.getInstance.get
  }
}
