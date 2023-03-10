package de.bwhc.mtb.query.api



import java.time.LocalDateTime
import scala.util.Either
import scala.concurrent.{
  Future,
  ExecutionContext
}
import cats.data.IorNel
import play.api.libs.json._
import de.bwhc.mtb.data.entry.dtos.
{ 
  MTBFile,
  ZPM
}


trait QCReportingOps
{

  // Map[String,String] used as placeholder parameter
  // because using None leads to JSON serializer resolution problems
  def getLocalQCReport(
    request: PeerToPeerRequest[Map[String,String]] 
  )(
    implicit ec: ExecutionContext
  ): Future[Either[String,LocalQCReport]]


  def compileGlobalQCReport(
    implicit
    querier: Querier,
    ec: ExecutionContext
  ): Future[IorNel[String,GlobalQCReport]]

}
