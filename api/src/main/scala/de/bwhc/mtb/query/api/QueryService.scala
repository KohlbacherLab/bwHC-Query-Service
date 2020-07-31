package de.bwhc.mtb.query.api



import de.bwhc.util.spi._



trait QueryService
extends DataOps
   with QueryOps
   with QCReportingOps


trait QueryServiceProvider extends SPI[QueryService]

object QueryService extends SPILoader(classOf[QueryServiceProvider])
