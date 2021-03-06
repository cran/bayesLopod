% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xyToRaster.R
\name{xyToRaster}
\alias{xyToRaster}
\title{Crate Shape object for a parameter estimated in a LopodModel}
\usage{
xyToRaster(xyRecords, xySamplingEffort, nrows = 50, extentExpansion = 0.1,
  extent = NULL, basemap = getData("worldclim", var = "alt", res = 10))
}
\arguments{
\item{xyRecords}{Object of class SpatialPoints or SpatialPointsDataFrame with the locality-records of the species.}

\item{xySamplingEffort}{Object of class SpatialPoints or SpatialPointsDataFrame with the coordinates of all sampling events (including those in which the species was found).}

\item{nrows}{Number of rows that the final Raster will have. It will be use to determine its resolution.}

\item{extentExpansion}{Factor by which the final extent should be expended. If 0 and extent is NULL, the final extent will be determined by the most extreme locality records. If extent is given, extentExpansion will be ignored.}

\item{extent}{Object of class Extent delimiting the region to be included in the final raster}

\item{basemap}{Object of class Raster in which cell with NA values will not be included. If NULL, all cells in the raster will be included.}
}
\value{
A Stack object with two layers: one for the number of records per cell ("samplingEffort"), and another one with the number of sampling events ("spDetections").
}
\description{
Crate Shape object for a parameter estimated in a LopodModel
}
\examples{
data("simSpRecords", package = "bayesLopod")
data("simSpSamplingEffort", package = "bayesLopod")
simSpRasters = xyToRaster(xyRecords = simSpRecords,xySamplingEffort = simSpSamplingEffort,
basemap = NULL, nrows = 50, extentExpansion = 0)
}
