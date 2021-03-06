% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rasterLopodData.R
\name{rasterLopodData}
\alias{rasterLopodData}
\title{Create a LopodData object from Raster data}
\usage{
rasterLopodData(rasterN, rasterY, Adjacency = T, extSample = 0.025,
  extDetection = 0.15)
}
\arguments{
\item{rasterN}{Raster object with sampling effort (number of sampling events)in each cell.}

\item{rasterY}{Raster object with number of detections in each cell.}

\item{Adjacency}{Boolean. If TRUE, and adjacency matrix is computed.}

\item{extSample}{Number between 0 and 1. Maximum distance (relative to the diagonal of the raster) from a sampled cell that should be included in the study area. If 0, there is no extrapolation to not sampled cells.}

\item{extDetection}{Number between 0 and 1. Maximum distance (relative to the diagonal of the raster) from cell in which the species was detected that should be included in the study area. If 0, there is no extrapolation to not sampled cells.}
}
\value{
A LopodData object to be used in modelLopod.
}
\description{
Create a LopodData object from Raster data
}
\examples{
data("simSpRecords", package = "bayesLopod")
data("simSpSamplingEffort", package = "bayesLopod")
simSpRasters = xyToRaster(xyRecords = simSpRecords,xySamplingEffort = simSpSamplingEffort,
basemap = NULL, nrows = 10, extentExpansion = 0)
ld_Raster = rasterLopodData(rasterN = simSpRasters[["samplingEffort"]],
rasterY = simSpRasters[["spDetections"]], Adjacency = FALSE )
\dontrun{
data("simSpRecords", package = "bayesLopod")
data("simSpSamplingEffort", package = "bayesLopod")
simSpRasters = xyToRaster(xyRecords = simSpRecords,xySamplingEffort = simSpSamplingEffort,
basemap = NULL, nrows = 50, extentExpansion = 0)
ld_Raster_adMatrix = rasterLopodData(rasterN = simSpRasters[["samplingEffort"]],
rasterY = simSpRasters[["spDetections"]], Adjacency = TRUE )
}
}
