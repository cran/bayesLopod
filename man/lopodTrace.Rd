% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lopodTrace.R
\name{lopodTrace}
\alias{lopodTrace}
\title{Plots the values of model parameters for each chain across iterations}
\usage{
lopodTrace(LopodModel, params = "lp__", inc_warmup = FALSE)
}
\arguments{
\item{LopodModel}{A LopodModel object}

\item{params}{Parameters to be plotted. Default is "lp__" which plots the log posterior probability}

\item{inc_warmup}{Boolean. If true, warm-up iterations are plotted. Default is FALSE.}
}
\value{
A ggplot object.
}
\description{
Plots the values of model parameters for each chain across iterations
}
\examples{

\dontrun{
data("simSpRecords", package = "bayesLopod")
data("simSpSamplingEffort", package = "bayesLopod")
simSpRasters = xyToRaster(xyRecords = simSpRecords,xySamplingEffort = simSpSamplingEffort,
basemap = NULL, nrows = 50, extentExpansion = 0)
ld_Raster_adMatrix = rasterLopodData(rasterN = simSpRasters[["samplingEffort"]],
rasterY = simSpRasters[["spDetections"]], Adjacency = TRUE )
mLopodRaster = modelLopod(LopodData = ld_Raster_adMatrix, varP = TRUE, q = NULL,
pmin = 0.1, CAR = FALSE, nChains = 4,warmup = 500,sampling = 100,nCores = 4)
lopodTrace(mLopodRaster, inc_warmup = FALSE, params = c("p","q"))

data("Andropogon_shape", package = "bayesLopod")
ld_Shape = shapeLopodData(Shapefile = Andropogon_shape, fieldN = "sampEffort",
fieldY = "detections",  Adjacency = TRUE, keepFields = FALSE)
mLopodShape = modelLopod(LopodData = ld_Shape, varP = TRUE, q = NULL,
pmin = 0, CAR = TRUE, nChains = 4,warmup = 500,sampling = 100,nCores =4)
lopodTrace(mLopodShape, inc_warmup = TRUE)
}
}
