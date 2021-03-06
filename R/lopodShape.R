#' Crate Shape object for a parameter estimated in a LopodModel
#'
#' @param LopodModel A LopodModel object
#' @param param Unit-level model parameter to be mapped. Values "psi_Sampled" can be mapped for models without CAR analyses, "psi_i" for LopodModels with CAR analysis and "pp","cellpres_i", "pCorr","sim_y","sim_true_y","sim_false_y" for both.
#' @param extrapolate Boolean. If True, parameters are mapped for cells that have not been sampled, this can only be done in LopodModels with CAR analysis. Only plotted for "psi_i", "pp" and, "cellpres_i".
#' @param metric "mean" or "sd". Plots the mean or standard deviation of the posterior distribution. If NULL, the value in quant is used.
#' @param quant Returns the raster for a given quantile of the Posterior Distribution. Default is 0.5 (the median of the posterior distribution). Not used if metric is other than NULL
#' @export
#' @return A Raster object.
#' @examples
#' \dontrun{
#' data("Andropogon_shape", package = "bayesLopod")
#' ld_Shape = shapeLopodData(Shapefile = Andropogon_shape, fieldN = "sampEffort",
#' fieldY = "detections",  Adjacency = TRUE, keepFields = FALSE)
#' mLopodShape = modelLopod(LopodData = ld_Shape, varP = TRUE, q = NULL,
#' pmin = 0, CAR = TRUE, nChains = 4,warmup = 500,sampling = 100,nCores =4)
#' psiShape_95 = lopodShape(mLopodShape, "psi_i", extrapolate = FALSE,  quant = 0.95)
#' psiShape_05 = lopodShape(mLopodShape, "psi_i", extrapolate = TRUE, quant = 0.05)
#'  #Visualize results
#' sp::spplot(psiShape_05, zcol = "psi_i")
#' sp::spplot(psiShape_95, zcol = "psi_i")
#' }




lopodShape =  function(LopodModel,param,extrapolate=T, metric = NULL, quant=0.5){

  if (is.null(metric)){
    columnName=paste(quant*100,"%", sep="")
    probs = quant

  }
  if (is.null(metric)==F){
    if((metric %in% c("mean", "sd"))==F)stop("metric can only be mean or sd")

    columnName=metric
    probs = NULL

    message(paste(metric, "will be returned. Value in quant (if any) will be ignored"))

  }


  FeatureName= param


  if(class(LopodModel) != "LopodModel") stop("Obeject needs to be a LopdModel")
  if(length(param)>1) stop("Only one parameter can be retuned at the time")
  if(LopodModel@LopodData@geoType != "Shapefile") stop("Data is not in a Shapefile format")

  finalShapefile = LopodModel@LopodData@geoDataObject
  finalShapefile@data[,FeatureName] = NA


  modelPar = modelParams(LopodModel)

  if(extrapolate==T){
    if(LopodModel@modelInfo$CAR==F) stop("Only CAR models can be extrapolated to not sampled units")

    if((param %in% modelPar$allCellsPars)==F) stop(paste("For this model only the following  parameters can be extrapolated into not sampled units:",toString(modelPar$allCellsPars)))


    CellsID = rbind(LopodModel@LopodData@geoInfo$sampledId,LopodModel@LopodData@geoInfo$notSampledId)
    ParObjects=paste(param,"[",CellsID$cellStan,"]",sep="")

    ParValues = rstan::summary(LopodModel@StanFit,pars=ParObjects,probs=probs, use_cache=FALSE)$summary[,columnName]
    finalShapefile@data[CellsID$featureShape,FeatureName] = ParValues



    }

  if(extrapolate==F){
  if((param %in% c(modelPar$allCellsPars,modelPar$sampledPars))==F) stop(paste("For this model only the following  parameters can be mapped:",toString(c(modelPar$allCellsPars,modelPar$sampledPars))))

    CellsID = LopodModel@LopodData@geoInfo$sampledId
    ParObjects=paste(param,"[",CellsID$cellStan,"]",sep="")


    if ((is.null(LopodModel@LopodData@geoInfo$W_sparse)==F)&(LopodModel@modelInfo$CAR==F)){

   ParObjects=paste(param,"[",1:dim(CellsID)[1],"]",sep="")

   }


  ParValues = rstan::summary(LopodModel@StanFit,pars=ParObjects,probs=probs, use_cache=FALSE)$summary[,columnName]
  finalShapefile@data[CellsID$featureShape,FeatureName] = ParValues


  }

  return(finalShapefile)

}
