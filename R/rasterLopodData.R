#' Create a LopodData object from Raster data
#' @importFrom raster raster stack extent ncell distanceFromPoints distance xyFromCell adjacent
#' @importFrom slam  as.simple_triplet_matrix simple_triplet_diag_matrix crossprod_simple_triplet_matrix
#' @importFrom  rgeos gTouches
#' @param rasterN Raster object with sampling effort (number of sampling events)in each cell.
#' @param rasterY Raster object with number of detections in each cell.
#' @param Adjacency Boolean. If TRUE, and adjacency matrix is computed.
#' @param extSample Number between 0 and 1. Maximum distance (relative to the diagonal of the raster) from a sampled cell that should be included in the study area. If 0, there is no extrapolation to not sampled cells.
#' @param extDetection Number between 0 and 1. Maximum distance (relative to the diagonal of the raster) from cell in which the species was detected that should be included in the study area. If 0, there is no extrapolation to not sampled cells.
#' @export
#' @return A LopodData object to be used in modelLopod.
#' @examples
#' data("simSpRecords", package = "bayesLopod")
#' data("simSpSamplingEffort", package = "bayesLopod")
#' simSpRasters = xyToRaster(xyRecords = simSpRecords,xySamplingEffort = simSpSamplingEffort,
#' basemap = NULL, nrows = 10, extentExpansion = 0)
#' ld_Raster = rasterLopodData(rasterN = simSpRasters[["samplingEffort"]],
#' rasterY = simSpRasters[["spDetections"]], Adjacency = FALSE )

#' \dontrun{
#' data("simSpRecords", package = "bayesLopod")
#' data("simSpSamplingEffort", package = "bayesLopod")
#' simSpRasters = xyToRaster(xyRecords = simSpRecords,xySamplingEffort = simSpSamplingEffort,
#' basemap = NULL, nrows = 50, extentExpansion = 0)
#' ld_Raster_adMatrix = rasterLopodData(rasterN = simSpRasters[["samplingEffort"]],
#' rasterY = simSpRasters[["spDetections"]], Adjacency = TRUE )
#' }



rasterLopodData = function(rasterN, rasterY,Adjacency = T, extSample = 0.025, extDetection = 0.15){

rastery = rasterY

if (extent(rasterN) != extent(rasterY) | ncell(rasterN) != ncell(rasterY)){
  stop ("Raster for sampling effort and for detections should have the same extent and resolution")

}

  if (min((rasterN - rasterY)[], na.rm=T)<0 ){
    stop ("Sampling effort must always be grater than number of detections")

  }

  if ( extSample>1 |extSample<0 |  extDetection>1 |extDetection<0 ){
    stop ("The extrapolation from sampled cells and those in which the species has been detected must be between 0 and 1, the value is relative to the maximum distance in the raster")

  }

maxDist = max(distanceFromPoints(rasterN,xyFromCell(rasterN,1))[])

if (Adjacency){

  maxExtDistSample = maxDist * extSample


} else {

  maxExtDistSample = 0
  if (extSample != 0){
    message ("Extrapolation into not-sampled cells cannot be performed without an adjacency matrix ")

  }


}

maxExtDistDect = maxDist * extDetection

baseNA = rasterN
baseNA[] = 1
baseNA[is.na(rasterN[])] = NA

DistSample = rasterN
DistSample[] = NA
DistSample[rasterN[]>0] = 1
if(sum(rasterN[]==0, na.rm = T)==0){

DistSample[] = 0

}else{

DistSample = distance(DistSample)

}

DistSample[DistSample[]>maxExtDistSample] = NA
DistSample[DistSample[]<=maxExtDistSample] = 1

DistDetec = rasterY
DistDetec[] = NA
DistDetec[rasterY[]>0] = 1

if(sum(rasterY[]==0, na.rm = T)==0){

  DistDetec[] = 0

}else{

  DistDetec = distance(DistDetec)

}

DistDetec[DistDetec[]>maxExtDistDect] = NA
DistDetec[DistDetec[]<=maxExtDistDect] = 1

StudyArea = DistDetec*DistSample*baseNA
rasterN = rasterN*StudyArea
rastery = rasterY*StudyArea

#Which cells have been sampled or not sampled
whichSampledCells = which(rasterN[]>0)

whichNotSampledCells = which(rasterN[]==0)

whichNoNACells = which(is.na(rasterN[]) == F)
if(sum(is.na(rasterN[])) > 0) message(paste(sum(is.na(rasterN[])),"cells are NA - Dropped from analysis"))

geoDataObject = stack(StudyArea,rasterN,rastery)
names(geoDataObject) = c("studyArea", "samplingEffort", "Detections")

if (Adjacency){


  #Adjacency Matrix

  AdMAtrix = matrix(0,ncol=length(whichNoNACells),nrow = length(whichNoNACells) )

  rownames(AdMAtrix)=as.character(whichNoNACells)
  colnames(AdMAtrix)=as.character(whichNoNACells)

  for ( i in 1:length(whichNoNACells)){

    adCells = as.character(adjacent(rasterN,cells = whichNoNACells[i], pairs = F, directions = 8, target = whichNoNACells ))
    AdMAtrix[as.character(whichNoNACells[i]),adCells] = 1
  }


  noNeighboursCells = which(colSums(AdMAtrix)==0)
  noNeighboursCells = noNeighboursCells[names(noNeighboursCells)]

  if (length(noNeighboursCells) >0 ){
    AdMAtrix = AdMAtrix[-noNeighboursCells,]
    AdMAtrix = AdMAtrix[,-noNeighboursCells]
  }

  if(length(noNeighboursCells) > 0) message(paste(length(noNeighboursCells),"cells have no neighbors - Dropped from analysis"))


  nPairs = sum(AdMAtrix)/2

  sampledId = match(as.character(whichSampledCells),colnames(AdMAtrix))
  sampledId = data.frame(cellRaster = whichSampledCells, cellStan = sampledId )
  whichIslandList=which(sampledId[,"cellRaster"]%in%as.numeric(names(noNeighboursCells)))

  if (length(whichIslandList) >0 ){
    sampledId = sampledId[-whichIslandList,]
  }


  notSampledId = match(as.character(whichNotSampledCells),colnames(AdMAtrix))
  notSampledId = data.frame(cellRaster = whichNotSampledCells, cellStan = notSampledId)
  whichIslandList=which(notSampledId[,"cellRaster"]%in%as.numeric(names(noNeighboursCells)))

  if (length(whichIslandList) >0 ){
    notSampledId = notSampledId[-whichIslandList,]
  }


  AllCellsId = rbind(sampledId,notSampledId)
  AllCellsId = AllCellsId[order(AllCellsId[,"cellStan"]),]

  n = length(sampledId[,"cellStan"])+length(notSampledId[,"cellStan"])


  ##Sparse representation of ad Matrix for Stan


  W_sparse =  matrix(0,nrow = nPairs, ncol = 2)

  counter = 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (AdMAtrix[i, j] == 1) {
        W_sparse[counter, 1] = i;
        W_sparse[counter, 2] = j;
        counter = counter + 1;
      }
    }
  }

  D_sparse = rowSums(AdMAtrix)


  w_sparse_mat= as.simple_triplet_matrix(AdMAtrix)
  invsqrtD_SparseDiag = simple_triplet_diag_matrix(1 / sqrt(D_sparse))

  quadMatrix_sparse = crossprod_simple_triplet_matrix(crossprod_simple_triplet_matrix(w_sparse_mat,invsqrtD_SparseDiag),invsqrtD_SparseDiag)
  lambda_sparse = eigen(quadMatrix_sparse,only.values = T)

  geoInfo = list(sampledId=sampledId,notSampledId=notSampledId,W_sparse=W_sparse,D_sparse=D_sparse,lambda_sparse=lambda_sparse$values)

} else {

  sampledId = data.frame("cellRaster" = whichSampledCells, cellStan = 1:length(whichSampledCells))

  geoInfo = list(sampledId=sampledId)

}

return(LopodData_Class (geoDataObject = geoDataObject, geoInfo = geoInfo, geoType = "Raster" ))



}
