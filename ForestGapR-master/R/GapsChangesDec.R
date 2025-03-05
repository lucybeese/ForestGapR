#' Forest Gaps Change Detection
#'
#' @description This function detects forest canopy gap changes across two forest gap [`terra::SpatRaster-class`] objects
#'
#' @usage GapChangeDec(gap_layer1,gap_layer2)
#'
#' @param gap_layer1 ALS-derived gap as an [`terra::SpatRaster-class`] object at time 1. (output of  [getForestGaps()] function).
#' @param gap_layer2 ALS-derived gap as an [`terra::SpatRaster-class`] object at time 2. (output of  [getForestGaps()] function).
#' @return A [`terra::SpatRaster-class`] object representing forest gap change area
#' @author Carlos Alberto Silva and Lucy Beese.
#'
#' @examples
#' \dontrun{
#' # Loading terra and viridis libraries
#' library(terra)
#' library(viridis)
#' 
#'
#' # ALS-derived CHM from Fazenda Cauxi - Brazilian tropical forest
#' data(ALS_CHM_CAU_2012)
#' data(ALS_CHM_CAU_2014)
#'
#' # set height thresholds (e.g. 10 meters)
#' threshold <- 10
#' size <- c(1, 10^4) # m2
#'
#' # Detecting forest gaps
#' gaps_cau2012 <- getForestGaps(chm_layer = ALS_CHM_CAU_2012, threshold = threshold, size = size)
#' gaps_cau2014 <- getForestGaps(chm_layer = ALS_CHM_CAU_2014, threshold = threshold, size = size)
#'
#' # Detecting forest gaps changes
#' Gap_changes <- GapChangeDec(gap_layer1 = gaps_cau2012, gap_layer2 = gaps_cau2014)
#'
#' # Plotting ALS-derived CHM and forest gaps
#' oldpar <- par(mfrow = c(1, 3))
#' plot(ALS_CHM_CAU_2012, main = "Forest Canopy Gap - 2012", col = viridis(10))
#' plot(gaps_cau2012, add = TRUE, col = "red", legend = FALSE)
#'
#' plot(ALS_CHM_CAU_2014, main = "Forest Canopy Gap - 2014", col = viridis(10))
#' plot(gaps_cau2014, add = TRUE, col = "red", legend = FALSE)
#'
#' plot(ALS_CHM_CAU_2014, main = "Forest Gap Changes Detected", col = viridis(10))
#' plot(Gap_changes, add = TRUE, col = "orange", legend = FALSE)
#' par(oldpar)
#' }
#' @import igraph
#' @export
GapChangeDec <- function(gap_layer1, gap_layer2) {
  
  gap1_binary <- !is.na(gap_layer1)  
  gap2_binary <- !is.na(gap_layer2)  
  
  gap_change <- gap_layer1
  gap_change[] <- NA  
  
  gap_change[gap1_binary & gap2_binary] <- 2  # Persistent Gaps (remained a gap)
  gap_change[!gap1_binary & gap2_binary] <- 1  # New Gaps (previously not a gap)
  gap_change[gap1_binary & !gap2_binary] <- 3  # Closed Gaps (was a gap, now not a gap)
  
  gap_change[!gap1_binary & !gap2_binary] <- NA  # No gap in either year
  
  return(gap_change)
}