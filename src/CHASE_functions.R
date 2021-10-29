# help functions for the CHASE assessment

#============== function ContaminationRatio ================================
# 
ContaminationRatio <- function(threshold, status, response = 1) {
  # If response is not specified, it will be assumed to be positive
  # i.e. ContaminationRatio increases (worsens) with increasing status value
  if (missing(response)) {
    response = 1
  }
  response <- ifelse(is.na(response), 1, response)
  
  # ContaminationRatio calculated depending on Response direction
  cr <- ifelse(response > 0, status / threshold, threshold / status)
  return(cr)
}

#============== function CHASEStatus =========================================

CHASEStatus <- function(CRsum, nCat = 5) {
  if (nCat == 5) {
    status <- ifelse(CRsum > 0.5, "Good", "High")
    status <- ifelse(CRsum > 1, "Moderate", status)
    status <- ifelse(CRsum > 5, "Poor", status)
    status <- ifelse(CRsum > 10, "Bad", status)
  } else{
    status <- ifelse(CRsum > 1, "Not good", "Good")
  }
  return(status)
}

CHASEStatus1 <- function(CRsum) {
  status <- ifelse(CRsum > 0.5, "Good", "High")
  status <- ifelse(CRsum > 1, "Moderate", status)
  status <- ifelse(CRsum > 5, "Poor", status)
  status <- ifelse(CRsum > 10, "Bad", status)
  return(status)
}

# Colours associated with Status classes - used by Shiny App
AddColours <- function(CRsum) {
  co <- ifelse(CRsum > 0.5, '#66FF66', '#3399FF')
  co <- ifelse(CRsum > 1, '#FFFF66', co)
  co <- ifelse(CRsum > 5, '#FF9933', co)
  co <- ifelse(CRsum > 10, '#FF6600', co)
  return(co)
}

#============== function SpatialConfidence ============================================================
SpatialConfidence <- function(GridCount,GridsAssessed,nStations,Area_km2){

  categories <- c("H", "M", "L")
  GridsPerSampleBounds<-c(10,50)
  km2perSampleBounds<-c(500,5000)
  
  # calculate the spatial parameters from number of grids, number of grids assessed, etc
  GridsPerSample <-  GridCount / nStations
  km2perSample <- Area_km2 / nStations
  
  # return the confidence indexes (1 - 5) for the three parameters
  ConfIndexSamples <- ConfIndex(GridsPerSample,GridsPerSampleBounds)
  ConfIndexKm2 <- ConfIndex(km2perSample,km2perSampleBounds)
  
  # take the average of the three indices
  ConfIndexAvg <- (ConfIndexSamples + ConfIndexKm2)/2
  ConfIndexAvg <- as.integer(round(ConfIndexAvg,digits=0))

  return(categories[ConfIndexAvg])
}

SpatialConfidence5 <- function(GridCount,GridsAssessed,nStations,Area_km2){
  
  categories <- c("High", "Moderate", "Mid", "Low", "Very low")
    
    # limit values for spatial confidence parameters
    #fractionGridsBounds<-c(0.5,0.4,0.3,0.2)
  GridsPerSampleBounds<-c(5,20,50,80)
  km2perSampleBounds<-c(250,1000,4000,8000)
  
  # calculate the spatial parameters from number of grids, number of grids assessed, etc
  #fractionGrids <- GridsAssessed / GridCount (this is essentially the inverse of GridsPerSample)
  GridsPerSample <-  GridCount / nStations
  km2perSample <- Area_km2 / nStations
  
  # return the confidence indexes (1 - 5) for the three parameters
  # ConfIndexGrids <- ConfIndex(fractionGrids,fractionGridsBounds,descending=T)
  ConfIndexSamples <- ConfIndex(GridsPerSample,GridsPerSampleBounds)
  ConfIndexKm2 <- ConfIndex(km2perSample,km2perSampleBounds)
  
  # take the average of the three indices
  ConfIndexAvg <- (ConfIndexSamples + ConfIndexKm2)/2
  ConfIndexAvg <- as.integer(round(ConfIndexAvg,digits=0))
  
  return(categories[ConfIndexAvg])
}

ConfIndex<-function(value,limits,descending=F){
  if(is.na(value)){
    return(ncat)
  }else{
    if(descending){
      ix<-length(limits[limits>value])
      }else{
        ix<-length(limits[limits<=value])
        }
    return(ix+1)
  }
}


#============== function ConfValue ============================================================

# Function to calculate numeric confidence from string
# between 0 and 1, depending on the argument sConf
# Given a numeric argument between 0 and 1, the function
# returns the same value
# e.g. ConfValue(0.37) returns a value of 0.37
# Passing a numeric argument less than 0, the function
# returns a value of 0
# Passing a numeric argument greater than 1, the function 
# returns a value of 1
# 
# The function recognizes the following words and returns 
# the respective values:
#    High = 1.0
#    Intermediate = 0.5
#    Medium = 0.5
#    Moderate = 0.5
#    Low = 0.0
# 
# The function is case-insensitive.
# Starting from the leftmost character, the function 
# recognizes any part of the key words
#
# e.g. "H", "hi", "hig" will all result in a value of 1.0
#      "med", "m", "int", "I", "in" will all return values of 0.5
#      "lo", "l" will all give a value of 0.0
#
# Any other argument passed to the function will give a result 
# equal to the argument NAvalue (default=0)

ConfValue <- function(sConf, ncat=3, NAvalue = NA) {
  if (is.numeric(sConf)) {
    return(sConf)
  } else{
    sConf <-str_to_lower(str_trim(sConf))
    l <- nchar(sConf)
    if(l<1) {
      return(NAvalue)
    } else{
      if(ncat==5){
        desc <- c("very low","v.low","v. low","low","mid","medium","moderate","high")
        value <- c(0, 0, 0, 0.25, 0.5, 0.5, 0.75, 1)
      }else{
        desc <- c("low","intermediate","medium","moderate","high")
        value <- c(0, 0.5, 0.5, 0.5, 1)
      }
      desc <- substr(desc,1,l)
      nc <- match(sConf,desc)
      nc <- value[nc]
      return(nc)
      }
    }
  }

#============== function ConfidenceStatus ===================================
# convert a numeric confidence score to Class I, II or III

ConfidenceStatus <- function(Score, Roman = FALSE) {
  Status <- ifelse(
    Roman == TRUE,
    ifelse(
      Score < 0.5,
      "Class III",
      ifelse(Score < 0.75, "Class II", "Class I")
    ),
    ifelse(Score < 0.5, "Low",
           ifelse(Score < 0.75, "Medium", "High"))
  )
  return(Status)
}


#============== function IsHeavyMetal ===================================
IsHeavyMetal <- function(sType, NAvalue = NA) {
  n <- NAvalue
  if (regexpr("hm", sType, ignore.case = TRUE) > 0) {
    n <- 1
  }
  if (regexpr("heavy", sType, ignore.case = TRUE) > 0) {
    n <- 1
  }
  return(n)
}

#============== function IsOrganic ===================================
IsOrganic <- function(sType, NAvalue = NA) {
  n <- NAvalue
  if (regexpr("organ", sType, ignore.case = TRUE) > 0) {
    n <- 1
  }
  if (regexpr("org", sType, ignore.case = TRUE) > 0) {
    n <- 1
  }
  return(n)
}
