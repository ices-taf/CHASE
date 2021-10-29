library("dplyr")
library("tidyr")

#===============================================================================
# function Assessment

# input:

#     assessmentdata (required): data frame containing the following columns


#     summarylevel: deault 
#     StatusClasses: number of status classes to be used 
#       CHASE status classes
#       the default is 5 (High, Good, Moderate, Poor, Bad)



CHASEassessment <- function(assessmentdata, 
                       summarylevel=NA,
                       StatusClasses=5,
                       ndigits=3) {
  
  
  # Confidence Penalty Criteria (minimum numbers of indicators)
  CountHM <- 2
  CountOrg <- 3
  PenaltyHM <- 0.5
  PenaltyOrg <- 0.5
  
  # relative weights of confidence components
  ConfWeightTemp = 0.1
  ConfWeightSpatial = 0.25
  ConfWeightAcc = 0.25
  ConfWeightMethod = 0.25
  ConfWeightThresh = 0.15
  
  # Get column names from the input data
  cnames <- names(assessmentdata)
  
  # list of required columns
  requiredcols <- c("Matrix","Substance","Type")
  
  # If CR is not included, then Threshold and Status are required columns
  if ('CR' %in% toupper(cnames)) {
    UserCR = TRUE
  } else{
    UserCR = FALSE
    requiredcols <- c(requiredcols,"Threshold","Status")
  }
  
  
  # If ConfSpatial is not included, then we need information to calculate Spatial confidence:
  # number of grids
  # number of grids assessed
  # number of stations
  # area km2
  
  if (toupper('ConfSpatial') %in% toupper(cnames)) {
    UserConfSpatial = TRUE
    extracolsConf <- c("ConfTemp","ConfSpatial",
                       "ConfAcc","ConfMethod","ConfThresh")
  } else{
    UserConfSpatial = FALSE
    requiredcols <- c(requiredcols,c("Grids","GridsAssessed","Stations","Area_km2"))
    extracolsConf <- c("ConfTemp","ConfMethod","ConfThresh")
                       
  }
  if (toupper('ConfAcc') %in% toupper(cnames)) {
    bConfAcc<-TRUE
    # user supplied ConfAcc
  }else{
    bConfAcc<-FALSE
  }

  # extra columns - if missing, values will be assumed for these columns
  #                 and we can still do the assessment
  extracols <- c("AU","Response",
                 extracolsConf)
  
  #Check column names in the imported data
  nimp = ncol(assessmentdata)
  nreq = length(requiredcols)
  nextra = length(extracols)
  
  ok <- rep(0, nreq)
  okextra <- rep(0, nextra)
  foundresponse = FALSE
  
  
  for (i in 1:nimp) {
    for (j in 1:nreq) {
      if (toupper(requiredcols[j]) == toupper(cnames[i])) {
        names(assessmentdata)[i] <- requiredcols[j]
        ok[j] = 1
      }
    }
    for (j in 1:nextra) {
      if (toupper(extracols[j]) == toupper(cnames[i])) {
        names(assessmentdata)[i] <- extracols[j]
        okextra[j] = 1
      }
    }
  }
  
  # if any of the optional columns are missing, add them to the dataframe
  for (j in 1:nextra) {
    if (okextra[j] == 0) {
      if (toupper(extracols[j]) %in% toupper(extracolsConf)) {
        assessmentdata[[extracols[j]]] <- 'L'
      } else{
        if(tolower(extracols[j])=="au"){
          assessmentdata[[extracols[j]]] <- "Whole assessment"
        }else{
          assessmentdata[[extracols[j]]] <- 1
        }
      }
    }
  }
  
  n <- sum(ok, na.rm = TRUE)
  
  if (n < nreq) {
    # Some of the required columns were not found in the input data
    message("Error in CHASE Assessment. Required column(s) were not found in the input data:")
    for (j in 1:nreq) {
      if (ok[j] != 1) {
        cat(paste0("    ", requiredcols[j]), "\n")
      }
    }
    return(NA)
  } else{
    # The required columns are present - do the assessment
    
    
    # Change order of matrices factors
    mat1 <- data.frame(unique(assessmentdata$Matrix))
    names(mat1)[1] <- 'Matrix'
    
    matorder<-c("w","s","b")
    # preferred order is water, sediment, biota
    
    mat1 <- mat1 %>%
      mutate(char=tolower(substr(as.character(Matrix),1,1))) %>%
      mutate(n=match(char,matorder)) %>%
      arrange(desc(n),desc(char))
    
    assessmentdata$Matrix <-
      factor(assessmentdata$Matrix, levels = mat1$Matrix)
    
    # All combinations of matrices and waterbodies
    # This is used later to ensure that a NA is 
    # returned where a combination is missing
    waterbodies <- unique(assessmentdata$AU)
    matrices <- unique(assessmentdata$Matrix)
    matrices <- expand.grid(waterbodies, matrices)
    names(matrices)[1] <- 'AU'
    names(matrices)[2] <- 'Matrix'
    
    if (UserCR == FALSE) {
      assessmentdata$CR <-
        ContaminationRatio(assessmentdata$Threshold,
                           assessmentdata$Status,
                           assessmentdata$Response)
    }

   # SpatialConfidence
    # if not supplied directily, calculate ConfSpatial from 
    # Grids, GridsAssessed, Stations, Area_km2
    if (UserConfSpatial == FALSE) {
      assessmentdata <- assessmentdata %>%
        rowwise() %>%
        mutate(ConfSpatial=SpatialConfidence(Grids,GridsAssessed,Stations,Area_km2))
    }
    
     
    # calculate numerical values for each confidence rating
    assessmentdata <- assessmentdata %>%
      rowwise() %>%
      mutate(ConfScoreTemp=ifelse(is.na(ConfTemp),0,ConfValue(ConfTemp)),
             ConfScoreSpatial=ifelse(is.na(ConfSpatial),0,ConfValue(ConfSpatial)),
             ConfScoreMethod=ifelse(is.na(ConfMethod),0,ConfValue(ConfMethod)),
             ConfScoreThresh=ifelse(is.na(ConfThresh),0,ConfValue(ConfThresh)))
    if(bConfAcc){
      assessmentdata <- assessmentdata %>%
        rowwise() %>%
        mutate(ConfScoreAcc=ifelse(is.na(ConfAcc),0,ConfValue(ConfAcc)))
    }
    

    # calculate overall indicator confidence
    if(bConfAcc){
      assessmentdata <- assessmentdata %>%
        mutate(ConfScore = ConfWeightTemp*ConfScoreTemp + 
                 ConfWeightSpatial*ConfScoreSpatial +
                 ConfWeightAcc*ConfScoreAcc +
                 ConfWeightMethod*ConfScoreMethod +
                 ConfWeightThresh*ConfScoreThresh)
      assessmentdata <- assessmentdata %>% 
        dplyr::select(-c(ConfScoreTemp,ConfScoreSpatial,ConfScoreAcc,ConfScoreMethod,ConfScoreThresh))
      
    }else{
      assessmentdata <- assessmentdata %>%
        mutate(ConfScore = ConfWeightTemp*ConfScoreTemp + 
                 ConfWeightSpatial*ConfScoreSpatial +
                 ConfWeightMethod*ConfScoreMethod +
                 ConfWeightThresh*ConfScoreThresh)
      assessmentdata <- assessmentdata %>% 
        dplyr::select(-c(ConfScoreTemp,ConfScoreSpatial,ConfScoreMethod,ConfScoreThresh))
      
    }
   
 
    # Add columns specifying if the indicator is an organic or heavy metal. Used in confidence penalty calculations
    assessmentdata <- assessmentdata %>%
      rowwise() %>%
      mutate(HM=IsHeavyMetal(Type),
             Org=IsOrganic(Type))

    MatchListBioEffects <-
      c(
        "bioeffect",
        "bioeffects",
        "bio effects",
        "bio effect",
        "biological effects",
        "biological effect"
      )
    MatchListBiota <- c("biota", "biot")
    MatchListSed <- c("sediment", "sed", "sedi")
    
    # counting substances in different matrices as distinct (not currently used - kept for information)
    # these will be compared with CountHM and CountOrg
    IndicatorCountByMatrix <- assessmentdata %>%
      filter(!is.na(Type), !is.na(CR)) %>%
      group_by(AU, Matrix, Type, Substance) %>%
      summarise() %>%
      ungroup %>%
      group_by(AU, Matrix, Type) %>%
      summarise(Count = n()) %>%
      group_by(AU, Type) %>%
      summarise(Count = sum(Count,na.rm=T)) %>%
      pivot_wider(names_from="Type",values_from="Count")  %>%
      dplyr::select(AU) # replace with e.g. select(AU,HM) if heavy metals in separate matrices count twice

      
    # counting substances where the same substance in different matrices counts only once 
    IndicatorCountByWB <- assessmentdata %>%
      filter(!is.na(Type), !is.na(CR)) %>%
      group_by(AU, Type, Substance) %>%
      summarise() %>%
      ungroup  %>%
      group_by(AU, Type) %>%
      summarise(Count = n()) %>%
      pivot_wider(names_from="Type",values_from="Count") %>%
      dplyr::select(AU,Org,HM)

    # calculate the confidence penalties where counts of Org and HM are below the limits CountOrg and CountHM
    QEtypeCount <-  IndicatorCountByMatrix %>% 
      left_join(IndicatorCountByWB,by="AU") %>%
      mutate(HM=ifelse(is.na(HM),0,HM),
             Org=ifelse(is.na(Org),0,Org)) %>%
      mutate(PenaltyHM=ifelse(HM<CountHM,PenaltyHM,0),
             PenaltyOrg=ifelse(Org<CountOrg,PenaltyOrg,0)) %>%
      mutate(Penalty=1-((1-PenaltyOrg)*(1-PenaltyHM))) %>%
      dplyr::select(AU,HM,Org,Penalty)
    
    
    # calculate results by AU and matrix
    QEdata <- assessmentdata %>%
      filter(!is.na(CR)) %>%
      group_by(AU, Matrix) %>%
      summarise(
        sumCR = sum(CR, na.rm = TRUE),
        sumConf = sum(ConfScore, na.rm = TRUE),
        countHM = sum(HM, na.rm = TRUE),
        countOrg = sum(Org, na.rm = TRUE),
        Count = n()
      ) %>%
      ungroup()
    
    # calculate results by AU and matrix
    QEdata$IsBio <-
      ifelse(tolower(QEdata$Matrix) %in% MatchListBioEffects,
             TRUE,
             FALSE)
    
    # for Bio Effects the simple average of CR is used, rather than sum(CR)/sqrt(n)
    QEdata$IsBiota <-
      ifelse(tolower(QEdata$Matrix) %in% MatchListBiota, TRUE, FALSE)
    QEdata$IsSed <-
      ifelse(tolower(QEdata$Matrix) %in% MatchListSed, TRUE, FALSE)
    QEdata$ConSum <-
      QEdata$sumCR / ifelse(QEdata$IsBio, QEdata$Count, sqrt(QEdata$Count))
    
    QEdata$ConfScore <- QEdata$sumConf / QEdata$Count
    
    QEdata$Confidence <-
      mapply(ConfidenceStatus, QEdata$ConfScore, TRUE)
    
    # do some rounding of confidence scores
    if(is.numeric(ndigits)){
      QEdata <- QEdata %>%
        mutate(ConSum=round(ConSum,digits=ndigits))
    }
    
    # for results per AU and matrix, transpose to wide form, with each Matrix in a separate column
    QEspr <- QEdata %>%
      dplyr::select(AU, Matrix, ConSum) %>%
      pivot_wider(names_from="Matrix", values_from="ConSum")
    
    QEdata$QEStatus <- CHASEStatus(QEdata$ConSum, StatusClasses)
    QEdata <- left_join(matrices, QEdata, c('AU', 'Matrix'))
    QEdata <- QEdata %>%
      arrange(AU, Matrix)
    
      
    QEdataOut <- QEdata %>%
      dplyr::select(AU, Matrix, ConSum, QEStatus, ConfScore, Confidence)
    
    # Get the maximum value of ConSum per AU
    # also get counts of IsSed and IsBiota for each AU
    # these will be used later to apply a penalty to confidence if 
    # the AU does not have data for at least one of the two matrices
    CHASE <- QEdata %>%
      group_by(AU) %>%
      summarise(
        ConSum = max(ConSum, na.rm = TRUE),
        Sed = sum(IsSed, na.rm = TRUE),
        Biota = max(IsBiota, na.rm = TRUE),
        ConfScore = mean(ConfScore, na.rm = TRUE)
      )
    
    # Join the max value of ConSum in each AU to the Matrix having that value
    # if two matrices have the same worst value of ConSum, we will now have two matches for the relevant AU
    CHASEQE <- QEdata %>%
      dplyr::select(AU, Matrix, ConSum, QEStatus) %>%
      inner_join(CHASE, by = c("AU" = "AU", "ConSum" = "ConSum"))
    
    # for each AU, take the first Matrix having the worst (greatest) 
    # value of ConSum (in the order biota, sediment, water)
    CHASEQE <- CHASEQE %>%
      group_by(AU) %>%
      arrange(Matrix) %>%
      slice(1) %>%
      ungroup()

    # we now have dataframe of the worst QE for each AU
    CHASEQE <- rename(CHASEQE, Status = QEStatus, Worst = Matrix)
   
    # if there is not at least one of Biota or Sediment matrices included,
    # then confidence for the AU is reduced by 50% (multiply by 0.5)
    CHASEQE$ConfMultiplier <-
      ifelse(CHASEQE$Sed + CHASEQE$Biota > 0, 1, 0.5)
    CHASEQE$ConfScore <- CHASEQE$ConfScore * CHASEQE$ConfMultiplier
    
    CHASEQE$Confidence <- NA
    
    CHASEQE <- CHASEQE %>%
      dplyr::select(AU, Worst, ConSum, Status, ConfScore, Confidence) %>%
      left_join(QEtypeCount, by = c("AU" = "AU"))
    
    CHASEQE$ConfScore <- CHASEQE$ConfScore * (1 - CHASEQE$Penalty)
    CHASEQE$Confidence <-
      mapply(ConfidenceStatus, CHASEQE$ConfScore, TRUE)
    
    
    # 
    CHASEQE$Penalty <- scales::percent(CHASEQE$Penalty)
    
    assessmentdata$HM <- NULL
    assessmentdata$Org <- NULL
    if (!'RESPONSE' %in% toupper(cnames)) {
      assessmentdata$Response <- NULL
    }
    
    assessmentdata <- assessmentdata %>%
      left_join(
        rename(
          QEdataOut,
          QEConfidence = Confidence,
          QEConfScore = ConfScore
        ),
        c('AU', 'Matrix')
      )
    
    QEspr <- inner_join(QEspr, CHASEQE, 'AU')
    
    # do rounding of confidence scores
    if(is.numeric(ndigits)){
      CHASEQE <- CHASEQE %>%
        mutate(ConfScore=round(ConfScore,digits=ndigits))
      QEdataOut <- QEdataOut %>%
        mutate(ConfScore=round(ConfScore,digits=ndigits))
      QEspr <- QEspr %>%
        mutate(ConfScore=round(ConfScore,digits=ndigits))
    }
    
    
    if(is.na(summarylevel)){
      # return a list with results at all 4 summary levels
      CHASE <- list(
        "Indicators"=assessmentdata,
        "Matrix by row"=QEdataOut,
        "Matrix by column"=QEspr,
        "AssessmentUnits"=CHASEQE
      )
      return(CHASE)
    } else if(summarylevel == 1) {
      return(assessmentdata)
    } else if (summarylevel == 2) {
      return(QEdataOut)
    } else if (summarylevel == 3) {
      return(QEspr)
    } else {
      return(CHASEQE)
    }
    #
  }
}


