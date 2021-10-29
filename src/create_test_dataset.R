# Development of two indicator datasets 
# for testing the CHASE assessment tool

# Load packages

library(sf)
library(tidyverse)
library(patchwork) # not essential - this is used only to combine the two maps of assessment units

# Load shape files for assessment units

units3 <- read_sf(dsn = "./assessment_units", 
                  layer = "AssessmentUnits3")

units4 <- read_sf(dsn = "./assessment_units", 
                  layer = "AssessmentUnits4")

# show maps of the assessment units
map3 <- ggplot() +
  theme_minimal(base_size=9) +
  ggtitle("Level 3 Assessment Units") +
  geom_sf(data=units3, colour="black", fill=NA)

map4 <- ggplot() +
  theme_minimal(base_size=9) + 
  ggtitle("Level 4 Assessment Units") +
  geom_sf(data=units4, colour="black",  fill=NA)

map3+map4

# Read indicator data from text file

file <- "./input/bsii_by_stn.txt"

df <- read.table(file,sep=";",
                 header=T,
                 fileEncoding="UTF-8",
                 comment.char="")

# Convert indicator data frame to simple features
df_sf <- st_as_sf(df, 
                  coords = c("longitude", "latitude"), 
                  crs = 4326) # EPSG 4326 is WGS84

# transform the sf dataframe to the same projection as the assessment units
df_sf <- st_transform(df_sf,
                      crs=st_crs(units3))

# plot the stations and the assessment units together
ggplot() +
  theme_minimal(base_size=9) + 
  ggtitle("Level 4 Assessment Units \n+ Indicator positions") +
  geom_sf(data=units4, colour="black",  fill=NA) +
  geom_sf(data=df_sf, colour="red")

# Intersect the indicator data points with the polygons to get dataframes 
# of indicators with added information about which assessment unit each
# indicator belongs to. 
# There will be one dataframe for intersection with Level 3 assessment
# units and another dataframe for Level 4 assessment units. 

df3 <- st_intersection(df_sf, units3)
df4 <- st_intersection(df_sf, units4)

# geometry information is no longer needed
df3$geometry <-NULL
df4$geometry <-NULL

# show the head for Level 3 data
head(df3)


# Spatial confidence 

# count stations for Level3
stn_count3 <- df3 %>%
  distinct(HELCOM_ID,level_3,Matrix,Substance,Station) %>%
  group_by(HELCOM_ID,level_3,Matrix,Substance) %>%
  summarise(CountStations=n()) %>%
  ungroup()

# count observations for Level3
data_count3 <- df3 %>%
  group_by(HELCOM_ID,level_3,Matrix,Substance) %>%
  summarise(CountData=n()) %>%
  ungroup()

# merge L3 counts back to original L3 data 

df3 <- df3 %>%
  left_join(stn_count3,by=c("HELCOM_ID","level_3","Matrix","Substance")) %>%
  left_join(data_count3,by=c("HELCOM_ID","level_3","Matrix","Substance"))

# count stations for Level4
stn_count4 <- df4 %>%
  distinct(HELCOM_ID,Name,Matrix,Substance,Station) %>%
  group_by(HELCOM_ID,Name,Matrix,Substance) %>%
  summarise(CountStations=n()) %>%
  ungroup()

# count observations for Level4
data_count4 <- df4 %>%
  group_by(HELCOM_ID,Name,Matrix,Substance) %>%
  summarise(CountData=n()) %>%
  ungroup()

# merge L4 counts back to original L4 data 

df4 <- df4 %>%
  left_join(stn_count4,by=c("HELCOM_ID","Name","Matrix","Substance")) %>%
  left_join(data_count4,by=c("HELCOM_ID","Name","Matrix","Substance"))

# Add spatial confidence based on km<sup>2</sup> per sample
# <500     | High
# 500-5000 | Moderate
# >5000    | Low

km2perSampleBounds<-c(500,5000)
conf<-c("L","M","H")

df3 <- df3 %>%
  mutate(km2perSample=Area_km2/CountData)
df3 <- df3 %>%
  rowwise() %>%
  mutate(ix=length(km2perSampleBounds[km2perSampleBounds>km2perSample])) %>%
  mutate(ConfSpatial=conf[ix+1]) %>%
  dplyr::select(-ix)

df4 <- df4 %>%
  mutate(km2perSample=Area_km2/CountData)

df4 <- df4 %>%
  rowwise() %>%
  mutate(ix=length(km2perSampleBounds[km2perSampleBounds>km2perSample])) %>%
  mutate(ConfSpatial=conf[ix+1]) %>%
  dplyr::select(-ix)


# Threshold confidence
# Load table of confidences for threshold values

dfConfThreshold <- read.table("./input/confidence_thresholds.txt",sep=";",header=T) 

print(dfConfThreshold)

# Original source for threshold confidence data:
# Table 2 in this meeting document: 
# https://portal.helcom.fi/meetings/HOLAS%20II%20HZ%20WS%201-2018-518/MeetingDocuments/2-3%20Confidence%20setting%20for%20CHASE%20integrated%20assessment.pdf

# Join threshold confidences to L3 and L4 indicator tables
# (NOT NEEDED as ConfThresh is already included)
# df3 <- df3 %>%
#   left_join(dfConfThreshold,by=c("Substance","Matrix")) %>%
#   mutate(AU_scale=3) 
# 
# df4 <- df4 %>%
#   left_join(dfConfThreshold,by=c("Substance","Matrix")) %>%
#   mutate(AU_scale=4)

# Methodological confidence

# Add random confidences for method to L3 and L4 indicator tables

conf<-c("L","M","H")
df3[,"ConfMethod"]<-round(runif(nrow(df3), min=0.5, max=3.49999))

df3 <- df3 %>%
  rowwise() %>%
  mutate(ConfMethod=conf[ConfMethod]) %>%
  ungroup()

df4[,"ConfMethod"]<-round(runif(nrow(df4), min=0.5, max=3.49999))
df4 <- df4 %>%
  rowwise() %>%
  mutate(ConfMethod=conf[ConfMethod]) %>%
  ungroup()

# Temporal confidence

# Add random temporal confidences to L3 and L4 indicator tables

conf<-c("L","M","H")
df3[,"ConfTemp"]<-round(runif(nrow(df3), min=0.5, max=3.49999))

df3 <- df3 %>%
  rowwise() %>%
  mutate(ConfTemp=conf[ConfTemp]) %>%
  ungroup()

df4[,"ConfTemp"]<-round(runif(nrow(df4), min=0.5, max=3.49999))
df4 <- df4 %>%
  rowwise() %>%
  mutate(ConfTemp=conf[ConfTemp]) %>%
  ungroup()

# Select only columns needed

df3 <- df3 %>%
  mutate(AU_scale=3) %>%
  dplyr::select(AU_scale,AU=level_3,Area_km2,Substance,Type,Matrix,CR,
                ConfThresh,CountStations,CountData,ConfSpatial,ConfMethod,ConfTemp)

df4 <- df4 %>%
  mutate(AU_scale=4) %>%
  dplyr::select(AU_scale,AU=HELCOM_ID,Area_km2,Substance,Type,Matrix,CR,
                ConfThresh,CountStations,CountData,ConfSpatial,ConfMethod,ConfTemp)

# Save the indicator data, including 
# * information on which assessment units they belong to
# * threshold confidence [H/M/L]
# * method confidence [H/M/L]
# * number of stations in AU
# * number of data points per km2
# * spatial confidence [H/M/L]
# * temporal confidence [H/M/L]

write.table(df3,file="./input/assessmentdata_L3.csv",
            sep=";",row.names=F,col.names=T,quote=T)
write.table(df4,file="./input/assessmentdata_L4.csv",
            sep=";",row.names=F,col.names=T,quote=T)

