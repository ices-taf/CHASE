require(tidyverse)
source('src/CHASE.R')
source('src/CHASE_functions.R')

# load required data
file3 <- "./input/assessmentdata_L3.csv"
df3 <- read.table(file3,sep=";",header=T)

file4 <- "./input/assessmentdata_L4.csv"
df4 <- read.table(file4,sep=";",header=T)

# Get assessment results for each assessment level
CHASE3<-CHASEassessment(df3) 
CHASE4<-CHASEassessment(df4)


resL3<-CHASE3[[4]]
resL4<-CHASE4[[4]]


p3<-ggplot(resL3) +
  geom_histogram(aes(x=ConfScore),binwidth=0.02) +
  ggtitle("Level 3") +
  theme_minimal()
p3

p4<-ggplot(resL4) +
  geom_histogram(aes(x=ConfScore),binwidth=0.02) +
  ggtitle("Level ") +
  theme_minimal()
p4

# save figures
ggsave(p4,file="output/hist_L4.png",dpi=300,units="cm",height=15,width=15)
ggsave(p3,file="output/hist_L3.png",dpi=300,units="cm",height=15,width=15)

# save results
save(CHASE3,file="output/test_results_L3.Rda")
save(CHASE4,file="output/test_results_L4.Rda")
