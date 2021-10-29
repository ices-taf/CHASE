CHASE Test
================

## Testing the CHASE hazardous substances assessment tool

This is a test of the CHASE tool, using two input datasets: one for
level 3 assessment units and one for level 4 assessment units. Both
datasets are based on the same indicator results but are aggregated at
different spatial scales. A description of the preparation of these test
data sets can be seen in [test_dataset.md](test_dataset.md).

The results of the test are stored in the [output](output/) folder.

The code contained in this markdown document is also available as a
standalone R script in [src/CHASE_test.R](src/CHASE_test.R).

Load required functions

``` r
require(tidyverse)
source('src/CHASE.R')
source('src/CHASE_functions.R')
```

Load indicator data for level 3 and level 4 assessments.

``` r
# load data for level 3 indicators
file3 <- "./input/assessmentdata_L3.csv"
df3 <- read.table(file3,sep=";",header=T)

# load data for level 4 indicators
file4 <- "./input/assessmentdata_L4.csv"
df4 <- read.table(file4,sep=";",header=T)

# show the head of df3
head(df3)
```

    ##   AU_scale                                  AU Area_km2 Substance Type Matrix
    ## 1        3 Bothnian Bay Finnish Coastal waters 5548.123        HG   HM  Biota
    ## 2        3 Bothnian Bay Finnish Coastal waters 5548.123      SBD6  Org  Biota
    ## 3        3 Bothnian Bay Finnish Coastal waters 5548.123      SCB6  Org  Biota
    ## 4        3 Bothnian Bay Finnish Coastal waters 5548.123    CS-137  Rad  Biota
    ## 5        3 Bothnian Bay Finnish Coastal waters 5548.123    CS-137  Rad  Biota
    ## 6        3 Bothnian Bay Finnish Coastal waters 5548.123        HG   HM  Biota
    ##            CR ConfThresh CountStations CountData ConfSpatial ConfMethod
    ## 1   4.2861606          H            10        10           M          H
    ## 2 218.8782490          H             7         8           M          M
    ## 3   0.1348837          H             7         8           M          L
    ## 4   2.2830000          H             2         2           M          M
    ## 5   1.5800000          H             2         2           M          M
    ## 6  11.9582607          H            10        10           M          H
    ##   ConfTemp
    ## 1        M
    ## 2        M
    ## 3        H
    ## 4        M
    ## 5        H
    ## 6        M

Do the assessments

``` r
# Get assessment results for each assessment level
CHASE3<-CHASEassessment(df3) 
CHASE4<-CHASEassessment(df4)
```

The fourth item in each result list gives the overall results for each
assessment unit.

``` r
resL3<-CHASE3[[4]]
resL4<-CHASE4[[4]]

# show the head of resL3
head(resL3)
```

    ## # A tibble: 6 x 9
    ##   AU               Worst  ConSum Status ConfScore Confidence    HM   Org Penalty
    ##   <chr>            <fct>   <dbl> <chr>      <dbl> <chr>      <dbl> <dbl> <chr>  
    ## 1 Arkona Basin     Biota  101.   Bad        0.434 Class III      3     8 0%     
    ## 2 Arkona Basin Da~ Biota   26.3  Bad        0.573 Class II       3     8 0%     
    ## 3 Arkona Basin Ge~ Biota    2.85 Moder~     0.439 Class III      3     3 0%     
    ## 4 Bay of Mecklenb~ Sedim~   6.47 Poor       0.489 Class III      3     3 0%     
    ## 5 Belts Danish Co~ Sedim~ 141.   Bad        0.505 Class II       3     8 0%     
    ## 6 Bornholm Basin   Biota   31.4  Bad        0.331 Class III      3     6 0%

Plot the variations in confidence for the overall assessment

``` r
p3<-ggplot(resL3) +
  geom_histogram(aes(x=ConfScore),binwidth=0.02) +
  ggtitle("Level 3") +
  theme_minimal()
plot(p3)
```

![](markdown_images/plot%20confidence%20results-1.png)<!-- -->

``` r
p4<-ggplot(resL4) +
  geom_histogram(aes(x=ConfScore),binwidth=0.02) +
  ggtitle("Level ") +
  theme_minimal()
plot(p4)
```

![](markdown_images/plot%20confidence%20results-2.png)<!-- -->

Save the results and the plots

``` r
ggsave(p3,file="output/histogram_confidence_L3.png",dpi=300,units="cm",height=15,width=15)
ggsave(p4,file="output/histogram_confidence_L4.png",dpi=300,units="cm",height=15,width=15)

save(CHASE3,file="output/test_results_L3.Rda")
save(CHASE4,file="output/test_results_L4.Rda")
```
