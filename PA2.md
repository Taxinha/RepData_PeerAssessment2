# RepData - Peer Assessment 2
Carlos Correia  
22 November 2014  

##Synopsis

##Data Processing

### Loading Libraries and defining global variables

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(reshape2)
library(ggplot2)

fileURL      <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
localZipFile <- "./data/storm_data.csv.bz2"
inputRmdFile <- "PA2.Rmd"
```

### Helper functions

```r
## Download and Extract Zip data file
downloadAndExtractZipFile <- function(fileName){
  ## check if the data folder exists
  if(!file.exists("data")){
    dir.create("data")
  }
    
  ## check if zip file exists - if not download the zip file with the data
  if(!file.exists(fileName)){
    download.file(fileURL, destfile = fileName, method = "curl")
  }
}

## Reads the CSV data file
readCSVFile <- function(fileName, ...){
  if(! file.exists(fileName)){
    stop(paste("readDataFile: File ", fileName, " doesn't exist"))
  }
  
  print(paste("Reading file ", fileName))
  read.csv(fileName, ...)
}

cleanupEVTYPE <- function(input){
  output <- input %>%
    toupper() %>%
    gsub(pattern = "^AVALANCE", replacement = "AVALANCHE", perl = TRUE) %>%
    gsub(pattern = "^COASTAL FLOOD.+", replacement = "COASTAL FLOOD", perl = TRUE) %>%
    gsub(pattern = "^COASTALSTORM", replacement = "COASTAL STORM", perl = TRUE) %>%
    gsub(pattern = "^COLD.+", replacement = "COLD", perl = TRUE) %>%
    gsub(pattern = "^DROUGHT.+", replacement = "DROUGHT", perl = TRUE) %>%
    gsub(pattern = "^DRY MIRCOBURST WINDS$", replacement = "DRY MIRCOBURST", perl = TRUE) %>%
    gsub(pattern = "^EXTREME COLD.+", replacement = "EXTREME COLD", perl = TRUE) %>%
    gsub(pattern = "^FLASH FLOOD.+", replacement = "FLASH FLOOD", perl = TRUE) %>%
    gsub(pattern = "^FREEZE.+", replacement = "FREEZE", perl = TRUE) %>%
    gsub(pattern = "^GUSTY WIND", replacement = "GUSTY WINDS", perl = TRUE) %>%
    gsub(pattern = "^HEAT WAVE.+", replacement = "HEAT WAVE", perl = TRUE) %>%
    gsub(pattern = "^HEAVY RAIN.+", replacement = "HEAVY RAIN", perl = TRUE) %>%
    gsub(pattern = "^HEAVY SNOW.+", replacement = "HEAVY SNOW", perl = TRUE) %>%
    gsub(pattern = "^FLOOD.+", replacement = "FLOOD", perl = TRUE) %>%
    gsub(pattern = "^RIVER FLOOD.+", replacement = "FLOOD", perl = TRUE) %>%
    gsub(pattern = "^FREEZE.+", replacement = "FREEZE", perl = TRUE) %>%
    gsub(pattern = "^GUSTY WINDS.+", replacement = "GUSTY WINDS", perl = TRUE) %>%
    gsub(pattern = "^HEAVY SURF.+", replacement = "HEAVY SURF", perl = TRUE) %>%
    gsub(pattern = "^HIGH WIND.+", replacement = "HIGH WIND", perl = TRUE) %>%
    gsub(pattern = "^HIGH$", replacement = "HIGH WIND", perl = TRUE) %>% ## see REMARKS
    gsub(pattern = "^HURRICANE.+", replacement = "HURRICANE", perl = TRUE) %>%
    gsub(pattern = "^HYPOTHERMIA.+", replacement = "HYPOTHERMIA", perl = TRUE) %>%
    gsub(pattern = "^ICE.+", replacement = "ICE", perl = TRUE) %>%
    gsub(pattern = "^ICY ROADS.+", replacement = "ICE", perl = TRUE) %>%
    gsub(pattern = "^LANDSLIDE.+", replacement = "LANDSLIDE", perl = TRUE) %>%
    gsub(pattern = "^LIGHTNING.+", replacement = "LIGHTNING", perl = TRUE) %>%
    gsub(pattern = "^RECORD/EXCESSIVE HEAT$", replacement = "RECORD HEAT", perl = TRUE) %>%
    gsub(pattern = "^RIP CURRENT.+", replacement = "RIP CURRENT", perl = TRUE) %>%
    gsub(pattern = "^SNOW.+", replacement = "SNOW", perl = TRUE) %>%
    gsub(pattern = "^STORM SURGE.+", replacement = "STORM SURGE", perl = TRUE) %>%
    gsub(pattern = "^STRONG WIND.+", replacement = "STRONG WIND", perl = TRUE) %>%
    gsub(pattern = "^THUNDERSTORM.*", replacement = "THUNDERSTORM WIND", perl = TRUE) %>%
    gsub(pattern = "^THUNDERTORM WINDS$", replacement = "THUNDERSTORM WIND", perl = TRUE) %>%
    gsub(pattern = "^TSTM WIND.*", replacement = "THUNDERSTORM WIND", perl = TRUE) %>%
    gsub(pattern = "^TORNADO.+", replacement = "TORNADO", perl = TRUE) %>%
    gsub(pattern = "^TROPICAL STORM.+", replacement = "TROPICAL STORM", perl = TRUE) %>%
    gsub(pattern = "^UNSEASONABLY WARM.+", replacement = "UNSEASONABLY WARM", perl = TRUE) %>%
    gsub(pattern = "^URBAN AND SMALL STREAM FLOODIN$", 
         replacement = "URBAN/SML STREAM FLD", perl = TRUE) %>%
    gsub(pattern = "^WATERSPOUT.+", replacement = "WATERSPOUT", perl = TRUE) %>%
    gsub(pattern = "^WILD.+", replacement = "WILDFIRE", perl = TRUE) %>%
    gsub(pattern = "^WIND.+", replacement = "WIND", perl = TRUE) %>%
    gsub(pattern = "^WINTER STORM.+", replacement = "WINTER STORM", perl = TRUE) %>%
    gsub(pattern = "^WINTER WEATHER.+", replacement = "WINTER WEATHER", perl = TRUE)
  
  output
}

calculateDMG <- function(value, exp){
  output <- value
  #if(as.numeric(value) > 0) {
  #  output <- switch(EXPR = exp,
  #         B =, b = value * 1000000000,
  #         M =, m = value * 1000000,
  #         K =, k = value * 1000,
  #         H =, h = value * 100,
  #         -1)
  #}
  
  output
}
```

### Load Raw Data

```r
downloadAndExtractZipFile(localZipFile)

data <- readCSVFile(localZipFile)
```

```
## [1] "Reading file  ./data/storm_data.csv.bz2"
```

```r
## summary(data)
```

### Tidy Data

```r
tidyDataHarmful <- data[!(data$FATALITIES == 0 & data$INJURIES == 0), ] %>%
  select(EVTYPE, FATALITIES, INJURIES) %>%
  mutate(EVTYPE = cleanupEVTYPE(EVTYPE)) %>%
  group_by(EVTYPE) %>%
  summarise(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES)) %>%
  arrange(desc(FATALITIES), desc(INJURIES)) %>%
  head(n = 10) %>%
  melt(id=c("EVTYPE"), variable.name = "TYPE")

str(tidyDataHarmful)
```

```
## 'data.frame':	20 obs. of  3 variables:
##  $ EVTYPE: chr  "TORNADO" "EXCESSIVE HEAT" "FLASH FLOOD" "HEAT" ...
##  $ TYPE  : Factor w/ 2 levels "FATALITIES","INJURIES": 1 1 1 1 1 1 1 1 1 1 ...
##  $ value : num  5658 1903 1018 937 817 ...
```


```r
summary(tidyDataHarmful)
```

```
##     EVTYPE                  TYPE        value      
##  Length:20          FATALITIES:10   Min.   :  255  
##  Class :character   INJURIES  :10   1st Qu.:  565  
##  Mode  :character                   Median : 1245  
##                                     Mean   : 6914  
##                                     3rd Qu.: 5338  
##                                     Max.   :91364
```


```r
tidyDataDMG <- data[!(data$PROPDMG == 0 & data$CROPDMG == 0), ] %>%
  select(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP, REMARKS) %>%
  mutate(EVTYPE = cleanupEVTYPE(EVTYPE)) %>%
  mutate(FINALPROPDMG = calculateDMG(PROPDMG, PROPDMGEXP), 
         FINALCROPDMG = calculateDMG(CROPDMG, CROPDMGEXP))
  


str(tidyDataDMG)
```

```
## 'data.frame':	245031 obs. of  8 variables:
##  $ EVTYPE      : chr  "TORNADO" "TORNADO" "TORNADO" "TORNADO" ...
##  $ PROPDMG     : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP  : Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG     : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP  : Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ REMARKS     : Factor w/ 436781 levels "","\t","\t\t",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ FINALPROPDMG: num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ FINALCROPDMG: num  0 0 0 0 0 0 0 0 0 0 ...
```


```r
summary(tidyDataDMG)
```

```
##     EVTYPE             PROPDMG          PROPDMGEXP        CROPDMG       
##  Length:245031      Min.   :   0.00   K      :229057   Min.   :  0.000  
##  Class :character   1st Qu.:   2.00   M      : 11319   1st Qu.:  0.000  
##  Mode  :character   Median :   8.00          :  4357   Median :  0.000  
##                     Mean   :  44.42   0      :   209   Mean   :  5.623  
##                     3rd Qu.:  25.00   B      :    40   3rd Qu.:  0.000  
##                     Max.   :5000.00   5      :    18   Max.   :990.000  
##                                       (Other):    31                    
##    CROPDMGEXP                                               REMARKS      
##         :145037                                                 : 32374  
##  K      : 97960                                                 :  7265  
##  M      :  1982   Trees down.\n                                 :   628  
##  k      :    21   Large trees and power lines were blown down.\n:   431  
##  0      :    17   Several trees were blown down.\n              :   404  
##  B      :     7   A few trees were blown down.\n                :   312  
##  (Other):     7   (Other)                                       :203617  
##   FINALPROPDMG      FINALCROPDMG    
##  Min.   :   0.00   Min.   :  0.000  
##  1st Qu.:   2.00   1st Qu.:  0.000  
##  Median :   8.00   Median :  0.000  
##  Mean   :  44.42   Mean   :  5.623  
##  3rd Qu.:  25.00   3rd Qu.:  0.000  
##  Max.   :5000.00   Max.   :990.000  
## 
```

##Results


####Questions
1) Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?


```r
plotHarmfull <- 
    ggplot(tidyDataHarmful, aes(x = reorder(EVTYPE, -value), y = value, fill = TYPE)) +
    geom_bar(stat="identity", position="dodge") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
    labs( 
        y = "Number of people", 
        x = "Type of Events",
      title = "TOP 10 of events more harmfull")
    
print(plotHarmfull)
```

![](./PA2_files/figure-html/plotHarmfull-1.png) 


2) Across the United States, which types of events have the greatest economic consequences?



### Software Details used on this project

```r
sessionInfo()
```

```
## R version 3.1.2 (2014-10-31)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## 
## locale:
## [1] en_IE.UTF-8/en_IE.UTF-8/en_IE.UTF-8/C/en_IE.UTF-8/en_IE.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] ggplot2_1.0.0 reshape2_1.4  dplyr_0.3.0.2
## 
## loaded via a namespace (and not attached):
##  [1] assertthat_0.1   colorspace_1.2-4 DBI_0.3.1        digest_0.6.4    
##  [5] evaluate_0.5.5   formatR_1.0      grid_3.1.2       gtable_0.1.2    
##  [9] htmltools_0.2.6  knitr_1.8        labeling_0.3     lazyeval_0.1.9  
## [13] magrittr_1.0.1   MASS_7.3-35      munsell_0.4.2    parallel_3.1.2  
## [17] plyr_1.8.1       proto_0.3-10     Rcpp_0.11.3      rmarkdown_0.3.3 
## [21] scales_0.2.4     stringr_0.6.2    tools_3.1.2      yaml_2.1.13
```

