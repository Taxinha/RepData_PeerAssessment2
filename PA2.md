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
fileURL      <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
localZipFile <- "./data/storm_data.csv.bz2"
## localFile    <- "./data/storm_data.csv"
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
    gsub(pattern = "^DRY MIRCOBURST.+", replacement = "DRY MIRCOBURST", perl = TRUE) %>%
    gsub(pattern = "^COLD.+", replacement = "COLD", perl = TRUE) %>%
    gsub(pattern = "^FLASH FLOOD.+", replacement = "FLASH FLOOD", perl = TRUE) %>%
    gsub(pattern = "^GUSTY WIND", replacement = "GUSTY WINDS", perl = TRUE) %>%
    gsub(pattern = "^HEAT WAVE.+", replacement = "HEAT WAVE", perl = TRUE) %>%
    gsub(pattern = "^HEAVY RAIN.+", replacement = "HEAVY RAIN", perl = TRUE) %>%
    gsub(pattern = "^HEAVY SNOW.+", replacement = "HEAVY SNOW", perl = TRUE) %>%
    gsub(pattern = "^FLOOD.+", replacement = "FLOOD", perl = TRUE) %>%
    gsub(pattern = "^FREEZE.+", replacement = "FREEZE", perl = TRUE) %>%
    gsub(pattern = "^GUSTY WINDS.+", replacement = "GUSTY WINDS", perl = TRUE) %>%
    gsub(pattern = "^HEAVY SURF.+", replacement = "HEAVY SURF", perl = TRUE) %>%
    gsub(pattern = "^(HIGH|HIGH WIND.+)", replacement = "HIGH WIND", perl = TRUE) %>%
    gsub(pattern = "^HURRICANE.+", replacement = "HURRICANE", perl = TRUE) %>%
    gsub(pattern = "^HYPOTHERMIA.+", replacement = "HYPOTHERMIA", perl = TRUE) %>%
    gsub(pattern = "^ICE.+", replacement = "ICE", perl = TRUE) %>%
    gsub(pattern = "vLANDSLIDE.+", replacement = "LANDSLIDE", perl = TRUE)
  
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
  summarise(TOTAL_FATALITIES = sum(FATALITIES), TOTAL_INJURIES = sum(INJURIES))



str(tidyDataHarmful)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	164 obs. of  3 variables:
##  $ EVTYPE          : chr  "AVALANCHE" "BLACK ICE" "BLIZZARD" "BLOWING SNOW" ...
##  $ TOTAL_FATALITIES: num  225 1 101 2 0 6 4 158 18 0 ...
##  $ TOTAL_INJURIES  : num  170 24 805 14 2 7 2 60 342 4 ...
##  - attr(*, "drop")= logi TRUE
```


```r
summary(tidyDataHarmful)
```

```
##     EVTYPE          TOTAL_FATALITIES  TOTAL_INJURIES    
##  Length:164         Min.   :   0.00   Min.   :    0.00  
##  Class :character   1st Qu.:   1.00   1st Qu.:    1.00  
##  Mode  :character   Median :   2.00   Median :    5.50  
##                     Mean   :  92.35   Mean   :  856.88  
##                     3rd Qu.:  17.25   3rd Qu.:   73.25  
##                     Max.   :5633.00   Max.   :91346.00
```


```r
tidyDataDMG <- data %>%
  select(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP, REMARKS)



str(tidyDataDMG)
```

```
## 'data.frame':	902297 obs. of  6 variables:
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ REMARKS   : Factor w/ 436781 levels "","\t","\t\t",..: 1 1 1 1 1 1 1 1 1 1 ...
```


```r
summary(tidyDataDMG)
```

```
##                EVTYPE          PROPDMG          PROPDMGEXP    
##  HAIL             :288661   Min.   :   0.00          :465934  
##  TSTM WIND        :219940   1st Qu.:   0.00   K      :424665  
##  THUNDERSTORM WIND: 82563   Median :   0.00   M      : 11330  
##  TORNADO          : 60652   Mean   :  12.06   0      :   216  
##  FLASH FLOOD      : 54277   3rd Qu.:   0.50   B      :    40  
##  FLOOD            : 25326   Max.   :5000.00   5      :    28  
##  (Other)          :170878                     (Other):    84  
##     CROPDMG          CROPDMGEXP    
##  Min.   :  0.000          :618413  
##  1st Qu.:  0.000   K      :281832  
##  Median :  0.000   M      :  1994  
##  Mean   :  1.527   k      :    21  
##  3rd Qu.:  0.000   0      :    19  
##  Max.   :990.000   B      :     9  
##                    (Other):     9  
##                                            REMARKS      
##                                                :287433  
##                                                : 24013  
##  Trees down.\n                                 :  1110  
##  Several trees were blown down.\n              :   568  
##  Trees were downed.\n                          :   446  
##  Large trees and power lines were blown down.\n:   432  
##  (Other)                                       :588295
```

##Results


## Questions
1) Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

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
## [1] dplyr_0.3.0.2
## 
## loaded via a namespace (and not attached):
##  [1] assertthat_0.1  DBI_0.3.1       digest_0.6.4    evaluate_0.5.5 
##  [5] formatR_1.0     htmltools_0.2.6 knitr_1.8       lazyeval_0.1.9 
##  [9] magrittr_1.0.1  parallel_3.1.2  Rcpp_0.11.3     rmarkdown_0.3.3
## [13] stringr_0.6.2   tools_3.1.2     yaml_2.1.13
```

