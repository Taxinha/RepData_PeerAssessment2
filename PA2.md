# Weather Events impact on People Health and Economics in the US
Carlos Correia  
24 November 2014  

## Synopsis
Weather Events have an impact on the Population Health as well on country Economics.  
The goal of this analysis is to identify which Weather Events are more harmful for population health and which Weather Events have the greatest economic consequences.  
For this analysis we use data from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database with events from the year 1950 until end of November 2011.

## Data Processing

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
library(grid)
library(gridExtra)

## URL for the NOAA file data
fileURL      <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
localZipFile <- "./data/storm_data.csv.bz2"
inputRmdFile <- "PA2.Rmd"
```

### Helper functions
#### downloadAndExtractZipFile(fileName)
The `downloadAndExtractZipFile` function will verify if the data file (`fileName`) is available, if not will download the file using _CURL_ method. 

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
```

#### readCSVFile(fileName, ...)
The `readCSVFile` function will check if the data file exists (`fileName`) and read the data using `read.csv` R function

```r
## Reads the CSV data file
readCSVFile <- function(fileName, ...){
  if(! file.exists(fileName)){
    stop(paste("readDataFile: File ", fileName, " doesn't exist"))
  }
  
  print(paste("Reading file ", fileName))
  read.csv(fileName, ...)
}
```

#### cleanupEVTYPE(input)
The `cleanupEVTYPE` will receive as `input` the __EVTYPE__ from the NOAA data source and apply some normalization

```r
## Cleanup and normalize the EVTYPE
cleanupEVTYPE <- function(input){
  output <- input %>%
    toupper() %>%
    gsub(pattern = "^\\t+", replacement = "", perl = TRUE) %>%
    gsub(pattern = "^\\s+", replacement = "", perl = TRUE) %>%
    gsub(pattern = "^AVALANCE.+", replacement = "AVALANCHE", perl = TRUE) %>%
    gsub(pattern = "^BLIZZARD.+", replacement = "BLIZZARD", perl = TRUE) %>%
    gsub(pattern = "^COASTAL FLOOD.+", replacement = "COASTAL FLOOD", perl = TRUE) %>%
    gsub(pattern = "^COASTAL  FLOODING/EROSION$", replacement = "COASTAL FLOOD", perl = TRUE) %>%
    gsub(pattern = "^COASTALSTORM", replacement = "COASTAL STORM", perl = TRUE) %>%
    gsub(pattern = "^COLD.+", replacement = "COLD", perl = TRUE) %>%
    gsub(pattern = "^COOL AND WET$", replacement = "COLD", perl = TRUE) %>%
    gsub(pattern = "^DROUGHT.+", replacement = "DROUGHT", perl = TRUE) %>%
    gsub(pattern = "^DRY MIRCOBURST WINDS$", replacement = "DRY MIRCOBURST", perl = TRUE) %>%
    gsub(pattern = "^DUST DEVIL.+", replacement = "DUST DEVIL", perl = TRUE) %>%
    gsub(pattern = "^DUST STORM.+", replacement = "DUST STORM", perl = TRUE) %>%
    gsub(pattern = "^EXTREME COLD.+", replacement = "EXTREME COLD", perl = TRUE) %>%
    gsub(pattern = "^EXTREME WIND CHILL$", replacement = "EXTREME WINDCHILL", perl = TRUE) %>%
    gsub(pattern = "^FLASH FLOOD.+", replacement = "FLASH FLOOD", perl = TRUE) %>%
    gsub(pattern = "^FREEZE.+", replacement = "FREEZE", perl = TRUE) %>%
    gsub(pattern = "^FREEZING.+", replacement = "FREEZE", perl = TRUE) %>%
    gsub(pattern = "^GUSTY WIND", replacement = "GUSTY WINDS", perl = TRUE) %>%
    gsub(pattern = "^HEAT WAVE.+", replacement = "HEAT WAVE", perl = TRUE) %>%
    gsub(pattern = "^HEAVY RAIN.+", replacement = "HEAVY RAIN", perl = TRUE) %>%
    gsub(pattern = "^HEAVY SNOW.+", replacement = "HEAVY SNOW", perl = TRUE) %>%
    gsub(pattern = "^FLOOD.+", replacement = "FLOOD", perl = TRUE) %>%
    gsub(pattern = "^RIVER FLOOD.+", replacement = "FLOOD", perl = TRUE) %>%
    gsub(pattern = "^FREEZE.+", replacement = "FREEZE", perl = TRUE) %>%
    gsub(pattern = "^FROST.+", replacement = "FROST", perl = TRUE) %>%
    gsub(pattern = "^GUSTY WINDS.+", replacement = "GUSTY WINDS", perl = TRUE) %>%
    gsub(pattern = "^HAIL.+", replacement = "HAIL", perl = TRUE) %>%
    gsub(pattern = "^HEAVY SURF.+", replacement = "HEAVY SURF", perl = TRUE) %>%
    gsub(pattern = "^HIGH SURF.+", replacement = "HIGH SURF", perl = TRUE) %>%
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
    gsub(pattern = "^URBAN FLOOD.+", replacement = "URBAN/SML STREAM FLD", perl = TRUE) %>%
    gsub(pattern = "^URBAN SMALL.*", replacement = "URBAN/SML STREAM FLD", perl = TRUE) %>%
    gsub(pattern = "^URBAN/SMALL STREAM.*", replacement = "URBAN/SML STREAM FLD", perl = TRUE) %>%
    gsub(pattern = "^URBAN AND SMALL STREAM FLOODIN$", 
         replacement = "URBAN/SML STREAM FLD", perl = TRUE) %>%
    gsub(pattern = "^WATERSPOUT.+", replacement = "WATERSPOUT", perl = TRUE) %>%
    gsub(pattern = "^WILD.+", replacement = "WILDFIRE", perl = TRUE) %>%
    gsub(pattern = "^WIND.+", replacement = "WIND", perl = TRUE) %>%
    gsub(pattern = "^WINTER STORM.+", replacement = "WINTER STORM", perl = TRUE) %>%
    gsub(pattern = "^WINTER WEATHER.+", replacement = "WINTER WEATHER", perl = TRUE)
  
  output
}
```

#### calculateDMG(value, exp)
The `calculateDMG` function will calculate the monetary value of the damage using the `value` and `exp` as input. The `exp` can be:

- B or b: Billion
- M or m: Million
- K or k: Thousands
- H or h: Hundreds
- Any other option is ignored

```r
## Calculates the DMG values by checking the EXP and update the DMG value
calculateDMG <- function(value, exp){
  output <- value
  
  if(as.numeric(value) > 0 && !is.null(exp)) {
    output <- switch(EXPR = as.character(exp),
           B =, b = value * 1000000000,
           M =, m = value * 1000000,
           K =, k = value * 1000,
           H =, h = value * 100,
           "0" =, "2" =,"3" =, "6" =, "7" = value * 10 + as.numeric(exp), #Zero id 26267
           "5" = (value * 10 +5 ) * 1000,     ## see remarks 135k for ID 49238
           "4" = value * 1000000, ## see remark for ID 37895 (2.2M damages)
           value)
  }

  output
}
```

### Loading Raw Data
Starying by calling the `downloadAndExtractZipFile` function to download the [NOAA data source][1] in case we don't have the file locally.  
Then we call the `readCSVFile` function to read the file and load into `data` dataset variable

```r
downloadAndExtractZipFile(localZipFile)

data <- readCSVFile(localZipFile)
```

```
## [1] "Reading file  ./data/storm_data.csv.bz2"
```

```r
str(data)
```

```
## 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : Factor w/ 16335 levels "1/1/1966 0:00:00",..: 6523 6523 4242 11116 2224 2224 2260 383 3980 3980 ...
##  $ BGN_TIME  : Factor w/ 3608 levels "00:00:00 AM",..: 272 287 2705 1683 2584 3186 242 1683 3186 3186 ...
##  $ TIME_ZONE : Factor w/ 22 levels "ADT","AKS","AST",..: 7 7 7 7 7 7 7 7 7 7 ...
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: Factor w/ 29601 levels "","5NM E OF MACKINAC BRIDGE TO PRESQUE ISLE LT MI",..: 13513 1873 4598 10592 4372 10094 1973 23873 24418 4598 ...
##  $ STATE     : Factor w/ 72 levels "AK","AL","AM",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : Factor w/ 35 levels "","  N"," NW",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_LOCATI: Factor w/ 54429 levels ""," Christiansburg",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_DATE  : Factor w/ 6663 levels "","1/1/1993 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_TIME  : Factor w/ 3647 levels ""," 0900CST",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : Factor w/ 24 levels "","E","ENE","ESE",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_LOCATI: Factor w/ 34506 levels ""," CANTON"," TULIA",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ WFO       : Factor w/ 542 levels ""," CI","%SD",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ STATEOFFIC: Factor w/ 250 levels "","ALABAMA, Central",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ ZONENAMES : Factor w/ 25112 levels "","                                                                                                                               "| __truncated__,..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : Factor w/ 436781 levels "","\t","\t\t",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
```

### Cleaning the Data
The raw dataset has 37 columns, but for this analysis we only need the following columns: 

- __EVTYPE__ (_factor_): Weather Event Type
- __FATALITIES__ (_num_): Number of Fatalities
- __INJURIES__ (_num_): Number of Injuries
- __PROPDMG__ (_num_): Value for the property damage
- __PROPDMGEXP__ (_factor_): Exponential  for property damage (Billions, Millions, Thousands, Hundreds)
- __CROPDMG__ (_num_): Value for the crop damage
- __CROPDMGEXP__ (_factor_): Exponential  for crop damage (Billions, Millions, Thousands, Hundreds)


```r
tidyData <- data[, c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
str(tidyData)
```

```
## 'data.frame':	902297 obs. of  7 variables:
##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
```

```r
summary(tidyData)
```

```
##                EVTYPE         FATALITIES          INJURIES        
##  HAIL             :288661   Min.   :  0.0000   Min.   :   0.0000  
##  TSTM WIND        :219940   1st Qu.:  0.0000   1st Qu.:   0.0000  
##  THUNDERSTORM WIND: 82563   Median :  0.0000   Median :   0.0000  
##  TORNADO          : 60652   Mean   :  0.0168   Mean   :   0.1557  
##  FLASH FLOOD      : 54277   3rd Qu.:  0.0000   3rd Qu.:   0.0000  
##  FLOOD            : 25326   Max.   :583.0000   Max.   :1700.0000  
##  (Other)          :170878                                         
##     PROPDMG          PROPDMGEXP        CROPDMG          CROPDMGEXP    
##  Min.   :   0.00          :465934   Min.   :  0.000          :618413  
##  1st Qu.:   0.00   K      :424665   1st Qu.:  0.000   K      :281832  
##  Median :   0.00   M      : 11330   Median :  0.000   M      :  1994  
##  Mean   :  12.06   0      :   216   Mean   :  1.527   k      :    21  
##  3rd Qu.:   0.50   B      :    40   3rd Qu.:  0.000   0      :    19  
##  Max.   :5000.00   5      :    28   Max.   :990.000   B      :     9  
##                    (Other):    84                     (Other):     9
```

#### TidyData for Weather Events impacts on Population Health
Using the `tidyData` dataset the the input, we ignore the rows where we don't have Fatalities and Injuries since we want to get the Weather Events with most Health impact. After we select only the 3 columns we need - `EVTYPE`, `FATALITIES` and `INJURIES`, do some cleanup to the `EVTYPE` factor calling the `cleanupEVTYPE` function, group the data by `EVTYPE` and then calculate the sum of Fatalities and Injures for each Weather Event (`EVTYPE`). Since we want the most impact, we sort the total Fatalities and Injures descendent, pick the top 10 and apply the `melt` R function to prepare the data for the plot.

```r
tidyDataHarmful <- tidyData[!(tidyData$FATALITIES == 0 & tidyData$INJURIES == 0), ] %>%
  select(EVTYPE, FATALITIES, INJURIES) %>%
  mutate(EVTYPE = cleanupEVTYPE(EVTYPE)) %>%
  group_by(EVTYPE) %>%
  summarise(Fatality = sum(FATALITIES), Injury = sum(INJURIES)) %>%
  arrange(desc(Fatality), desc(Injury)) %>%
  head(n = 10) %>%
  melt(id = c("EVTYPE"), variable.name = "Type")

str(tidyDataHarmful)
```

```
## 'data.frame':	20 obs. of  3 variables:
##  $ EVTYPE: chr  "TORNADO" "EXCESSIVE HEAT" "FLASH FLOOD" "HEAT" ...
##  $ Type  : Factor w/ 2 levels "Fatality","Injury": 1 1 1 1 1 1 1 1 1 1 ...
##  $ value : num  5658 1903 1018 937 817 ...
```


```r
summary(tidyDataHarmful)
```

```
##     EVTYPE                Type        value      
##  Length:20          Fatality:10   Min.   :  255  
##  Class :character   Injury  :10   1st Qu.:  565  
##  Mode  :character                 Median : 1245  
##                                   Mean   : 6914  
##                                   3rd Qu.: 5338  
##                                   Max.   :91364
```

#### TidyData for Weather Events impacts on Economics
From the `tidyData`dataset, we ignore the rows where there isn't Property nor Crop damage value because we want to get the Weather Events with most Economical impact. After we select only the 5 columns we need - `EVTYPE`, `PROPDMG`, `PROPDMGEXP`, `CROPDMG` and `CROPDMGEXP`, do some cleanup to the `EVTYPE` factor calling the `cleanupEVTYPE` function. In other to get the real monetary value for Property or Crop damage, we call the function `calculateDMG`. Then we group the data by `EVTYPE` and then calculate the total of Propertie and Crop monetary damage for each Weather Event (`EVTYPE`).

```r
tidyDataDMG <- tidyData[!(tidyData$PROPDMG == 0 & tidyData$CROPDMG == 0), ] %>%
  select(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>%
  mutate(EVTYPE = cleanupEVTYPE(EVTYPE)) %>%
  mutate(FINALPROPDMG = mapply(calculateDMG, PROPDMG, PROPDMGEXP), 
         FINALCROPDMG = mapply(calculateDMG, CROPDMG, CROPDMGEXP)) %>%
  group_by(EVTYPE) %>%
  summarise(Property = sum(FINALPROPDMG), 
            Crop = sum(FINALCROPDMG))

str(tidyDataDMG)
```

```
## Classes 'tbl_df', 'tbl' and 'data.frame':	160 obs. of  3 variables:
##  $ EVTYPE  : chr  "?" "AGRICULTURAL FREEZE" "APACHE COUNTY" "ASTRONOMICAL HIGH TIDE" ...
##  $ Property: num  5000 0 5000 9425000 320000 ...
##  $ Crop    : num  0 28820000 0 0 0 ...
##  - attr(*, "drop")= logi TRUE
```


```r
summary(tidyDataDMG)
```

```
##     EVTYPE             Property              Crop          
##  Length:160         Min.   :0.000e+00   Min.   :0.000e+00  
##  Class :character   1st Qu.:1.538e+04   1st Qu.:0.000e+00  
##  Mode  :character   Median :4.505e+05   Median :0.000e+00  
##                     Mean   :2.671e+09   Mean   :3.069e+08  
##                     3rd Qu.:1.068e+07   3rd Qu.:5.025e+06  
##                     Max.   :1.451e+11   Max.   :1.397e+10
```

## Results
### Weather Event Impact on US Population Health
Data from the dataset of the most impact Weather Event of Population Health

```r
tidyDataHarmful
```

```
##               EVTYPE     Type value
## 1            TORNADO Fatality  5658
## 2     EXCESSIVE HEAT Fatality  1903
## 3        FLASH FLOOD Fatality  1018
## 4               HEAT Fatality   937
## 5          LIGHTNING Fatality   817
## 6  THUNDERSTORM WIND Fatality   711
## 7        RIP CURRENT Fatality   577
## 8              FLOOD Fatality   497
## 9          HIGH WIND Fatality   293
## 10      EXTREME COLD Fatality   287
## 11           TORNADO   Injury 91364
## 12    EXCESSIVE HEAT   Injury  6525
## 13       FLASH FLOOD   Injury  1785
## 14              HEAT   Injury  2100
## 15         LIGHTNING   Injury  5232
## 16 THUNDERSTORM WIND   Injury  9508
## 17       RIP CURRENT   Injury   529
## 18             FLOOD   Injury  6807
## 19         HIGH WIND   Injury  1472
## 20      EXTREME COLD   Injury   255
```

Now we plot to show the which Weather Event has the bigger impact on the US Population Health

```r
plotHarmfull <- 
    ggplot(tidyDataHarmful, aes(x = reorder(EVTYPE, -value), y = value/1000, fill = Type)) +
    geom_bar(stat="identity", position="dodge") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
    labs( 
        y = "Number of people (thousands)", 
        x = "",
      title = "Impact of Weather Events on the US Population Health")
    
print(plotHarmfull)
```

![Fig. 1) Top 10 Weather Events on Population Health](./PA2_files/figure-html/plotHarmfull-1.png) 

### Weather Events Impact on Economic


```r
head(tidyDataDMG)
```

```
## Source: local data frame [6 x 3]
## 
##                   EVTYPE Property     Crop
## 1                      ?     5000        0
## 2    AGRICULTURAL FREEZE        0 28820000
## 3          APACHE COUNTY     5000        0
## 4 ASTRONOMICAL HIGH TIDE  9425000        0
## 5  ASTRONOMICAL LOW TIDE   320000        0
## 6              AVALANCHE  3721800        0
```


```r
plotPropDMGData <- tidyDataDMG %>%
  select(EVTYPE, Property) %>%
  arrange(desc(Property)) %>%
  head(n=10)

plotPropDMGData
```

```
## Source: local data frame [10 x 2]
## 
##               EVTYPE     Property
## 1              FLOOD 145076186889
## 2          HURRICANE  84756180010
## 3            TORNADO  58542970113
## 4        STORM SURGE  47964724000
## 5        FLASH FLOOD  16735266179
## 6               HAIL  15974777189
## 7  THUNDERSTORM WIND   9773168468
## 8           WILDFIRE   8491563500
## 9     TROPICAL STORM   7714390550
## 10      WINTER STORM   6748997265
```


```r
plotCropDMGData <- tidyDataDMG %>%
  select(EVTYPE, Crop) %>%
  arrange(desc(Crop)) %>%
  head(n=10)

plotCropDMGData
```

```
## Source: local data frame [10 x 2]
## 
##               EVTYPE        Crop
## 1            DROUGHT 13972571780
## 2              FLOOD  5906732950
## 3          HURRICANE  5515292800
## 4        RIVER FLOOD  5029459000
## 5                ICE  5027113500
## 6               HAIL  3026094806
## 7        FLASH FLOOD  1437163150
## 8       EXTREME COLD  1313023000
## 9  THUNDERSTORM WIND  1225459732
## 10             FROST  1160186000
```



```r
plotDMGPROP <- 
    ggplot(plotPropDMGData, aes(x = reorder(EVTYPE, -Property), 
                                y = Property/10^9)) +
    geom_bar(stat="identity", fill="blue") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
    labs( 
        y = "Billions of Dollars", 
        x = "",
      title = "Property")

plotCROPDMG <- 
    ggplot(plotCropDMGData, aes(x = reorder(EVTYPE, -Crop), 
                                y = Crop/10^9)) +
    geom_bar(stat="identity", fill="blue") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
    labs( 
        y = "Billions of Dollars", 
        x = "",
      title = "Crop")

grid.arrange(plotDMGPROP, plotCROPDMG, ncol = 2,
             main = "Economic impact of Weather Events in the US")
```

![Fig. 2) Top 10 Weather Events Economical Impact on (left) Property (right) Crop](./PA2_files/figure-html/plotDMGPROPAndCROP-1.png) 





```r
plotTotalDMGData <- tidyDataDMG %>%
  arrange(desc(Property), desc(Crop)) %>%
  head(n=20) %>%
  melt(id=c("EVTYPE"), variable.name = "Type")

plotTotalDMGData
```

```
##                 EVTYPE     Type        value
## 1                FLOOD Property 145076186889
## 2            HURRICANE Property  84756180010
## 3              TORNADO Property  58542970113
## 4          STORM SURGE Property  47964724000
## 5          FLASH FLOOD Property  16735266179
## 6                 HAIL Property  15974777189
## 7    THUNDERSTORM WIND Property   9773168468
## 8             WILDFIRE Property   8491563500
## 9       TROPICAL STORM Property   7714390550
## 10        WINTER STORM Property   6748997265
## 11           HIGH WIND Property   6003353773
## 12         RIVER FLOOD Property   5118945500
## 13                 ICE Property   3971717315
## 14          HEAVY RAIN Property   3230998140
## 15 SEVERE THUNDERSTORM Property   1205360000
## 16             DROUGHT Property   1046106000
## 17          HEAVY SNOW Property    952949150
## 18           LIGHTNING Property    933925813
## 19            BLIZZARD Property    659713950
## 20             TYPHOON Property    600230000
## 21               FLOOD     Crop   5906732950
## 22           HURRICANE     Crop   5515292800
## 23             TORNADO     Crop    417462919
## 24         STORM SURGE     Crop       855000
## 25         FLASH FLOOD     Crop   1437163150
## 26                HAIL     Crop   3026094806
## 27   THUNDERSTORM WIND     Crop   1225459732
## 28            WILDFIRE     Crop    402781630
## 29      TROPICAL STORM     Crop    694896000
## 30        WINTER STORM     Crop     32444000
## 31           HIGH WIND     Crop    686301900
## 32         RIVER FLOOD     Crop   5029459000
## 33                 ICE     Crop   5027113500
## 34          HEAVY RAIN     Crop    795752800
## 35 SEVERE THUNDERSTORM     Crop       200000
## 36             DROUGHT     Crop  13972571780
## 37          HEAVY SNOW     Crop    134673100
## 38           LIGHTNING     Crop     12092090
## 39            BLIZZARD     Crop    112060000
## 40             TYPHOON     Crop       825000
```

We plot to show which Weather Event has the most Impact on country Economic (Property and Crop)

```r
plotTOTALDMG <- 
    ggplot(plotTotalDMGData, aes(x = reorder(EVTYPE, -value), 
                                 y = value/1000000000, fill = Type)) +
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
    labs( 
        y = "Billions of Dollars", 
        x = "",
      title = "Economic impact of Weather Events in the US")

print(plotTOTALDMG)
```

![Fig. 3) Top 20 Weather Events Economical Impact (Property and Crop)](./PA2_files/figure-html/plotTotalDMG-1.png) 

## Conclusions
With this analysis we conclude that:

* TORNADO is the Weather Event with the biggest impact on the US Population Health
* FLOOD is the Weather Event with biggest Economical impact on the Properties
* DROUGHT is the Weather Event with biggest Economical impact on the Crop

[1]: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2 "NOAA Data Source"
