# Exploration of Severe Weather Events in the US between 1950 and 2011
Tulin Varol  
10/09/2017  



## Synopsis
In this document, the NOAA Storm Database is explored in order to answer some basic questions about severe weather events. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage. The events in the database start in the year 1950 and end in November 2011.  In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered more complete.

Using these dataset, the types of events that are most harmful with respect to population health across the United States are investigated. Beside that, the types of events that have the greatest economic consequences across the United States are studied and reported. 

## Data Processing
Let's first check if the dataset is already in our directory. If not, download the data from the website and then read it. When reading data, we'll use the advantage of fast reading ability of the function "fread". Only columns that are relevant to this study are kept.


```r
if(!file.exists("StormData.csv.bz2")) {
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(fileUrl, destfile = "stormData.csv.bz2", method="curl")
}
library(data.table)
library(dplyr)
inputData <- tbl_df(fread(sprintf("bzcat %s", "StormData.csv.bz2"), sep = ",", select = c(8,23:28)))
```

```
## 
Read 71.3% of 967216 rows
Read 902297 rows and 7 (of 37) columns from 0.523 GB file in 00:00:03
```

```r
head(inputData)
```

```
## # A tibble: 6 × 7
##    EVTYPE FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
##     <chr>      <dbl>    <dbl>   <dbl>      <chr>   <dbl>      <chr>
## 1 TORNADO          0       15    25.0          K       0           
## 2 TORNADO          0        0     2.5          K       0           
## 3 TORNADO          0        2    25.0          K       0           
## 4 TORNADO          0        2     2.5          K       0           
## 5 TORNADO          0        2     2.5          K       0           
## 6 TORNADO          0        6     2.5          K       0
```
Now, in the selected dataset, we have the following information:

* EVTYPE (Event types)
* FATALITIES
* INJURIES
* PROPDMG (Property damage estimates given in dollar)
* PROPDMGEXP (An alphabetical character signifying the magnitude of the number given in PROPDMG; "K" for thousands, "M" for millions, "B" for billions, etc.)
* CROPDMG (Crop damage)
* CROPDMGEXP (Similar to PROPDMGEXP)

## Results
The dataset is ready to be worked on. We can analyze the data now to answer the questions:

### Across the United States, which types of events are most harmful with respect to population health?
In order to answer this question, let's check the number of fatalities and number of injuries per event type. It makes more sense to look for the first 10 most harmful events since there are 985 event types (you can check levels(as.factor(inputData$EVTYPE)) ). 


```r
# For each event type, sum fatalities and for each event type sum injuries
healthSummary <- summarise(group_by(inputData,EVTYPE),fatal=sum(FATALITIES,na.rm = TRUE), injury=sum(INJURIES,na.rm = TRUE))

# Order the event types in a descending total number of fatalities
sumFatal <- healthSummary %>% arrange(desc(fatal))

# Select only first 10 leading events for fatalities and select only related data columns
sumFatal <- sumFatal[1:10,1:2]
sumFatal
```

```
## # A tibble: 10 × 2
##            EVTYPE fatal
##             <chr> <dbl>
## 1         TORNADO  5633
## 2  EXCESSIVE HEAT  1903
## 3     FLASH FLOOD   978
## 4            HEAT   937
## 5       LIGHTNING   816
## 6       TSTM WIND   504
## 7           FLOOD   470
## 8     RIP CURRENT   368
## 9       HIGH WIND   248
## 10      AVALANCHE   224
```

```r
# Order the event types in a descending total number of injuries 
sumInjury <- healthSummary %>% arrange(desc(injury))

# Select only first 10 leading events for injuries and select only related data columns
sumInjury <- sumInjury[1:10,c(1,3)]
sumInjury
```

```
## # A tibble: 10 × 2
##               EVTYPE injury
##                <chr>  <dbl>
## 1            TORNADO  91346
## 2          TSTM WIND   6957
## 3              FLOOD   6789
## 4     EXCESSIVE HEAT   6525
## 5          LIGHTNING   5230
## 6               HEAT   2100
## 7          ICE STORM   1975
## 8        FLASH FLOOD   1777
## 9  THUNDERSTORM WIND   1488
## 10              HAIL   1361
```

```r
# Plot number of injuries and fatalities separately for 10 leading events
par(mfrow=c(1,2), mar=c(10,4,3,2))
# las=2 make label text perpendicular to axis
barplot(as.numeric(sumFatal$fatal),names.arg=sumFatal$EVTYPE,las=2,col="blue", ylim=c(0,6000), main="Total Deaths for the Top 10 Leading Events", cex.main=0.7)
title(ylab="Total Deaths", line=2.9) 
barplot(as.numeric(sumInjury$injury),names.arg=sumInjury$EVTYPE,las=2,col="blue", ylim=c(0,100000), main="Total Injuries for the Top 10 Leading Events", cex.main=0.7)
title(ylab="Total Injuries", line=3.2) 
```

![](Explore_storm_files/figure-html/populationHealth-1.png)<!-- -->

The left distribution shows that "tornado", "excessive heat" and "flash flood" are the first three event types that result in the highest number of fatalities. The right distribution then tells us that "tornado", "TSTM wind", "flood" are the first three event types that result in the highest number of injuries. The rest of the harmful events are shown on the plots.

### Across the United States, which types of events have the greatest economic consequences?
In order to study the economic consequences of the event types, it is first needed to clean the data. Damages caused by different harmful events are splitted into two categories as the property damage and the crop damage. In the dataset, these are listed in the columns with the names of PROPDMG and CROPDMG, respectively. The values given in these columns are not enough to determine the amount of damage in dollars. There are two other associated columns PROPDMGEXP and CROPDMGEXP, including alphabetical variables that represent the amount of exponential to be multiplied by the values in columns themselves. Let's first check what type of characters exist in PROPDMGEXP.
 

```r
levels(as.factor(inputData$PROPDMGEXP))
```

```
##  [1] ""  "-" "?" "+" "0" "1" "2" "3" "4" "5" "6" "7" "8" "B" "h" "H" "K"
## [18] "m" "M"
```

After some search in Google, the good discussion of the association of these alphabetical characters with the exponential values, supported by the proofs, is found [here](https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html). According to this webpage, the values of alphabetical characters are as the following:

* **B,b = billions** = 10^9^ 
* **M,m = millions** = 10^6^ 
* **K,k = thousands** = 10^3^ 
* **H,h = hundreds** = 10^2^
* **numeric 0..8** = 10^1^
* **(+)** = 10^0^
* **(-, ?, "")** = 0

For example, if the number from PROPDMG is 10, and the associated row in PROPDMGEXP is "K", then in order to calculate the damage, 10 should be multiplied by 1000 which is the number associated to K. Then the final amount in dollars would be 10*1000 = 10000 USD.

Let's define a simple function to do the conversion:


```r
convert <- function(x) {
    value = toupper(as.character(x))
    switch(value,
           "+" = 1e0,
           "0" = 1e1,
           "1" = 1e1,
           "2" = 1e1,
           "3" = 1e1,
           "4" = 1e1,
           "5" = 1e1,
           "6" = 1e1,
           "7" = 1e1,
           "8" = 1e1,
           "H" = 1e2,
           "K" = 1e3,
           "M" = 1e6,
           "B" = 1e9,
           0)
}

# Do the conversion for PROPDMGEXP and PROPDMGEXP and multiply them with the associated columns to calculate the amount in dollars for the damage. The amount in dollars for these two categories are kept in new columns "propDamage" and "cropDamage". Selecting only these columns and the EVTYPE in the end.
damageData <- inputData %>% rowwise() %>% mutate(propDamage = PROPDMG * convert(PROPDMGEXP), cropDamage = CROPDMG * convert(CROPDMGEXP)) %>% select(EVTYPE,propDamage, cropDamage)

head(damageData)
```

```
## # A tibble: 6 × 3
##    EVTYPE propDamage cropDamage
##     <chr>      <dbl>      <dbl>
## 1 TORNADO      25000          0
## 2 TORNADO       2500          0
## 3 TORNADO      25000          0
## 4 TORNADO       2500          0
## 5 TORNADO       2500          0
## 6 TORNADO       2500          0
```

Now the dataset "damageData" contains the amount in dollars for property damage (in propDamage) and crop damage (in cropDamage). We can now sum the amount of damage per event type to determine the total amount in USD for property and crop damages per event.


```r
# For each event type, sum property damage and for each event type sum crop damage
damageSummary <- summarise(group_by(damageData,EVTYPE),propDamTot=sum(propDamage,na.rm = TRUE), cropDamTot=sum(cropDamage,na.rm = TRUE))

# Order the event types in a descending total amount of property damage
sumProp <- damageSummary %>% arrange(desc(propDamTot))

# Select only first 10 leading events for property damage and select only related data columns
sumProp <- sumProp[1:10,1:2]
sumProp
```

```
## # A tibble: 10 × 2
##               EVTYPE   propDamTot
##                <chr>        <dbl>
## 1              FLOOD 144657709800
## 2  HURRICANE/TYPHOON  69305840000
## 3            TORNADO  56937162897
## 4        STORM SURGE  43323536000
## 5        FLASH FLOOD  16140815011
## 6               HAIL  15732269877
## 7          HURRICANE  11868319010
## 8     TROPICAL STORM   7703890550
## 9       WINTER STORM   6688497260
## 10         HIGH WIND   5270046280
```

```r
# Order the event types in a descending total amount of crop damage
sumCrop <- damageSummary %>% arrange(desc(cropDamTot))

# Select only first 10 leading events for crop damage and select only related data columns
sumCrop <- sumCrop[1:10,c(1,3)]
sumCrop
```

```
## # A tibble: 10 × 2
##               EVTYPE  cropDamTot
##                <chr>       <dbl>
## 1            DROUGHT 13972566000
## 2              FLOOD  5661968450
## 3        RIVER FLOOD  5029459000
## 4          ICE STORM  5022113500
## 5               HAIL  3025954650
## 6          HURRICANE  2741910000
## 7  HURRICANE/TYPHOON  2607872800
## 8        FLASH FLOOD  1421317100
## 9       EXTREME COLD  1292973000
## 10      FROST/FREEZE  1094086000
```

```r
# Plot the total amount of property damage and crop damage in millions USD for 10 leading events
par(mfrow=c(1,2), mar=c(11,5,3,1))
barplot(as.numeric(sumProp$propDamTot)*1e-6,names.arg=sumProp$EVTYPE,las=2,col="magenta", ylim=c(0, 1.8e5), main="Total Property Damage for the Top 10 Leading Events", cex.main=0.6)
title(ylab="Total Property Damage (in millions USD)", line=3.5) 
barplot(as.numeric(sumCrop$cropDamTot)*1e-6,names.arg=sumCrop$EVTYPE,las=2,col="magenta", ylim=c(0, 1.5e4), main="Total Crop Damage for the Top 10 Leading Events", cex.main=0.6)
title(ylab="Total Crop Damage (in millions USD)", line=3.4) 
```

![](Explore_storm_files/figure-html/analyseDamage-1.png)<!-- -->

The left distribution shows that the three events that caused the biggest property damage are "flood", "typhoon/hurricane" and "tornado". On the right distribution, it is seen that the three events that cause the biggest crop damage are "drought", "flood" and "river flood".
