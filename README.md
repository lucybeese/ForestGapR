![](https://github.com/carlos-alberto-silva/ForestGapR/blob/master/readme/fig_1.png)<br/>

![Github](https://img.shields.io/badge/Github-0.0.1-green.svg) ![licence](https://img.shields.io/badge/Licence-GPL--3-blue.svg) 

ForestGapR: An R Package for Airborne Laser Scanning-derived Tropical Forest Gaps Analysis 

The GapForestR package provides functions to i) automate canopy gaps detection, ii) compute  a serie of forest canopy gap statistics, including gap-size frequency distributions and iii) map gap dynamics (when multi-temporal ALS data are available), and convert the data among spatial formats.

## Installation
```r
  - the development version:
    devtools::install_github(“forestcas/forestGap”)
```    

## Getting Started

### Forest Canopy Gap detection
```r
#Loading raster and viridis library
library(raster)
library(viridis)

# ALS-derived CHM over Adolpho Ducke Forest Reserve - Brazilian tropical forest
data(ALS_CHM_DUC)

# Plotting chm
plot(ALS_CHM_DUC, col=viridis(10))

# set height tresholds (e.g. 10 meters)
threshold<-10
size<-c(1,1000) # m2

# Detecting forest gaps
gaps_duc<-getForestGaps(chm_layer=ALS_CHM_DUC, threshold=threshold, size=size)

# Ploting gaps
plot(gaps_duc, col="red", add=TRUE, main="Forest Canopy Gap", legend=FALSE)
```
![Net 1 plot](https://github.com/carlos-alberto-silva/ForestGapR/blob/master/readme/fig_2.png)

### Forest canopy Gaps Stats
This function computes a serie of forest canopy gap statistics

List of forest gaps statistics:
*gap_id: gap id
*gap_area - area of gap (m2)
*chm_max - Maximum canopy height (m) within gap boundary
*chm_min - Minimum canopy height (m) within gap boundary
*chm_mean - Mean canopy height (m) within gap boundary
*chm_sd - Standard Deviation of canopy height (m) within gap boundary
*chm_range - Range of canopy he ight (m) within gap boundary

```r
#Loading raster library
library(raster)

# ALS-derived CHM over Adolpho Ducke Forest Reserve - Brazilian tropical forest
data(ALS_CHM_DUC)

# set height tresholds (e.g. 10 meters)
threshold<-10
size<-c(5,1000) # m2

# Detecting forest gaps
gaps_duc<-getForestGaps(chm_layer=ALS_CHM_DUC, threshold=threshold, size=size)

# Computing basic statistis of forest gap
gaps_stats<-GapStats(gap_layer=gaps_duc, chm_layer=ALS_CHM_DUC)
```
    ##    gap_id gap_area chm_max chm_min chm_mean chm_sd chm_range
    ## 1       1       34    9.22    1.09     5.12   2.61      8.13
    ## 2       2        6    8.17    6.06     7.40   0.74      2.11
    ## 3       3        5    9.96    7.42     8.85   1.23      2.54
    ## 4       4       32    9.91    4.42     8.12   1.69      5.49
    ## 5       5       11    9.83    6.23     8.48   1.09      3.60
    ## 6       6       44    9.72    1.92     7.31   1.60      7.80
    ## 7       7        6    9.88    8.81     9.49   0.40      1.07
    ## 8       8        6    9.07    3.10     7.02   2.96      5.97
    ## 9       9       10    9.52    2.86     8.03   2.22      6.66
    ## 10     10       18    9.90    2.74     5.06   2.18      7.16
    ## 11     11       13    9.91    1.75     5.47   2.94      8.16
    ## 12     12       10    9.92    3.75     7.77   2.27      6.17
    ## 13     13       66    9.94    0.99     5.31   2.91      8.95
    ## 14     14        7   10.00    5.83     7.41   1.53      4.17
    ## 15     15       12    9.65    5.61     7.97   1.43      4.04
    ## 16     16        7    8.64    5.64     7.67   0.97      3.00
    ## 17     17       21    8.42    0.40     6.02   2.23      8.02
    ## 18     18        6    7.39    3.37     5.03   1.82      4.02
    ## 19     19        5    9.07    4.91     7.74   1.65      4.16
    ## 20     20       36    9.90    2.10     6.62   2.45      7.80
    ## 21     21        5    9.71    8.43     9.19   0.57      1.28
    ## 22     22       12    9.83    7.42     8.39   0.85      2.41
    ## 23     23       15    9.25    7.81     8.56   0.48      1.44
    ## 24     24       27    9.43    0.26     2.37   2.55      9.17
    ## 25     25        5    4.54    2.43     3.78   0.80      2.11
    ## 26     26        7    9.98    6.34     8.40   1.07      3.64
    ## 27     27       25    9.76    3.78     7.67   1.13      5.98
    ## 28     28        6    9.49    4.92     7.23   1.56      4.57
    ## 29     29       22    9.76    3.78     5.96   1.97      5.98
    ## 30     30        6    5.73    2.69     4.46   1.28      3.04
    ## 31     31        7    9.41    7.72     8.44   0.56      1.69
    ## 32     32       57    9.89    1.97     5.70   2.62      7.92
    ## 33     33       38    9.68    0.25     4.58   2.07      9.43
    ## 34     34        8    9.83    4.88     6.55   1.54      4.95
    ## 35     35        6    9.66    8.26     9.16   0.48      1.40
    
 
### Forest Canopy Gap-size Frequency Distributions
<img align="right" src="https://github.com/carlos-alberto-silva/ForestGapR/blob/master/readme/fig_3.png">

```r
#Loading raster library
library(raster)

# ALS-derived CHM over Adolpho Ducke Forest Reserve - Brazilian tropical forest
data(ALS_CHM_DUC)

# set height tresholds (e.g. 10 meters)
threshold<-10
size<-c(1,1000) # m2

# Detecting forest gaps
gaps_duc<-getForestGaps(chm_layer=ALS_CHM_DUC, threshold=threshold, size=size)

# Computing basic statistis of forest gap
gaps_stats<-GapStats(gap_layer=gaps_duc, chm_layer=ALS_CHM_DUC)

# Gap-size Frequency Distributions
GapSizeFDist(gaps_stats=gaps_stats, col="forestgreen", pch=16, cex=1,
axes=FALSE,ylab="Gap Frequency",xlab=as.expression(bquote("Gap Size" ~ (m^2) )))
axis(1);axis(2)
grid(4,4)
```

### Forest Gaps Changes Detection
```r
#Loading raster and viridis libraries
library(raster)
library(viridis)

# ALS-derived CHM from Fazenda Cauxi - Brazilian tropical forest
data(ALS_CHM_CAU_2012)
data(ALS_CHM_CAU_2014)

# set height tresholds (e.g. 10 meters)
threshold<-10
size<-c(1,1000) # m2

# Detecting forest gaps
gaps_cau2012<-getForestGaps(chm_layer=ALS_CHM_CAU_2012, threshold=threshold, size=size)
gaps_cau2014<-getForestGaps(chm_layer=ALS_CHM_CAU_2014, threshold=threshold, size=size)

# Detecting forest gaps changes
Gap_changes<-GapChangeDec(gap_layer1=gaps_cau2012,gap_layer2=gaps_cau2014)

# Plotting ALS-derived CHM and forest gaps
par(mfrow=c(1,3))
plot(ALS_CHM_CAU_2012, main="Forest Canopy Gap - 2012", col=viridis(10))
plot(gaps_cau2012, add=TRUE, col="red", legend=FALSE)

plot(ALS_CHM_CAU_2014,  main="Forest Canopy Gap - 2014", col=viridis(10))
plot(gaps_cau2014, add=TRUE,col="blue", legend=FALSE)

plot(ALS_CHM_CAU_2014,main="Forest Gaps Changes Detection",col=viridis(10))
plot(Gap_changes, add=TRUE, col="yellow", legend=FALSE)
```
![Net 1 plot](https://github.com/carlos-alberto-silva/ForestGapR/blob/master/readme/fig_4.png)

### references
Asner, G.P., Kellner, J.R., Kennedy-Bowdoin, T., Knapp, D.E., Anderson, C. & Martin, R.E. (2013). Forest canopy     gap distributions in the Southern Peruvian Amazon. PLoS One, 8, e60875.
  
White EP, Enquist BJ, Green JL (2008) On estimating the exponent of powerlaw frequency distributions. Ecology 89:   905–912.
  
### Acknowledgements

ALS data from Adolfo Ducke (ALS_CHM_DUC) Forest Reserve and Cauaxi Forest (ALS_CHM_CAU_2012 and ALS_CHM_CAU_2014) used as exemple datasets were acquired by the Sustainable Landscapes Brazil project supported by the Brazilian Agricultural Research Corporation (EMBRAPA), the US Forest Service, USAID, and the US Department of State. 