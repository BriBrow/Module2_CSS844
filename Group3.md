---
title: "Group 3: Potato data"
author: "Abby Bryson, Brianna Brown, Paulo Izquierdo, Sikta Das Adhikari, Wendy Leuenberger"
date: "2022-03-30"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
---

## Set up 



### Load packages 


```r
# Load packages
library(tidyverse)  # General data wrangling
library(magrittr)   # Piping. %>% = "and then", %<>% saves over
library(readxl)     # Read in excel spreadsheets (.xlsx)
library(prospectr)  # For managing wavelengths, derivative
```

### ggplot settings


```r
tbw <- theme_bw(base_size = 18)
th <- theme(panel.grid = element_blank())
nol <- theme(legend.position = 'none')
xl <- xlab('Wavelength (nm)')
yl <- ylab('First derivative')
```

## Initial data management

### Read in data


```r
# Folder containing data, mapped on Wendy's computer. 
# Would need to change for different people.
DataLocation <- 'C:\\Users\\Wendy\\OneDrive - Michigan State University\\CSS 844\\Module 2\\Potatoes_8_26_20'

# Read in the potato data
# RAUDPC = relative area under disease progress curve
# First four rows are metadata; don't read them in 
RAUDPC <- read_excel(paste0(DataLocation, '\\2020 Potato Late Blight Trial RAUDPC 20220224.xlsx'), 
                     skip = 4, na = '.')

# Read in hyperspectral data
# Big file - 153 rows x 1394 columns. Won't need all columns
spect <- read_csv(paste0(DataLocation, '\\plotwise_values_new.csv'))

# NM 
vnir <- read_excel(paste0(DataLocation, 
                          '\\VNIR\ Band\ Name\ and\ range.xlsx'),
                   col_names = c('Layer', 'nm', 'Units'))

## Combined data
# We will create this document in the following code
# Can use this code to read the formatted data into other R scripts
# AllData <- read_csv('CombinedPotatoData.csv')
```

### Take a look at data


```r
# Disease data
RAUDPC %>% head
```

```
## # A tibble: 6 x 40
##     ROW TIER  BLOCK    PLOT    LINE    Rep `LB%_dpi_0` `LB%_dpi_23` `LB%_dpi_26`
##   <dbl> <chr> <chr>    <chr>   <chr> <dbl>       <dbl>        <dbl>        <dbl>
## 1     1 ALL   Spreader Spread~ Atla~     1           0            5          7.5
## 2     5 ALL   Spreader Spread~ Atla~     2           0            5          5  
## 3     8 ALL   Spreader Spread~ Atla~     3           0            2         10  
## 4     2 1     1        101     MSU-~     1           0            0          0  
## 5     2 2     1        104     MSU-~     1          NA           NA         NA  
## 6     2 3     1        107     MSU-~     1           0            0          0  
## # ... with 31 more variables: `LB%_dpi_30` <dbl>, `LB%_dpi_37` <dbl>,
## #   `LB%_dpi_43` <dbl>, `LB%_dpi_47` <dbl>, `LB_AUDC-1` <dbl>,
## #   `LB_AUDC-2` <dbl>, `LB_AUDC-3` <dbl>, `LB_AUDC-4` <dbl>, `LB_AUDC-5` <dbl>,
## #   `LB_AUDC-6` <dbl>, LB_AUDC_total <dbl>, LB_Area_total <dbl>,
## #   LB_RAUDPC <dbl>, LB_RAUDPCx100 <dbl>, `EB%_dpi_0` <dbl>,
## #   `EB%_dpi_23` <dbl>, `EB%_dpi_26` <dbl>, `EB%_dpi_30` <dbl>,
## #   `EB%_dpi_37` <dbl>, `EB%_dpi_43` <dbl>, `EB%_dpi_47` <dbl>, ...
```

```r
# Hyperspectral data
spect %>% head
```

```
## # A tibble: 6 x 1,394
##     fid id      Row Range Block Line              Rep PlotNumber Pixels Red_mean
##   <dbl> <chr> <dbl> <dbl> <dbl> <chr>           <dbl>      <dbl>  <dbl>    <dbl>
## 1    NA S1-1      1     1    NA AtlanticSpread~    NA         NA   1249    116. 
## 2    NA S1-11     1    11    NA AtlanticSpread~    NA         NA   3215     95.4
## 3   184 S1-12     1    12    NA AtlanticSpread~    NA         NA   3218     89.8
## 4    NA S1-16     1    16    NA AtlanticSpread~    NA         NA   3188     92.5
## 5   260 S1-18     1    18    NA AtlanticSpread~    NA         NA   2468    112. 
## 6    NA S1-23     1    23    NA AtlanticSpread~    NA         NA   3124    102. 
## # ... with 1,384 more variables: Red_median <dbl>, Red_stdev <dbl>,
## #   Red_min <dbl>, Red_max <dbl>, Green_mean <dbl>, Green_medi <dbl>,
## #   Green_stde <dbl>, Green_min <dbl>, Green_max <dbl>, Blue_mean <dbl>,
## #   Blue_media <dbl>, Blue_stdev <dbl>, Blue_min <dbl>, Blue_max <dbl>,
## #   `1_mean` <dbl>, `2_mean` <dbl>, `3_mean` <dbl>, `4_mean` <dbl>,
## #   `5_mean` <dbl>, `6_mean` <dbl>, `7_mean` <dbl>, `8_mean` <dbl>,
## #   `9_mean` <dbl>, `10_mean` <dbl>, `11_mean` <dbl>, `12_mean` <dbl>, ...
```

```r
# Layer names -> wavelengths (nm)
vnir %>% head
```

```
## # A tibble: 6 x 3
##   Layer      nm Units
##   <chr>   <dbl> <chr>
## 1 Layer_1  398. nm   
## 2 Layer_2  400. nm   
## 3 Layer_3  403. nm   
## 4 Layer_4  405. nm   
## 5 Layer_5  407. nm   
## 6 Layer_6  409. nm
```

Add a wavelength column to vnir that matches the other files. Make that column just have the number, not Layer_number


```r
vnir %<>% 
  mutate(Wavelength = Layer %>% str_remove('Layer_'))
```


### Which columns contain the same information

Output not shown - a bunch of TRUE/FALSE to show if the columns line up. 


```r
colnames(spect)[1:20]
RAUDPC[1:6]

# Column names aren't the same
colnames(RAUDPC) %in% colnames(spect) 

# Which columns correspond
# Rep is similar
spect$Rep %in% RAUDPC$Rep

# Rows have same data
spect$Row %in% RAUDPC$ROW

# Lines have mostly the same data
spect$Line %in% RAUDPC$LINE
RAUDPC$LINE %in% spect$Line

# Blocks have mostly the same data
spect$Block %in% RAUDPC$BLOCK

# PlotNumber and PLOT seem like mostly the same data
spect$PlotNumber %in% RAUDPC$PLOT
RAUDPC$PLOT %in% spect$PlotNumber

# Tier and range appear to be the same
spect$Range %in% RAUDPC$TIER
```

### Prep data for join


```r
# Let's use Rep and Plot to join
# Need to make sure the information lines up
# Column names - join by PLOT
spect %<>% rename(PLOT = PlotNumber)

# Make the Line values line up
spect$PLOT[spect$Line == 'AtlanticSpreader'] <- 'Spreader'

# Make the Reps line up
spect$Rep[spect$Line == 'AtlanticSpreader' &
            spect$Row == 1] <- 1
spect$Rep[spect$Line == 'AtlanticSpreader' &
            spect$Row == 5] <- 2
spect$Rep[spect$Line == 'AtlanticSpreader' &
            spect$Row == 8] <- 3

# Take a look (results not shown)
spect %>% head
RAUDPC %>% head
```

### Join data


```r
# Join
AllData <- RAUDPC %>% 
  full_join(spect)
# Move rows so index columns are all together
AllData %<>% 
  relocate(c(fid, id, Row, Range, Block, Line, Pixels),
           .after = Rep)

# Make sure each plot has a unique identifier
AllData$UniqueValue <- paste(AllData$PLOT, AllData$id, sep = '_')

# Save as csv in case it's helpful
# write_csv(AllData, file = 'CombinedPotatoData.csv')
```

## Data processing for analyses

### Functions

`MakeLong()`: Change data to long format

`PlotSummary()`: Take mean of the replicate plots and organize for plotting

`Derivative()`: Take the first derivative of the the wavelength data

`PrepPCA()`: Subset the data to work with the PCA

`RunPCA()`: Fit the PCA using the prepared data

`Classify()`: Use a threshold to determine 1 (blight) or 0 (not blight) categories for a particular date.


```r
# Pull out values, change into long format

MakeLong <- function(Data = data, Value = value){
  
  NewData <- Data %>% 
    # Pull out the columns with early blight data (EB)
    # Pull out the columns with a number and the provided value
    # I.e. Value = _mean, pulls out 1_mean to 274_mean
    select(UniqueValue, PLOT, LINE, Rep, id, Pixels,
           starts_with('EB'), num_range(1:274, Value))
  
  # Change from wide to long format. This part only changes the 
  # wavelengths to long. We may need to also change EB data
  NewData %<>% 
    pivot_longer(ends_with(Value), names_to = 'Wavelength',
                 values_to = 'Value')
  
  # Remove the Value (i.e. _mean) from the wavelength column
  # Change to numeric for use as numeric/plotting
  NewData$Wavelength %<>% str_remove(Value)
  
  NewData %<>% 
    left_join(vnir[,c('Wavelength', 'nm')])
  
  return(NewData)
}


PlotSummary <- function(Data = data, Colname = colname){
  
  # Pull out just the data wanted for plotting
  # Might need to also pull out certain EB data
  # Take the mean of whichever value (mean among replicates)
  NewData <- Data %>% 
    group_by(UniqueValue, PLOT, id, Wavelength, nm) %>% 
    summarize(Name = mean(Value, na.rm = TRUE)) %>%
    # Summarize isn't good at variable column names
    # Change name below
    rename_at('Name', ~ Colname) %>% 
    arrange(Wavelength)
  
  return(NewData)
  
}


Derivative <- function(Data = data, Colname = colname){
  # Pull out just the wavelength numbers and values
  # Needs to be a dataframe (not tibble)
  NewData <- Data %>% 
    ungroup %>% 
    select(-PLOT, -id, -Wavelength) %>% 
    arrange(nm) %>% 
    pivot_wider(names_from = nm,
                values_from = all_of(Colname)) %>% 
    as.data.frame
  
  # Make the UniqueValues into rownames
  # Only numeric values in data are allowed for derivative functions
  rownames(NewData) <- NewData$UniqueValue
  NewData$UniqueValue <- NULL
  
  # Take the first derivative
  Deriv <- t(diff(t(NewData), differences = 1)) %>% 
    as.data.frame()
  
  # Put the unique values back into the dataset
  Deriv$UniqueValue <- rownames(Deriv)
  
  # Make into a long version for plotting/manipulation
  DerivLong <- Deriv %>% 
    pivot_longer(cols = -UniqueValue,
                 names_to = 'nm',
                 values_to = 'Derivative',
                 names_transform = list(nm = as.numeric)) %>% 
    arrange(nm)
  
  # Return the long form
  return(DerivLong)
}

# Prepare the data for a PCA using data of particular wavelengths
PrepPCA <- function(Data, Min_nm, Max_nm){
  NewData <- Data %>% 
    # Choose the wavelengths to include
    filter(nm > Min_nm & nm < Max_nm) %>% 
    # Pull out just the necessary columns
    select(UniqueValue, Value, nm) %>% 
    # Remove NAs
    filter(!is.na(Value)) %>% 
    # Change into wide format as a prerequisite to the matrix
    pivot_wider(values_from = Value, names_from = nm)
  # Return the wide form
  return(NewData)
}

# Run the PCA on the simplified data
RunPCA <- function(Data, IDcol){
  # Remove the non-numeric identifying column 
  # (only numbers allowed in the matrix)
  JustNumbers <- Data %>% 
    select(-IDcol)
  # Run the PCA
  PCA <- JustNumbers %>% 
    prcomp
  # Pull out the two most meaningful PCs
  # Add the identifying column back in
  PCA_Data <- data.frame(
    PC1 = PCA$x[,1],
    PC2 = PCA$x[,2],
    label = Data[,IDcol]
  )
  # Return the PCA + identifying information
  return(PCA_Data)
}

# Change continuous data into classified 0/1 data
Classify <- function(Data, OrigColumn, NewColumn, Threshold){
  # Make new object (safer than working with original)
  Newdata <- Data
  # Create new column with 1/0 data
  Newdata[,NewColumn] <- ifelse(Newdata[,OrigColumn] > Threshold, 
                                1, 0)
  # Return data with the added column
  return(Newdata)
}
```

### Make datasets


```r
# Mean values
MeanLong <- MakeLong(AllData, '_mean')
MeanPlot <- PlotSummary(Data = MeanLong, Colname = 'MeanValue')
MeanDeriv <- Derivative(MeanPlot, Colname = 'MeanValue')
MeanPrep <- PrepPCA(Data = MeanLong, Min_nm = 611, Max_nm = 784)
MeanPCA <- RunPCA(Data = MeanPrep, IDcol = 'UniqueValue')

# Median values
MedianLong <- MakeLong(AllData, '_median')
MedianPlot <- PlotSummary(Data = MedianLong,
                          Colname = 'MedianValue')
MedianDeriv <- Derivative(MedianPlot, Colname = 'MedianValue')
```




### Initial plots of wavelengths

Here's plots with 10 random and all of the plots. The 10 random plots will change every time the code is run.


```r
Ten <- unique(AllData$UniqueValue) %>% sample(10)

# Plot
ggplot(MeanPlot %>% filter(UniqueValue %in% Ten),
       aes(x = nm, y = MeanValue, 
                     color = UniqueValue)) +
  geom_line() + th + tbw + xl + #nol +
  labs(y = 'Mean value', 
       title = 'Mean value of 10 random plots') 
```

![](Group3_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
ggplot(MedianPlot %>% filter(UniqueValue %in% Ten),
       aes(x = nm, y = MedianValue, 
                     color = UniqueValue)) +
  geom_line() + th + tbw + xl + 
  labs(y = 'Median value',
       title = 'Median value of 10 random plots')
```

![](Group3_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```r
# Mean all plots
ggplot(MeanPlot,
       aes(x = nm, y = MeanValue, 
                     color = UniqueValue)) +
  geom_line() + th + tbw + nol + xl +
  labs(y = 'Mean value',
       title = 'Mean value of all plots')
```

![](Group3_files/figure-html/unnamed-chunk-6-3.png)<!-- -->

```r
# Median all plots
ggplot(MedianPlot,
       aes(x = nm, y = MedianValue, 
                     color = UniqueValue)) +
  geom_line() + th + tbw + nol + xl +
  labs(y = 'Median value',
       title = 'Median value of all plots')
```

![](Group3_files/figure-html/unnamed-chunk-6-4.png)<!-- -->

### Troubleshooting

Plots with low wavelength values

Problem solved with new data (3/27/2022), so this section isn't needed. Code is still in the document but isn't run.




### Derivatives





```r
# ggplot(d1_rl_long %>% filter(UniqueValue %in% Ten[1]),
#        aes(x = nm, y = Derivative, color = UniqueValue)) +
#   geom_line() + th + tbw + nol

ggplot(MeanDeriv %>% filter(UniqueValue %in% Ten),
       aes(x = nm, y = Derivative, color = UniqueValue)) +
  geom_line() + th + tbw + nol + yl + xl +
  ggtitle('First derivative of mean values at 10 random plots')
```

![](Group3_files/figure-html/DerivPlots-1.png)<!-- -->

```r
ggplot(MeanDeriv,
       aes(x = nm, y = Derivative, color = UniqueValue)) +
  geom_line() + th + tbw + nol + yl + xl +
  ggtitle('First derivative of mean values')
```

![](Group3_files/figure-html/DerivPlots-2.png)<!-- -->

## PCA



### Variation explained by PCA

```r
JustNumbers <- MeanPrep %>%
  select(-UniqueValue)
  # Run the PCA
PCA <- JustNumbers %>% 
  prcomp

summary(PCA)$importance[1:3,1:5]
```

```
##                              PC1        PC2        PC3        PC4         PC5
## Standard deviation     0.2524319 0.06700849 0.02439324 0.01278637 0.008460772
## Proportion of Variance 0.9223000 0.06499000 0.00861000 0.00237000 0.001040000
## Cumulative Proportion  0.9223000 0.98729000 0.99590000 0.99827000 0.999310000
```



```r
ggplot(MeanPCA, aes(x = PC1, y = PC2)) +
  geom_point() 
```

![](Group3_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


## Classify EB


```r
# Note: Threshold is >, not >=

# Quantiles for choosing a threshold
# quantile(AllData$`EB%_dpi_0`, na.rm = TRUE)
# quantile(AllData$`EB%_dpi_23`, na.rm = TRUE)
# quantile(AllData$`EB%_dpi_26`, na.rm = TRUE)
# quantile(AllData$`EB%_dpi_30`, na.rm = TRUE)
# quantile(AllData$`EB%_dpi_37`, na.rm = TRUE)
# quantile(AllData$`EB%_dpi_43`, na.rm = TRUE)
quantile(AllData$`EB%_dpi_47`, na.rm = TRUE)
```

```
##    0%   25%   50%   75%  100% 
##  2.00 10.00 25.00 38.75 80.00
```

```r
Mean23 <- Classify(MeanLong, 'EB%_dpi_23', 'EB_23', 10)
Mean26 <- Classify(MeanLong, 'EB%_dpi_26', 'EB_26', 10)

# Mean26 %>% 
#   select(`EB%_dpi_26`, EB_26) %>% 
#   unique

Mean47 <- Classify(AllData, OrigColumn = 'EB%_dpi_47', 
                   NewColumn = 'EB_47', Threshold = 24)
Mean47 %>% 
  select(`EB%_dpi_47`, EB_47) %>% 
  unique
```

```
## # A tibble: 16 x 2
##    `EB%_dpi_47` EB_47
##           <dbl> <dbl>
##  1           NA    NA
##  2           25     1
##  3           40     1
##  4           80     1
##  5           15     0
##  6           30     1
##  7           20     0
##  8           10     0
##  9            5     0
## 10            3     0
## 11            7     0
## 12           75     1
## 13            2     0
## 14           50     1
## 15           60     1
## 16           35     1
```

## Logistic regression


```r
# Add the PC1 and PC2 values to the data frame
Mean47 %<>% left_join(MeanPCA)

Accuracy <- vector(mode = 'numeric', length = 1000)

for(tt in 1:1000){

  # Rownames for the 80% of training data
  Include80 <- sample(x = length(Mean47$EB_47), 
                      size = 0.8 * length(Mean47$EB_47))
  Mean47_Train <- Mean47[Include80,]
  Mean47_Test <- Mean47[-Include80,]

  # 80/20
  MeanLogistic <- glm(EB_47 ~ PC1 + PC2, 
                      family = 'binomial',
                      data = Mean47_Train)
  summary(MeanLogistic)
  
  Mean47_Test %<>% 
    select(EB_47, PC1, PC2)

  Mean47_Test$Pred <- predict(MeanLogistic, 
                              newdata = Mean47_Test,
                              type = 'response')

  Mean47_Test$Pred01 <- ifelse(Mean47_Test$Pred >= 0.5, 1, 0)

  Correct <- sum(Mean47_Test$EB_47 == Mean47_Test$Pred01, 
                 na.rm = TRUE)
  Total <- Mean47_Test %>% filter(!is.na(EB_47)) %>% dim
  # print(paste(Correct, 'correct out of', Total[1]))
  Accuracy[tt] <- Correct/Total[1]
}

# Note - these values will change each time the markdown is run. 
# The means and medians should stay about the same.
Accuracy %>% summary
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.3478  0.5769  0.6400  0.6373  0.6957  0.8800
```


```r
AccuracyDF <- tibble(Accuracy = Accuracy,
                     Index = 1:length(Accuracy))
ggplot(AccuracyDF, aes(y = Accuracy, x = Index)) + 
  geom_point() +
  geom_hline(aes(yintercept = mean(Accuracy)), color = 'red') +
  tbw + th 
```

![](Group3_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


## Poisson regression

Similar relationship. We didn't include this model in our final presentation.


```r
MeanPoisson <- glm(`EB%_dpi_47` ~ PC1 + PC2,
                   family = 'poisson',
                   data = Mean47)
summary(MeanPoisson)
```

```
## 
## Call:
## glm(formula = `EB%_dpi_47` ~ PC1 + PC2, family = "poisson", data = Mean47)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -6.3021  -3.0080  -0.6356   1.9798   7.3919  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  3.15393    0.02036 154.921  < 2e-16 ***
## PC1         -0.91808    0.06843 -13.417  < 2e-16 ***
## PC2         -2.42461    0.30450  -7.963 1.68e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 1496.0  on 115  degrees of freedom
## Residual deviance: 1216.3  on 113  degrees of freedom
##   (41 observations deleted due to missingness)
## AIC: 1771.6
## 
## Number of Fisher Scoring iterations: 5
```

```r
plot(MeanPoisson)
```

![](Group3_files/figure-html/unnamed-chunk-15-1.png)<!-- -->![](Group3_files/figure-html/unnamed-chunk-15-2.png)<!-- -->![](Group3_files/figure-html/unnamed-chunk-15-3.png)<!-- -->![](Group3_files/figure-html/unnamed-chunk-15-4.png)<!-- -->

## Make PCA plot with classified data


```r
# Classified version
ggplot(Mean47, aes(x = PC1, y = PC2, color = factor(EB_47))) +
  geom_point() +
  labs(color = 'Early blight day 47') +
  tbw + th
```

![](Group3_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```r
# Continuous version
# NA = gray
ggplot(Mean47, aes(x = PC1, y = PC2, color = `EB%_dpi_47`)) +
  geom_point() +
  labs(color = 'Early blight day 47')
```

![](Group3_files/figure-html/unnamed-chunk-16-2.png)<!-- -->


<!-- ## Can we use python? -->



