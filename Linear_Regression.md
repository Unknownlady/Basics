#### Exercise 1: The objective of this exercise is to fit a model predicting energy consumed per capita (energy) from the percentage of residents living in metropolitan areas (metro).The dataset used includes data about various states in the USA comprising of 51 states with 21 characteristics (region,population,density,etc).

We begin by inspecting our dataset.

``` r
 setwd("~/Git/Rstatistics")
 states.data0 <- readRDS("datasets/states.rds")
  states.info0 <- data.frame(attributes(states.data0)[c("names", "var.labels")])
 head(states.info0,5) 
```

    ##     names              var.labels
    ## 1   state                   State
    ## 2  region     Geographical region
    ## 3     pop         1990 population
    ## 4    area Land area, square miles
    ## 5 density  People per square mile

As we can see,the first 5 characteristics are state,geographical region,population in the year 1990(pop),area of the land(in square miles) and people per square mile(density).

Since we want to fit a model that describes the relationship between the variables energy and metro, we extract this data from the dataset using the subset function.

``` r
  sts.ex.sat0 <- subset(states.data0,select = c("energy","metro"))
 summary(sts.ex.sat0)
```

    ##      energy          metro       
    ##  Min.   :200.0   Min.   : 20.40  
    ##  1st Qu.:285.0   1st Qu.: 46.98  
    ##  Median :320.0   Median : 67.55  
    ##  Mean   :354.5   Mean   : 64.07  
    ##  3rd Qu.:371.5   3rd Qu.: 81.58  
    ##  Max.   :991.0   Max.   :100.00  
    ##  NA's   :1       NA's   :1

The summary function is used above to get different measures which descibe the type of distributions that the two datasets(energy and metro) have. The table shows that the minimum value of energy consumed per capita is 200 (city:New York) whereas the minimum percent of residents living in metropolitan cities is 20% (city:Idaho). Similarly,the maximum values for energy and metro are 991.0 and 100%.

We then use the correlation function to establish a relationship between energy and metro variables.

``` r
cor(sts.ex.sat0)
```

    ##        energy metro
    ## energy      1    NA
    ## metro      NA     1

``` r
cor(sts.ex.sat0,use = "complete")
```

    ##            energy      metro
    ## energy  1.0000000 -0.3397445
    ## metro  -0.3397445  1.0000000

As you can see, the first command gives a table with NA values.This does not give us a clear picture of the correlation between two variables.This happens because all the missing values in the datasets in taken into consideration implicitly. To prevent usage of NA(missing) values, we put use="complete" in the correlation function.

From the table we observe that the correlation between energy and metro is -0.3397445.This implies that there is a negative relationship between energy and metro. As the percentage of residents living in metropolitan cities(metro) increase,the minimum value of energy consumed per capita(energy) decreases.

This can be further explained by plotting the data. ![](Linear_Regression_files/figure-markdown_github/Plot%20data-1.png)

The plot,however does not explain this relationship clearly. For a better understanding of our dataset, we fit our regression model and study the different values to establish a relationship.

``` r
sat.mod0 <- lm(energy~metro, data = states.data0)
summary(sat.mod0) 
```

    ## 
    ## Call:
    ## lm(formula = energy ~ metro, data = states.data0)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -215.51  -64.54  -30.87   18.71  583.97 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 501.0292    61.8136   8.105 1.53e-10 ***
    ## metro        -2.2871     0.9139  -2.503   0.0158 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 140.2 on 48 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.1154, Adjusted R-squared:  0.097 
    ## F-statistic: 6.263 on 1 and 48 DF,  p-value: 0.01578

``` r
plot(sat.mod0)
```

![](Linear_Regression_files/figure-markdown_github/Regression%20model-1.png)![](Linear_Regression_files/figure-markdown_github/Regression%20model-2.png)![](Linear_Regression_files/figure-markdown_github/Regression%20model-3.png)![](Linear_Regression_files/figure-markdown_github/Regression%20model-4.png)

The plots show residuals in four different plots.

#### 1. Residuals vs Fitted

This plot shows if residuals have non-linear patterns.Since the residuals are equally spread around the horizontal line without distinct patterns,the relationship between metro and energy is non-linear. There are a few outliers (19,2 and 51) which if removed can give a better model that fits our data more accurately.

#### 2.Normal Q-Q

This plot shows if the datasets are normally distributed.For example, if we run a statistical analysis that assumes our dependent variable(energy) is Normally distributed, we can use a Normal Q-Q plot to check that assumption. Do residuals follow a straight line well or do they deviate severely? In our plot, most of the residuals follow a straight line but the outliers(51,19,2) are not following the line. These points could give be a problem in giving us the correct analysis.

#### 3.Scale Location

This plot shows if residuals are spread equally along the ranges of predictors. This can be used to check the assumption of equal variance (homoscedasticity).The residuals appear to be randomly spread and outliers(19,2,51) are not included in the model.

#### 4. Residuals vs Leverage

This plot helps us to find influential cases. Not all outliers are influential in linear regression analysis. Even though data has outliers, they may not be influential to determine a regression line. This implies that the results wouldn't be much different if we either include or exclude them from analysis. On the other hand, some cases could be very influential even if they look to be within a reasonable range of the values. They could be extreme cases against a regression line and can alter the results if we exclude them from analysis. In the plot shown above, all the data points are well inside the Cook's distance lines(red dotted lines). Hence, there are no influential cases.

We now select additional predictors to add to the model and establish a relationship between different predictors(as done above).

``` r
 summary(lm(energy~metro+density+area+region, data = states.data0))
```

    ## 
    ## Call:
    ## lm(formula = energy ~ metro + density + area + region, data = states.data0)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -131.52  -51.40  -12.91   16.60  434.31 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    3.727e+02  6.734e+01   5.535 1.73e-06 ***
    ## metro         -2.040e+00  9.048e-01  -2.255   0.0293 *  
    ## density        1.133e-01  1.090e-01   1.040   0.3043    
    ## area           1.144e-03  2.037e-04   5.615 1.32e-06 ***
    ## regionN. East -4.747e+01  6.427e+01  -0.739   0.4641    
    ## regionSouth    5.696e+01  4.351e+01   1.309   0.1975    
    ## regionMidwest  1.272e+01  4.511e+01   0.282   0.7793    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 106 on 43 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.5473, Adjusted R-squared:  0.4841 
    ## F-statistic: 8.663 on 6 and 43 DF,  p-value: 3.399e-06

``` r
summary(lm(energy~metro+area+pop, data = states.data0))
```

    ## 
    ## Call:
    ## lm(formula = energy ~ metro + area + pop, data = states.data0)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -134.89  -56.58  -24.79   35.03  461.32 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.599e+02  5.426e+01   6.634 3.24e-08 ***
    ## metro       -1.002e+00  8.701e-01  -1.151    0.256    
    ## area         1.119e-03  1.839e-04   6.087 2.15e-07 ***
    ## pop         -4.124e-06  3.458e-06  -1.192    0.239    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 106.6 on 46 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.5101, Adjusted R-squared:  0.4781 
    ## F-statistic: 15.97 on 3 and 46 DF,  p-value: 2.986e-07

A good model is decided by the values of adjusted R squared and multiple R squared. We need to ensure that overfitting is avoided and thus many variables are not added to the model. On trying various predictors like waste,density,region,etc.,it was observed that when area,density,region was added, multiple R squared value is 0.5473 and adjusted R squared is 0.4841.When expense was further added tp this mode, it was found that both multiple R squared and adjusted R squared increases and significance of area and metro remains the same.However, area,density,region are closely interrelated and this could lead to overfitting. To avoid this,we consider area,metro, density and region which gives us the same significance level with a slight change in values of multiple R squared(0.5473) and adjusted R squared(0.4841). This implies that percent of people living(metro), area,region and density are good predictors of energy per capita(energy).

#### Exercise 2: The aim of this ecercise is to generate an interaction term and test the interaction.Region is added to the model and difference in different region is considered.

Initially, we study the interaction between metro and density.
We add the interaction term to the model

``` r
energy.metro.interaction <- lm(energy~metro*density,data = states.data0)                    
coef(summary(energy.metro.interaction))
```

    ##                   Estimate   Std. Error   t value     Pr(>|t|)
    ## (Intercept)   514.10424265 72.513405133  7.089782 6.683817e-09
    ## metro          -1.72111951  1.135554671 -1.515664 1.364465e-01
    ## density        -1.43817898  0.911292059 -1.578176 1.213782e-01
    ## metro:density   0.01386147  0.009534258  1.453859 1.527747e-01

The estimate in this case is 0.01386147. As it is very close to 0, this variable must be removed from the model as it is not helping in predicting the dependent variable.

``` r
str(states.data0$region)
```

    ##  Factor w/ 4 levels "West","N. East",..: 3 1 1 3 1 1 2 3 NA 3 ...

``` r
states.data0$region <- factor(states.data0$region)
sat.region0 <- lm(energy~region,data = states.data0)
coef(summary(sat.region0))
```

    ##                 Estimate Std. Error    t value     Pr(>|t|)
    ## (Intercept)    405.61538   39.23196 10.3389019 1.395176e-13
    ## regionN. East -156.50427   61.33807 -2.5515032 1.411486e-02
    ## regionSouth    -25.49038   52.81764 -0.4826112 6.316606e-01
    ## regionMidwest  -61.61538   56.62646 -1.0881024 2.822182e-01

``` r
anova(sat.region0)
```

    ## Analysis of Variance Table
    ## 
    ## Response: energy
    ##           Df Sum Sq Mean Sq F value  Pr(>F)  
    ## region     3 145757   48586  2.4282 0.07737 .
    ## Residuals 46 920410   20009                  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The table shows us that there is not much of a significant difference among various regions and region is not a significant predictor.

#### Exercise 3: The purpose is to use glm to conduct a logistic regression to predict ever worked (everwrk) using age (agep) and marital status (rmaritl). It is also required to predict the probability of working for each level of marital status.

The data is initially loaded into NH11 using the readRDS function.We then study the data using str() and collapse all mising values to NA.Once this is done,we run our regression model using glm().

``` r
setwd("~/Git/Rstatistics")
NH11 <- readRDS("dataSets/NatHealth2011.rds")
str(NH11$everwrk)
```

    ##  Factor w/ 5 levels "1 Yes","2 No",..: NA NA 1 NA NA NA NA NA 1 1 ...

``` r
levels(NH11$everwrk)
```

    ## [1] "1 Yes"             "2 No"              "7 Refused"        
    ## [4] "8 Not ascertained" "9 Don't know"

``` r
NH11$everwrk <- factor(NH11$everwrk,levels= c("2 No","1 Yes"))
evr.out <- glm(everwrk~age_p+r_maritl, data = NH11 ,family = "binomial")
coef(summary(evr.out))
```

    ##                                                Estimate  Std. Error
    ## (Intercept)                                  0.44024757 0.093537691
    ## age_p                                        0.02981220 0.001645433
    ## r_maritl2 Married - spouse not in household -0.04967549 0.217309587
    ## r_maritl4 Widowed                           -0.68361771 0.084335382
    ## r_maritl5 Divorced                           0.73011485 0.111680788
    ## r_maritl6 Separated                          0.12809081 0.151366140
    ## r_maritl7 Never married                     -0.34361068 0.069222260
    ## r_maritl8 Living with partner                0.44358296 0.137769623
    ## r_maritl9 Unknown marital status            -0.39547953 0.492966577
    ##                                                z value     Pr(>|z|)
    ## (Intercept)                                  4.7066328 2.518419e-06
    ## age_p                                       18.1181481 2.291800e-73
    ## r_maritl2 Married - spouse not in household -0.2285932 8.191851e-01
    ## r_maritl4 Widowed                           -8.1059419 5.233844e-16
    ## r_maritl5 Divorced                           6.5375152 6.254929e-11
    ## r_maritl6 Separated                          0.8462316 3.974236e-01
    ## r_maritl7 Never married                     -4.9638756 6.910023e-07
    ## r_maritl8 Living with partner                3.2197443 1.283050e-03
    ## r_maritl9 Unknown marital status            -0.8022441 4.224118e-01

From the str() and levels() functions,it is observed that there are 5 levels in the category of people working- yes(1),no(2),Refused(7),Not ascertained(8) and Dont Know(9). When the regression is applied to the model, we are able to get a table that describes the relationship between working people and marital status and age. As we can observe from the table, the highest z value(18.11) is of age(age\_p), which implies that it has a stronger effect on working people. Its deviation from the estimate(standard error=0.001) is the least which proves our hypothesis.

To predict the probability of working for each level of marital status, we load the effects package using require().

``` r
require(effects)
```

    ## Loading required package: effects

``` r
data.frame(Effect("r_maritl", evr.out))                                     
```

    ##                              r_maritl       fit         se     lower
    ## 1     1 Married - spouse in household 0.8917800 0.04413754 0.8831439
    ## 2 2 Married - spouse not in household 0.8868918 0.21326041 0.8377247
    ## 3                           4 Widowed 0.8061891 0.06806325 0.7844913
    ## 4                          5 Divorced 0.9447561 0.10272953 0.9332564
    ## 5                         6 Separated 0.9035358 0.14579706 0.8755978
    ## 6                     7 Never married 0.8538900 0.05978759 0.8386559
    ## 7               8 Living with partner 0.9277504 0.13285112 0.9082334
    ## 8            9 Unknown marital status 0.8472992 0.49100994 0.6794427
    ##       upper
    ## 1 0.8998502
    ## 2 0.9225394
    ## 3 0.8261864
    ## 4 0.9543712
    ## 5 0.9257318
    ## 6 0.8679122
    ## 7 0.9433753
    ## 8 0.9355916

#### Exercise 4: The purpopse is to understand multilevel modelling by

1.  creating a null model predicting wellbeing ("WBEING")
2.  Calculating the ICC for null model
3.  Runing a second multi-level model that adds two individual-level predictors, average number of hours worked ("HRS") and leadership skills ("LEAD") to the model and interpret output.
4.  adding a random effect of average number of hours worked ("HRS") to the model and interpret output. Test the significance of this random term.

The required packages are loaded, that is, multilevel,matrix,lme4 and multilevel.

``` r
data(bh1996, package="multilevel")
require("Matrix")
```

    ## Loading required package: Matrix

``` r
require("lme4")
```

    ## Loading required package: lme4

``` r
require("multilevel")
```

    ## Loading required package: multilevel

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmList

    ## Loading required package: MASS

``` r
data(bh1996, package="multilevel")
```

A null model predicting wellbeing ("WBEING") is created.

``` r
WBEING <- lmer(WBEING ~ 1 + (1|G.HRS),  data=bh1996, REML = FALSE) 
summary(WBEING)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: WBEING ~ 1 + (1 | G.HRS)
    ##    Data: bh1996
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  19347.5  19368.2  -9670.8  19341.5     7379 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3227 -0.6471  0.0309  0.7188  2.6679 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  G.HRS    (Intercept) 0.03622  0.1903  
    ##  Residual             0.78960  0.8886  
    ## Number of obs: 7382, groups:  G.HRS, 95
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  2.77533    0.02266   122.5

By looking at the random effects table,the interclass correlation(ICC) is calculated.Hence, (0.03622)/(0.03622+0.78960) = 4.3% is variance at group level.

``` r
WBEING2 <- lmer(WBEING ~ HRS + LEAD + (1|G.HRS),  data=bh1996, REML = FALSE)  
 summary(WBEING2)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: WBEING ~ HRS + LEAD + (1 | G.HRS)
    ##    Data: bh1996
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  17847.5  17882.0  -8918.7  17837.5     7377 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.9181 -0.6585  0.0384  0.7044  3.6435 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  G.HRS    (Intercept) 0.01925  0.1387  
    ##  Residual             0.64657  0.8041  
    ## Number of obs: 7382, groups:  G.HRS, 95
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept)  1.687605   0.067694   24.93
    ## HRS         -0.031697   0.004379   -7.24
    ## LEAD         0.500198   0.012790   39.11
    ## 
    ## Correlation of Fixed Effects:
    ##      (Intr) HRS   
    ## HRS  -0.799       
    ## LEAD -0.635  0.120

``` r
   anova(WBEING,WBEING2)
```

    ## Data: bh1996
    ## Models:
    ## WBEING: WBEING ~ 1 + (1 | G.HRS)
    ## WBEING2: WBEING ~ HRS + LEAD + (1 | G.HRS)
    ##         Df   AIC   BIC  logLik deviance Chisq Chi Df Pr(>Chisq)    
    ## WBEING   3 19348 19368 -9670.8    19342                            
    ## WBEING2  5 17848 17882 -8918.7    17838  1504      2  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
  WBEING3 <- lmer(WBEING ~ HRS + (1|G.HRS),  data=bh1996, REML = FALSE)
  summary(WBEING3)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: WBEING ~ HRS + (1 | G.HRS)
    ##    Data: bh1996
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  19235.3  19262.9  -9613.6  19227.3     7378 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3767 -0.6497  0.0376  0.7205  2.6761 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  G.HRS    (Intercept) 0.02476  0.1574  
    ##  Residual             0.78009  0.8832  
    ## Number of obs: 7382, groups:  G.HRS, 95
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept)  3.365135   0.057655   58.37
    ## HRS         -0.052075   0.004779  -10.90
    ## 
    ## Correlation of Fixed Effects:
    ##     (Intr)
    ## HRS -0.940

``` r
    anova(WBEING,WBEING2,WBEING3)
```

    ## Data: bh1996
    ## Models:
    ## WBEING: WBEING ~ 1 + (1 | G.HRS)
    ## WBEING3: WBEING ~ HRS + (1 | G.HRS)
    ## WBEING2: WBEING ~ HRS + LEAD + (1 | G.HRS)
    ##         Df   AIC   BIC  logLik deviance   Chisq Chi Df Pr(>Chisq)    
    ## WBEING   3 19348 19368 -9670.8    19342                              
    ## WBEING3  4 19235 19263 -9613.6    19227  114.22      1  < 2.2e-16 ***
    ## WBEING2  5 17848 17882 -8918.7    17838 1389.83      1  < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

A new model,WBEING2, is created by adding two individual-level predictors,in this case, work hours(HRS) and leadership climate(LEAD). The two models that have been created are compared using anova(). It is seen that model 2 is more significant than model 1.We then create a 3rd model,WBEING3,to see the random effect of HRS and all the 3 models are compared using anova(). It is obsrerved that WBEING2 and WBEING 3 are equally significant models (\*\*\*).
