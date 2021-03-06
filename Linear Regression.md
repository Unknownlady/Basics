---
Title: "Linear  Regression"
---

#### Exercise 1: The objective of this exercise is to fit a model predicting energy consumed per capita (energy) from the percentage of residents living in metropolitan areas (metro).The dataset used includes data about various states in the USA comprising of 51 states with 21 characteristics (region,population,density,etc).



We begin by inspecting our dataset.
```{r Load data}
 setwd("~/Git/Rstatistics")
 states.data0 <- readRDS("datasets/states.rds")
  states.info0 <- data.frame(attributes(states.data0)[c("names", "var.labels")])
 head(states.info0,5) 
```


As we can see,the first 5 characteristics are state,geographical region,population in the year 1990(pop),area of the land(in square miles) and people per square mile(density).

Since we want to fit a model that describes the relationship between the variables energy and metro, we extract this data from the dataset using the subset function. 
```{r Extract information}
  sts.ex.sat0 <- subset(states.data0,select = c("energy","metro"))
 summary(sts.ex.sat0)
 ```

 The summary function is  used above to get different measures which descibe the type of distributions that the two datasets(energy and metro) have. The table shows that the  minimum value of energy consumed per capita is 200 (city:New York) whereas the minimum percent of residents living in metropolitan cities is 20% (city:Idaho). Similarly,the maximum values for energy and metro are 991.0 and 100%. 


We then use the correlation function to establish a relationship between energy and metro variables.
```{r Correlation}
cor(sts.ex.sat0)
cor(sts.ex.sat0,use = "complete")
```

As you can see, the first command gives a table with NA values.This does not give us a clear picture of the correlation between two variables.This happens because all the missing values in the datasets in taken into consideration implicitly. To prevent usage of NA(missing) values, we put use="complete" in the correlation function.

From the table we observe that the correlation between energy and metro is -0.3397445.This implies that there is a negative relationship between energy and metro. As the percentage of residents living in metropolitan cities(metro) increase,the minimum value of energy consumed per capita(energy) decreases.

This can be further explained by plotting the data.
```{r Plot data,echo=FALSE}
plot(sts.ex.sat0)
```

The plot,however does not explain this relationship clearly. For a better understanding of our dataset, we fit our regression model and study the different values to establish a relationship.
```{r Regression model}
sat.mod0 <- lm(energy~metro, data = states.data0)
summary(sat.mod0) 
plot(sat.mod0)
```

The plots show residuals in four different plots.

#### 1. Residuals vs Fitted
This plot shows if residuals have non-linear patterns.Since the residuals are equally spread  around the horizontal line without distinct patterns,the relationship between metro and energy is non-linear. There are a few outliers (19,2 and 51) which if removed can give a better model that fits our data more accurately.


#### 2.Normal Q-Q 
This plot shows if the datasets are normally distributed.For example, if we run a statistical analysis that assumes our dependent variable(energy) is Normally distributed, we can use a Normal Q-Q plot to check that assumption. Do residuals follow a straight line well or do they deviate severely? In our plot, most of the residuals follow a straight line but the outliers(51,19,2) are not following the line. These points could give be a problem in giving us the correct analysis.


#### 3.Scale Location
This plot shows if residuals are spread equally along the ranges of predictors. This can be used to check the assumption of equal variance (homoscedasticity).The residuals appear to be randomly spread and outliers(19,2,51) are not included in the model.


####4. Residuals vs Leverage
This plot helps us to find influential cases. Not all outliers are influential in linear regression analysis. Even though data has outliers, they may not be influential to determine a regression line. This implies that the results wouldn't be much different if we either include or exclude them from analysis. On the other hand, some cases could be very influential even if they look to be within a reasonable range of the values. They could be extreme cases against a regression line and can alter the results if we exclude them from analysis. 
In the plot shown above, all the data points are well inside the Cook's distance lines(red dotted lines). Hence, there are no influential cases.


We now select additional predictors to add to the model and establish a relationship between different predictors(as done above).
```{r Other models}
 summary(lm(energy~metro+density+area+region, data = states.data0))
summary(lm(energy~metro+area+pop, data = states.data0))
```


A good model is decided by the values of adjusted R squared and multiple R squared. We need to ensure that overfitting is avoided and thus many variables are not added to the model. On trying various predictors like waste,density,region,etc.,it was observed that when area,density,region was added, multiple R squared value is 0.5473 and adjusted R squared is 0.4841.When expense was further added tp this mode, it was found that both multiple R squared and adjusted R squared increases and significance of area and metro remains the same.However, area,density,region are closely interrelated and this could lead to overfitting. To avoid this,we consider area,metro, density and region which gives us the same significance level with a slight change in values of multiple R squared(0.5473) and adjusted R squared(0.4841). This implies that percent of people living(metro), area,region and density are good predictors of energy per capita(energy).






#### Exercise 2: The aim of this ecercise is to generate an interaction term and test the interaction.Region is added to the model and difference in different region is considered.


Initially, we study the interaction between metro and density.                                         
We add the interaction term to the model
```{r Generating Interaction}                
energy.metro.interaction <- lm(energy~metro*density,data = states.data0)                    
coef(summary(energy.metro.interaction))
```

The estimate in this case is 0.01386147. As it is very close to 0, this variable must be removed from the model as it is not helping in predicting the dependent variable.


```{r Adding region}
str(states.data0$region)
states.data0$region <- factor(states.data0$region)
sat.region0 <- lm(energy~region,data = states.data0)
coef(summary(sat.region0))
anova(sat.region0)
```

The table shows us that there is not much of a significant difference among various regions and region is not a significant predictor.






#### Exercise 3: The purpose is to use glm to conduct a logistic regression to predict ever worked (everwrk) using age (agep) and marital status (rmaritl). It is also required to predict the probability of working for each level of marital status.


The data is initially loaded into NH11 using the readRDS function.We then study the data using str() and collapse all mising values to NA.Once this is done,we run our regression model using glm().


```{r load new data}
setwd("~/Git/Rstatistics")
NH11 <- readRDS("dataSets/NatHealth2011.rds")
str(NH11$everwrk)
levels(NH11$everwrk)
NH11$everwrk <- factor(NH11$everwrk,levels= c("2 No","1 Yes"))
evr.out <- glm(everwrk~age_p+r_maritl, data = NH11 ,family = "binomial")
coef(summary(evr.out))
```

From the str() and levels() functions,it is observed that there are 5 levels in the category of people working- yes(1),no(2),Refused(7),Not ascertained(8)  and Dont Know(9). When the regression is applied to the model, we are able to get a table that describes the relationship between working people and  marital status and age. As we can observe from the table, the highest z value(18.11) is of age(age_p), which implies that it has a stronger effect on working people. Its deviation from the estimate(standard error=0.001) is the least which proves our hypothesis. 

To predict the probability of working for each level of marital status, we load the effects package using require().
```{r load packages}
require(effects)
data.frame(Effect("r_maritl", evr.out))                                     
```





#### Exercise 4: The purpopse is to understand multilevel modelling by
a. creating a null model predicting wellbeing ("WBEING")
b. Calculating the ICC for null model
c. Runing a second multi-level model that adds two individual-level predictors, average number of       hours worked ("HRS") and leadership skills ("LEAD") to the model and interpret output.
d. adding a random effect of average number of hours worked ("HRS") to the model and interpret          output. Test the significance of this random term.

The required packages are loaded, that is, multilevel,matrix,lme4 and multilevel.
```{r load data}
data(bh1996, package="multilevel")
require("Matrix")
require("lme4")
require("multilevel")
data(bh1996, package="multilevel")
```



A null model predicting wellbeing ("WBEING") is created.
```{r null model}
WBEING <- lmer(WBEING ~ 1 + (1|G.HRS),  data=bh1996, REML = FALSE) 
summary(WBEING)
```

By looking at the random effects table,the interclass correlation(ICC) is calculated.Hence, (0.03622)/(0.03622+0.78960) = 4.3% is variance at group level.


```{r adding predictors}
WBEING2 <- lmer(WBEING ~ HRS + LEAD + (1|G.HRS),  data=bh1996, REML = FALSE)  
 summary(WBEING2)
   anova(WBEING,WBEING2)
  WBEING3 <- lmer(WBEING ~ HRS + (1|G.HRS),  data=bh1996, REML = FALSE)
  summary(WBEING3)
    anova(WBEING,WBEING2,WBEING3)
```
A new model,WBEING2, is created by adding two individual-level predictors,in this case, work hours(HRS) and leadership climate(LEAD). The two models that have been created are compared using anova(). It is seen that model 2 is more significant than model 1.We then create a 3rd model,WBEING3,to see the random effect of HRS and all the 3 models are compared using anova().
It is obsrerved that WBEING2 and WBEING 3 are equally significant models (***).