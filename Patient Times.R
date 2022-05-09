---
title: "Patient Times"
author: ""
output:
  word_document:
    toc: yes
  html_document:
    number_sections: yes
    toc: yes
    fig_width: 8
    fig_height: 5
    theme: cosmo
    highlight: tango
    code_folding: hide
  pdf_document:
    toc: yes
---
# Step:1

  We have a data set that doesn't give too much information ;- we have the average_time which is the average time       spent by the patients in the department this is measured in minutes. We have the patients_2hrs this is the            percentage of patients for the day whom spend less than 2 hrs in the department.
  
  We created extra two columns ; - average_time_selection which is the average time less than 120minutes is given 1     and average time more than 120 minutes is represented by 0. We also have patients_2hrs_selection which is the         percentage number of patients less than 50 who spend 2hrs in th department is given 1 and 0 for otherwise. 

  Read and inspect the data set. Provide a descriptive analysis for each of the variables in the data set. 

- **Anwser**

```{r Step:1}
# Load packages
library("plotly")
library("tidyverse")
library("data.table")
library("dplyr")
library("gridExtra")
library("knitr")
library("scales")

# Load Cloud Data
df.al <- readxl::read_xlsx("DATA Times in Hospital (1).xlsx")
df.al <- df.al[,-1]
df.al$patients_2hrs_selection <- as.factor(df.al$patients_2hrs_selection)
df.al$average_time_selection <- as.factor(df.al$average_time_selection)
# diplay data
head(df.al,26)
# reveal cloud.csv data analysis
str(df.al)
# descriptive analysis of cloud.csv
print("Summary of average time spent by each patients in a day at the department in minutes")
summary(df.al$average_time)

print("Summary of percentage number of Patients whom spend less than 2 hours in a day")
summary(df.al$patients_2hrs)
```

  The Maximum average time spent by patients in the hospital Outpatients deptartment is 146 minutes , the               minimum is 103 minutes , while the median and mode are 130 minutes and 129 minutes respectively. This indicates       that most patients spend more than two hours on an average day in the hopital department.
    
  Again the median and mode of patients that spend less than 2 hours in day was as high as 62% for some days and as     low as 38% for some days the mode and median indicates that most patients or half of the patients visiting the        hospital department spend most spend more than 2 hours at the hospital facility.

# Step:2

  We need to fit a model that will keep the time patients spend in hospital outpatients department to less than         2hours.
  
  We use the Linear Model functions to test both variables against easch other , since we have very little              information and no outline dependent variable we can troubleshoot to find a model that fits the information given     best.
  
  

```{r Tasks:2}
print("Model fit of percentage number of Patients whom spend less than 2 hours in a day")
fit.1 <- lm(patients_2hrs_selection == "1" ~ average_time  , data = df.al )
summary(fit.1)

print("Model fit for Average time spent by Patients in a day at the Outpaitents Hospital Department")
fit.2 <- lm(average_time_selection == "1" ~ patients_2hrs , data = df.al)
summary(fit.2)
```
   fit.1 model has an Adjusted R-Squared of 58.5%, suggesting that the model can explain the data well enough. We        might also interested in seeing how good the model will be on the testing dataset.
    
   fit.2 model has an Adjusted R-Squared of 40.5%, suggesting that the model can't explain the data well enough as       fit.1.
  
# Step:3
 plot models
  
```{r Step:3}
print("Plots of Model fit for Average time spent by Patients in a day at the Outpaitents Hospital Department")
plot(x = df.al$average_time , y = df.al$average_time_selection)
plot(fit.2,pch=20)

print("Plots of Model fit for percentage number of Patients whom spend less than 2 hours in a day")
plot(x = df.al$patients_2hrs , y = df.al$patients_2hrs_selection)
plot(fit.1, pch=20)


```


# Step:4

  Perform white test using fit.2

  
```{r Step:4}
#load lmtest library
library(lmtest)

#perform White's test
bptest(fit.2, ~ patients_2hrs + I(patients_2hrs^2), data = df.al)
```

  Here is how to interpret the output:
  The test statistic is X2 = 16.145.
  The degrees of freedom is 2.
  The corresponding p-value is 0.000312.
  
  Null (H0): Homoscedasticity is present.
  Alternative (HA): Heteroscedasticity is present.
  
  Since the p-value is less than 0.05, we reject the null hypothesis. We have sufficient evidence to accept the         Alternative that there is Heteroscedasticity present. 
  When this assumption is violated, we say that heteroscedasticity is present in the residuals. When this occurs, the   results of the regression become unreliable

# Step:5

  Perform white test using fit.1

  
```{r Step:5}
#load lmtest library
library(lmtest)

#perform White's test
bptest(fit.1, ~ average_time + I(average_time^2), data = df.al)
```

  Here is how to interpret the output:
  The test statistic is X2 = 1.1694.
  The degrees of freedom is 2.
  The corresponding p-value is 0.5573.
  
  Null (H0): Homoscedasticity is present.
  Alternative (HA): Heteroscedasticity is present.
  
  Since the p-value is not less than 0.05, we fail to reject the null hypothesis. We do not have sufficient evidence    to say that heteroscedasticity is present in the regression model. 



# Step:6

  fit the model to new data to predict percentage of patients that will spend less than 2 hours at the department.

```{r Step:6}
df.al2 <- read_csv("DATA Times in HospitalV1.csv")
df.al2 <-data.frame(df.al2)
df.al2$average_time_selection <- as.factor(df.al2$average_time_selection)
predicted_p <- predict(fit.1, df.al2)

head(predicted_p)
table(predicted_p)

# compute confussion matrix of model2 
tab = table(predicted_p>0.5,df.al2$average_time_selection)
tab
#compute accuracy of model2
accurracy <- sum(diag(tab))/sum(tab)*100
accurracy


```








