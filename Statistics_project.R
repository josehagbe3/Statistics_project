---
title: "Statistic_Project 2023"
author:
      - José Thiéry M. HAGBE 
      - Alassane SOMA 
date: "Date: January 31, 2023"
output:
  pdf_document: default
  word_document: default
  html_document:
    df_print: paged
editor_options: 
  markdown: 
    
    
    wrap: 0
---
# I - Motivation

This project is important because it allows us to put the theoretical
knowledge acquired in our statistics courses into practice. It gives us
the opportunity to better understand the application of concepts on real
data and to better understand the challenges related to data analysis.
It also helps us to develop our skills in data manipulation and
visualization, as well as to improve our ability to communicate the
results of our analyses. In short, this project contributes to our
training as statisticians and prepares us to better tackle the
challenges that arise in our professional field.

# II - Quality

There are several reasons why one might choose to use the "The Second National Health and Nutrition Examination Survey" database for a statistics project.
Firstly, we have the data quality: the "The Second National Health and Nutrition Examination Survey" database is considered to be of high quality, as it was collected by the National Center for Health Statistics (NCHS), which is a federal agency whose mission is to produce high-quality information on the health of the American population. 
Secondly,we have the data richness: the database includes detailed information on health, nutrition, lifestyle, and demographic characteristics of the participants, allowing to answer a wide range of research questions. Thirdly, we also have the representativeness of the data: the "The Second National Health and Nutrition Examination Survey" database is sampled in a way that is representative of the American population, allowing to obtain results that can be generalized to the American population. Fourthly, the data is available: the database is widely used and is easily accessible to researchers and statisticians, which makes it easy to set up our statistics project.


# III- Data Source

The "The Second National Health and Nutrition Examination Survey"
(NHANES II) database was collected by the National Center for Health
Statistics (NCHS), which is a federal agency that is part of the Centers
for Disease Control and Prevention (CDCP). It can be accessed by using
"The Second National Health and Nutrition Examination Survey" database
via the NCHS website:
<https://www.cdc.gov/nchs/nhanes/nhanes2/index.htm>. From all those
sectors have been selected some variables hat we used to conduct our
study. Variables in our study are labeled as followed: race, sex, weight, diabetes,
height, region and houssiz.

### Loading RStudio libraries.
```{r load packages, echo = F, message = F}
library(foreign)
library(haven)
library(readstata13)
library(prettyR)
library(ineq)
library(lattice)
library(dplyr)
library(ggplot2)
library(MASS)
library(readxl)
library(readr)
library(vcd)
library(questionr)
library(DataExplorer)
library(stats)
```


## 1 - Importation of data

```{r Importation of our database in computer}
dta <- read_excel("C:/Users/Mr SOMA/Documents/Divers/AA/ASE 2022-2023/Statistics/Midterm/Project_Group_Statistics_2023/dta.xls")
```


# IV - Descriptive Statistics

## 1- Parameters calculation

Analysis of the distribution of continuous variables: calculate
statistics such as mean, median, standard deviation, minimum, quantile,
maximum for variables such as weight, height and age.

### a - for weight

```{r parameters calculation_1, message = F}
length(dta$weight)
# Calculation of the average.
mean(dta$weight)

# Calculation of the median.
median(dta$weight)

# Calculation of standard deviation
sd(dta$weight)

# Calculation of minimum
min(dta$weight)

# Calculation of maximum
max(dta$weight)

# Quantiles calculation.
quantile(dta$weight, c(0.25, 0.5, 0.75))
```
The above commands allowed us to obtain descriptive results on the weight variable. Indeed, with a number of 10337 individuals whose minimum weight is 30 kg and a maximum of 176 kg. the average weight was about 72 kg with a median weight of about 70 kg which means that more than half of the patients had a weight greater than 70 kg. Here our median is close to the mean, this may mean that the weight distribution is symmetrical around the central tendency.

### b - for height

```{r parameters calculation_2, message = F}
# Calculation of the average.
mean(dta$height)

# Calculation of the median.
median(dta$height)

# Calculation of standard deviation
sd(dta$height)

# Calculation of minimum
min(dta$height)

# Calculation of maximum
max(dta$height)

# Quantiles calculation.
quantile(dta$height, c(0.25, 0.5, 0.75))
```

These commands above allowed us to obtain descriptive results on the height variable. Indeed, with a number of 10337 individuals whose minimum height is 135.5 cm and a maximum of 200 cm. the average height was about 167.7 cm with a median height of about 167 cm which means that half of the patients had a height less than or equal to 167 cm. Here our median is close to the mean, also translate the symmetrical distribution around the central tendency.

### c - for Age
```{r parameters calculation_3, message = F}
# Calculation of the age average.
mean(dta$age)

# Calculation of the age median.
median(dta$age)

# Calculation of age standard deviation
sd(dta$age)

# Calculation of age minimum
min(dta$age)

# Calculation of age maximum
max(dta$age)

# Quantiles calculation.
quantile(dta$age, c(0.25, 0.5, 0.75))

#interquartile range (IQR) for age
IQR(dta$age)
# or use 
q75 <- quantile(dta$age, 0.75)
q25 <- quantile(dta$age, 0.25)
IQR <- q75 - q25
IQR
```

The above commands allowed us to obtain descriptive results on the age variable. Indeed, with a population of 10337 individuals, a minimum age of 20 years and a maximum age of about 74 years, we can see that the ages of the study population cover a wide range. The mean age of 47 years and the median of 49 years suggest that the central tendency of age in the study population is around 49 years. However, the high variance of 296.42 shows that the ages in the study population are not highly concentrated around this central tendency and that the age distribution is quite scattered. Half of the patients have an age less than or equal to 49 years, which is reflected in the median. By using the mean age and median together, we can better understand the age distribution in the study population and thus better assess the potential association between age and diabetes.

```{r parameters calculation_0, message = F}
# we can also use summary to output all these results
summary(dta$weight)
summary(dta$height)
summary(dta$age)
```

The above command for the "summary()" function also gives us the same results on each variable as found above.

## 2 - Categorical variable distribution analysis:

calculating frequencies and percentages for variables such as sex, race and diabetes.

```{r parameters calculation_4, message = F}
# Frequencies and percentages for the "sex" variable
sex_table <- table(dta$sex)
sex_prop <- prop.table(sex_table)
cbind(sex_table,sex_prop)

# Frequencies and percentages for the "race" variable
race_table <- table(dta$race)
race_prop <- prop.table(race_table)
cbind(race_table,race_prop)

# Frequencies and percentages for the "diabetes" variable
diabetes_table <- table(dta$diabetes)
diabetes_prop <- prop.table(diabetes_table)
cbind(diabetes_table,diabetes_prop)
```

We obtain from these orders the numbers and frequencies of the variables sex, race and diabetes. In fact, 52% (5428/10337) of the individuals included in the study are female. The blond race is the most represented with a number of 9051, that is to say about 87% of the total study population. Finally, about 5% of the total population is affected by diabetes.

## 3 - Graphical representation

To create graphical representations of the categorical variables "sex",
"race" and "diabetes" in the "The Second National Health and Nutrition
Examination Survey" dataset, we can use various R plotting functions
such as barplot(), pie()

### a - Sex
```{r}
# Bar chart for the "sex" variable
barplot(table(dta$sex), main = "Sex Distribution", xlab = "Sex", ylab = "Frequency",
        col=c("pink","gray"))

# Pie chart for the "sex" variable
# pie(table(dta$sex), main = "Sex Distribution", labels = c("Female", "Male"), col=c("pink","gray"))
```
As said above, these graphical representations are representations according to the sex of the individuals in the sample, the female gender is the most represented in our study.

### b - Race
```{r}
# # Bar chart for the "race" variable
# barplot(table(dta$race), main = "Race Distribution", xlab = "Race", ylab = "Frequency", col=c("pink","gray","purple"))

# Pie chart for the "race" variable
pie(table(dta$race), main = "Race Distribution", labels = c("Black","Other","White"), col=c("pink","gray","purple"))
```
These graphs are the graphical representations of the race, in fact the white race as said above is the most represented over the black race.

### c - Diabetes
```{r}
# Bar chart for the "diabetes" variable
# barplot(table(dta$diabetes), main = "Diabetes Distribution", xlab = "Diabetes",
#         ylab = "Frequency", col=c("pink","red"))

# Pie chart for the "diabetes" variable
pie(table(dta$diabetes), main = "Diabetes Distribution", 
    labels = c("Non-Diabetic", "Diabetic"), col=c("pink","red"))
```
These representations are the distribution of individuals according to health status in diabete.


#### d - To create graphs for continuous variables such as weight and height in R.
```{r}

```




Histograms: We can use the hist() function to create a histogram.
we can use the following code to create a histogram of weight.


```{r}
#fig.width: 10
# Histograms: You can use the hist() function to create a histogram.
# we can use the following code to create a histogram of weight:
 hist(dta$weight, main = "Weight Distribution", xlab = "Weight", 
      ylab = "Frequency", col = "blue",density =NULL, angle = 100)

```


```{r}
# Density Plots: You can use the density() function to create a density plot. 
# we can use the following code to create a density plot of Weight:

plot(density(dta$weight), main = "Weight Density", xlab = "Weight",
     col = "blue")

# Box Plots: You can use the boxplot() function to create a box plot.
# we can use the following code to create a box plot of weight:

boxplot(dta$weight, main = "Weight Box Plot", xlab = "Weight")

```


```{r}
# Histograms: You can use the hist() function to create a histogram. 
# we can use the following code to create a histogram of height:

hist(dta$height, main = "height Distribution", xlab = "height",
     ylab = "Frequency", col = "gold")

# Density Plots: You can use the density() function to create a density plot. 
# we can use the following code to create a density plot of height:

plot(density(dta$height), main = "Height Density", xlab = "Height", col = "red")


# Box Plots: You can use the boxplot() function to create a box plot.
# we can use the following code to create a box plot of weight:

boxplot(dta$height, main = "height Box Plot", xlab = "height")
```


```{r}
# Histograms: You can use the hist() function to create a histogram. 
# we can use the following code to create a histogram of age:

hist(dta$age, main = "Age Distribution", xlab = "Age", 
     ylab = "Frequency", col = "pink")

# Density Plots: You can use the density() function to create a density plot. 
# we can use the following code to create a density plot of age:

plot(density(dta$age), main = "Age Density", xlab = "Age", col = "blue")


# Scatter Plots: You can use the plot() function to create a scatter plot. 
# we can use the following code to create a scatter plot of weight and height:

plot(dta$weight ~ dta$height, xlab = "height", ylab = "Weight",
     main = "Weight vs Height Scatter Plot")


# Line Plots: You can use the lines() function to create a line plot.
# we can use the following code to create a line plot of weight :
  
  lines(dta$weight,col="blue",main = "Weight Line Plot", xlab = "Observations",
        ylab = "Weight")
  
  # Line Plots: You can use the lines() function to create a line plot.
  # we can use the following code to create a line plot of height :
  
  lines(dta$height,col="gold",main = "height Line Plot", xlab = "Observations",
        ylab = "height")
```

Descriptive analysis of individual height and weight shows that these variables appear to follow a normal distribution. The boxplots, which graphically represent the distribution of the data, indicate that most of the height and weight measurements are concentrated around the mean height and mean weight, respectively. In addition, the fact that the boxplot are centered indicates that the data are in good correspondence with a normal distribution. The distribution density representations also reinforce this observation, showing that the distribution of height and weight strongly resembles that of a normal distribution. As a result, we can conclude that the heights and weights of individuals are generally distributed similarly to a normal distribution.

## 4 - Association of two variables

### 1 - Between Categorical

```{r Age group and diabeties}
# In this case we have as modalities of diabetes are: 0= No diabetes and 1 = yes.
# To have more meaningful results, we will reduce our "diabetes" variable to a variable named "diabetic" where 0 corresponds to "non-diabetic" and 1 corresponds to "diabetic" so "Yes".
dta<-dta %>%
  dplyr::mutate(
    "diabetic"=ifelse(diabetes=='0',"Non-diabetic",
                      "Diabetic"))


barchart(table(dta$agegrp, dta$diabetic), stack = F, horizontal = F, 
         auto.key = T,
         xlab = "Age group", 
         main = "people's diabetic analysis according to the age group")
table(dta$agegrp, dta$diabetic)
prop.table(table(dta$agegrp, dta$diabetic))
chisq.test(table(dta$agegrp, dta$diabetic))
assocstats(xtabs(~dta$agegrp+dta$diabetic))
```
The graph above is the graphical representation of the individuals included in the study according to age group. here we have kept the age groups as obtained in the database, the groups from 60 to 69 years old and from 20 to 29 years old were the most represented. the commands above have allowed us to make this representation. in fact, these commands have also allowed us to make a representation of cases of diabetes by age group. visibly the age groups from 60 to 70 years old and more are the most affected.

Regarding the p-values of the pearson Chi-squared test (p-value \< 5%),
we can say that the age groupe is not independent from diabetes. The
associated Cramer's V reveals that this association is weak (Cramer's V
= 0.172)

```{r Race and diabetes}
barchart(table(dta$race, dta$diabetic), stack = F, horizontal = F, auto.key = T,
         xlab = "Type of race", 
         main = "people's diabetic analysis according to the race")
table(dta$race, dta$diabetic)
prop.table(table(dta$race, dta$diabetic))
chisq.test(table(dta$race, dta$diabetic))
assocstats(xtabs(~dta$race+dta$diabetic))
```
The graph above is a graphic representation of the individuals included in the study according to race group. The above commands allowed us to make this representation. Indeed, these commands also allowed us to make a representation of cases of diabetes by race group. obviously the white race group is the most affected.
We still have a weak association (p-value \< 10% and Cramer's V = 0.049) between the type of race and the fact that who has the diabetes.

```{r remainings assoc qual}
barchart(table(dta$sex, dta$diabetic), stack = F, horizontal = F, auto.key = T,
         xlab = "Sex", 
         main = "People's diabetic analysis according to the sex")
table(dta$sex, dta$diabetic)
prop.table(table(dta$sex, dta$diabetic))
chisq.test(table(dta$sex, dta$diabetic))
assocstats(xtabs(~dta$sex+dta$diabetic))

barchart(table(dta$rural, dta$diabetic), stack = F, horizontal = F, auto.key = T,
         xlab = "Habitation area", 
         main = "People's diabetic analysis according to the location")
table(dta$rural, dta$diabetic)
prop.table(table(dta$rural, dta$diabetic))
chisq.test(table(dta$rural, dta$diabetic))
assocstats(xtabs(~dta$rural+dta$diabetic))
```

What is interesting in those last analyses is that there is a quite
moderate association between (p-value \< 10% and Cramer's V =0.002 ) the
habitation area and the variable diabetes.

### 2 - Between continuous and categorical

```{r anovas}
anova_1 = aov(dta$weight~dta$diabetes, data = dta)
summary(anova_1)
```

While there is a 95% significant relationship between weight and the people who has the diabetes (p-value \< 5%).

## 5 - Probability Calculus

### a - probability mass
```{r probability mass}
truehist(dta$weight, main='Probability mass function of weight',
                 ylab='Probabilities', xlab = 'weigt')
lines(density(dta$weight),col="red") 
```
### b - Probability density function
For the probability density function, we'll categorize the variable weight which we are dealing with.
```{r Probability density function}
summary(dta$weight)


Cweight <- cut(dta$weight, c(-Inf,60,70,80,180), labels =
                c('less 60', '61-70', '71-80', 'over 80'))
# Cweight <- as.numeric(Cweight)  
summary(Cweight)  

plot(Cweight, main='Histogram of categorized weight', xlab='Number of individual')

```

### c - permutation

We consider that over the course of a year, a given person may contact
Consider a sample of 1% of the people who have diabetes. How many draw
patterns can we get in that month?

```{r permutations}
diab = subset(dta, dta$diabetes == '1')
factorial(0.01*nrow(diab))
```

Let's consider men from an urban area who has the diabetes and want to
create a committee of 3 peoples, How many possible committees can we have?

### d - combinatorics
```{r combinatorics}
n = length(diab$sex[(dta$rural == "0") & (dta$sex == "Male")])
choose(n, 3)
```

## 6 Conditional probabilities

We first create the contingency tables

```{r conditionals 1}
#filter individuals living in rural areas
dta_rurale <- dta[dta$rural == 1, ]
# calcul of conjunctive probability
joint_prob_1 <- addmargins(prop.table(table(dta_rurale$diabetes)))
joint_prob_1
#calculate the conditional probability of living in a rural area
prop_rurale <- sum(dta_rurale$rural) / nrow(dta)
prop_rurale
```

For the following command, we want to calculate the probability that an individual chosen in the female gender is diabetic

```{r conditionals 2}
prob_fem_diab <- dta %>%
  filter(sex == "Female", diabetic == "Yes") %>%
  nrow() / dta %>%
  filter(diabetic == "Yes") %>%
  nrow()

prob_fem_diab
```

## 7 - Tshebytshev and Markov inequalities

### a - Tshebytshev inequality

Considering the weight variable, let us verify the Tshebytshev
inequality.\
We successively:\
1 - create the vector that will contains the values \|X - mean(X)\|\
2 - compute the probability Pr(\|X - mean(X)\| \< c)\
3 - Now, we compute 1 - Var(X)/c²\
4 - And we confirm that Pr(\|X - mean(X)\| \< c) \>= 1 - Var(X)/c²

```{r Tsheb}
# 1
abs_one = abs(dta$weight - mean(dta$weight))

# 2
c = numeric(length(abs_one)); cc = 72
for (i in 1:length(abs_one))
  c[i] = cc
prob = length(abs_one[abs_one < c])/length(abs_one)

# 3
comp = 1 - (var(dta$weight)/cc^2)

# 4
maxi = max(prob, comp)

sprintf("The highest between the probability Pr(|X - mean(X)| < c) = %.2f 
        and the calculation 1 - Var(X)/c² =  %.3f is %.2f Tshebytshev inequality is satisfied",
        prob, comp, maxi)
```

### b - Markov inequality

We successively:

```{r markov}
absv = abs(dta$houssiz) # absolute value of the number of people
c = numeric(length(absv)); a = 5
for (i in 1:length(absv))
  c[i] = a
prob = length(absv[absv >= c])/length(absv) # probability to have more than 5 persons per house
E = mean(dta$houssiz)/a # the mean
Markov = min(prob, E)
Markov
```

## 8 - Probability Mass Function

### a - Goemetric

Let's denote by Y the random variable that is the number of individuals
we must take before we find the first given individual who has attained
diabetes. Y \~ Geometric(p), where p is the proportion of individuals
who have diabetes. We have:

```{r geom}
p1 = length(dta$diabetes[dta$diabetes == "1"])/length(dta$diabetes)
sprintf("p equals %.2f", p1)
```

Let's plot and see

```{r geom plot}
k <- 0:n
pmf <- dgeom(k,p1)
plot(k,pmf)
```

The probability of finding the first given individual who reached
diabetes after 10 trials.

```{r goem prob}
dgeom(10, p1)
```

### b - Binomial

Consider the experiment in which we test whether or not a given
individual has diabetes. Let us also denote by X the random variable
associated with such an experiment. X \~ Binomial(n, p), with n the
total number of individuals in the database and p the proportion of
individuals who have diabetes. We have :

```{r binom}
p2 = length(dta$diabetes[dta$diabetes == "1"])/length(dta$diabetes); n = length(dta$diabetes)
sprintf("p equals %.2f,and n equals %d", p2, n)
```

Let's plot and see

```{r binom plot}
k <- 0:n
pmf <- dbinom(k,n,p2)
plot(k,pmf)
```

Let's compute the probability that 1000 people from our database have
the diabete

```{r binom prob}
dbinom(1000, size = n, prob = p1)
```

## 9 - Probability Density Function

We will approximate a variable that follows a binomial distribution by a
normal distribution. Let's now consider the variable X1 that takes 1
whether the given individual in the database have a diabetes and 0 if
not. We have X1 \~ Binomial(n, p) such that:

```{r binomapprox}
p3 = length(dta$diabetes[dta$diabetes == "1"])/length(dta$diabetes); p3
sprintf("p equals %.2f", p3)
```

The distribution of X1 can be approximated by a normal distribution such
that X1 \~ N(np, (np(1-p)\^1/2)), since n \>= 30, np \>= 5 and np(1-p)
\>= 5

Let's compute the probability that 50 individuals in the database have a
diabetes using the binomial distribution and the approximated normal
distribution.

```{r approx and see}
dbinom(100, n, p3)
dnorm(500, mean = n * p3, sd = sqrt(n * p3 * (1 - p3)))
```

Now we have good approximation.

# V - inference

Let choose 3 samples and fund the mean and variance of all of them
before to estimate

```{r}
Sample1 = sample(dta$zinc, 200)
Sample2 = sample(dta$weight, 200)
Sample3 = sample(dta$houssiz,200)
```

## 1 - Point Estimation"

For our data we will estimate those three parameters for each sample.
Sample 1 : zinc

```{r}
mean(Sample1, na.rm = TRUE)
```

```{r}
var(Sample1, na.rm = TRUE)
```

Sample 2 : weight

```{r}
mean(Sample2)
```

```{r}
var(Sample2)
```

Sample 3 : house size

```{r}
mean(Sample3)
```

```{r}
var(Sample3)
```

## 2 - Confidence Interval Estimation (mean & Proportion)"

Confidence interval for the mean of each sample

```{r}
t.test(Sample1)$conf.int
```

```{r}
t.test(Sample2)$conf.int
```

```{r}
t.test(Sample3)$conf.int
```

Confidence interval for the variance of each sample

```{r}
var.test(Sample1, dta$zinc)$conf.int
```

```{r}
var.test(Sample2, dta$weight)$conf.int
```

```{r}
var.test(Sample3, dta$houssiz)$conf.int
```

Hypothesis testing

## 3 - Comparison test of the mean

```{r}
t.test(Sample1, mu = mean(dta$zinc, na.rm = TRUE))
```

```{r}
t.test(Sample2, mu = mean(dta$weight))
```

```{r}
t.test(Sample3, mu = mean(dta$houssiz))
```

## 4 - Comparison tests between the variance of the sample and the sample of the population

```{r}
var.test(Sample1, dta$zinc)
```

```{r}
var.test(Sample2, dta$weight)
```

```{r}
var.test(Sample3, dta$houssiz)
```

# VI - Regression

## 1 - Verification of certains variables
Record our variables age in group ("age_rec") for better control

```{r}
dta<-dta %>%
  dplyr::mutate(
    "age_rec"=ifelse(age %in% 20:35,"[20;35]",
                    ifelse(age %in% 36:55, "[36;55]",
                           ifelse(age %in% 56:69, "[56;69]",
                                  "[70+["))))

```

test to choose the variables whith had khi2<=10%
```{r tests khi2}
# chisq.test(table(dta$diabetes,dta$agegrp)) 
chisq.test(table(dta$diabetes,dta$age_rec))
chisq.test(table(dta$diabetes,dta$race)) # yes
chisq.test(table(dta$diabetes,dta$region))
chisq.test(table(dta$diabetes,dta$rural)) #no p-value=0.91
chisq.test(table(dta$diabetes,dta$sex)) # Non
```

In the course, it is mentioned only the linear regression but for a linear regression, we will just be forced to see the effect of increase in unit of an independent variable on the dependent variable which is diabetes. That is why we are going to use another type of regression in order to see the weight of each group according to the Odds-ratio which are only chance ratios. Because we want to
know if there are categories favorable to diabetes or not, the logistic regression is the most adapted according to our data. 
It is coded between 0 and 1, where the diabetic cases are marked by 1 and the non-diabetic ones by 0.

## 2 - Logit regression
```{r}
reg <- glm(dta$diabetes ~ dta$age_rec + dta$race + dta$region,
           data=dta, family = binomial(link="logit"))

 reg
 odds.ratio(reg)
```

```{r}
summary(reg)
plot(reg)
```

We have already justified the choice of simple logistic regression and we add here, after the chi-square tests, what should be retained. First, the variables associated with diabetes are age, race and region. As age increases, the chance of having diabetes is higher according to our results. White race is the most affected but we also have to notice that it is the most represented in the sample. The region also plays a role in the risk of having diabetes since we have shown that the southern region is the most favorable to this disease. But generally speaking, at the 5% threshold, we can say that the region factor does not affect the occurrence of diabetes in our case.

# Conclusion

In conclusion, we can say firstly that the female gender is the most represented in our study with a percentage of 53% of the studied population and also they are the most diabetic with a percentage of about 5% while diabetic men are about 4% percent. The median age of the population was 49 years with an interquartile range of IIQ= [31-63], without forgetting the IQR that is 32. The included population had a minimum age of 20 years and a maximum age of 74 years. The average weight of the patients was 70.42 kg. At the end of the analyses, we found that the probability of a female patient having diabetes was 0.5. The descriptive analysis allowed us to suspect that the gender has a significant effect on diabete’s occurence as well as the different age groups and also the region of residence. This allowed us to verify by tests such as the chi-2 test through its p-value to know what were the doubtful factors. In view of the content of our variables retained at the threshold of 10%, we considered it appropriate to carry out a logistic regression in order to identify the actors and coast of contribution of links with contribution of each. Following this regression, we notice that age is associated with only 5% and individuals whose age was greater than or equal to 70 years had about 11 times more risk of contracting diabetes and also, those with an age between 56 and 69 years had about 8 times more risk of contracting this disease compared to those who had an age between 20 and 35 years including the youngest. From these results we can say that the age has a significant effect on the contraction of diabetes and that the more the age advances, the risk of contracting diabetes also rises. Concerning the race, the individuals of black race are the most sensitive to the disease. In fact, white race have approximately 50% more chance to be spared of the diabetes than the black one, and the other race has approximately 60% more chance to be spared than black race. For the area of habitation characterized by region, those residing in the northeast and south regions were the most sensitive. In fact, they were generally more likely to develop diabetes than those in the mid-western region. If we were open to making proposals for solutions, we would have recommended to the ministry to be careful about the above-mentioned at-risk housing areas, and also to recommend to the relatives of the elderly people to be careful about them because they are more likely to have this disease.
