## Tutorial on study designs and measures of effect
## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## African Institute for Mathematical Sciences, Muizenberg, RSA
## Jim Scott 2012


## Identifying study designs
## ==============================================================================


## For each of the following descriptions, determine what type of 
## study design was used.  The answers are at the end of this tutorial

## Study #1
## A study examined risk factors associated with falling at an 
## assisted care living facility.  Researchers enrolled 75 patients that 
## had falls and another 211 inpatients that did not have falls.  For each 
## study participant, the researchers examined adverse event reports, 
## medical records and nurse staffing records.  They found that patients 
## with a balance deficit or lower extremity problem were at higher risk 
## for a fall.
##
## What type of study design best describes this study?
##
## a) Cohort Study
## b) Case-Control Study
## c) Cross-Sectional
## d) Correlational Study
## e) Randomized Controlled Trial

## Study #2
## Researchers were interested in identifying risk factors associated 
## with needle stick injuries among medical students.  To do so, a survey
## was mailed to  417 medical students at a National University.
## The survey included questions about demographic factors, knowledge of 
## needle handling protocols, and episodes of needlestick injury.  Over all,
## 59 students (14.1%) reported experiencing one or more needle stick injuries.  
## Invesigators found that those who reported having attended at least one 
## needle handling seminar had a lower prevalence of injury compared to 
## those that had not reported attending a seminar on needle handling.
##
##
## What type of study design best describes this study?
##
## a) Cohort Study
## b) Case-Control Study
## c) Cross-Sectional
## d) Correlational Study
## e) Randomized Controlled Trial

## Study 3
## Investigators sought to determine if water treatment via solar radiation
## is effective at reducing the overall incidence of diarrheal illness.  To 
## do so, researchers solicited participants from two neighboring towns.  
## All participants recieved clear plastic water containers.  However, 
## participants in town A were asked to treat their drinking water
## using the solar radiation method while those in town B were given no specific
## instructions.  After 6 months of follow-up, diarrhea incidence rates were 
## compared.
##
## What type of study design best describes this study?
##
## a) Cohort Study
## b) Case-Control Study
## c) Cross-Sectional
## d) Correlational Study
## e) Randomized Controlled Trial

## Study 4
##
## Is alcohol consumption associated with HIV transmission?
## To answer this question, researchers collected data on alcohol sales 
## and HIV prevalence in in 48 different countries.  To control for possible
## confounding, additional data such as GDP (Gross Domestic Product), 
## unemployment, and education were also included in the analysis. 
##
## What type of study design best describes this study?
##
## a) Cohort Study
## b) Case-Control Study
## c) Cross-Sectional
## d) Correlational Study
## e) Randomized Controlled Trial


## Analyzing a 2 x 2 table
## ==============================================================================


## In order to demonstrate 2 x 2 table analysis and to calculate measures of effect,
## we'll look at some data collected by Lefevre, et. al (2010).  In that study 
## researchers conducted an experiment to determine if beer consumption increases how
## attractive humans are to mosquitoes.  In short, A number of  volunteers were 
## randomized to consume Beer.  Subsequently, mosquitoes were released into a
## controlled apparatus that led them to tents filled with study participants or an
## empty tent of outdoor air (uncontaminated by participants).  Two different mosquito
## releases were performed, once before beer was consumed and once after.
## You can read more details about the complete experiment online:
##  https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0009546

## Prior to beer consumption, 215 mosquitoes flew towards the tent containing study 
## participants while 219 flew toward the outdoor air.  After beer consumption 369
## mosquitoes flew toward the participants while only 221 flew towards the outdoor air. 

## We can replicate these data in R by entering the following commands:

Timing <- c(rep(1,590),rep(0,434))
Choice <- c(rep(1,369),rep(0,221),rep(1,215),rep(0,219))

Timing <- factor(Timing, levels = c(1,0), labels=c("After","Before"))
Choice <- factor(Choice, levels = c(1,0), labels=c("Human","Outdoors"))

MosquitoData <- data.frame(Timing,Choice)

## To see the data in table format you can use the table() command:

table(MosquitoData)

## Examining the data:
## It's usually a good idea to visually inspect the data with an appropriate graph.
## Immediate results can be attained by entering the following command:

barplot(table(MosquitoData))

## There are a number of reasons why this isn't a very satisfying plot.  Take a 
## a minute to consider what it's lacking.  How can it be improved?

## For one, it would probably make more sense to have the data stratified by 
## exposure - in this case, Timing of mosquito release -  before or after beer
## consumption.  Also, the plot is lacking appropriate labels and a title.

table(Choice, Timing)  # Timing variable now in columns
# or
t(table(MosquitoData)) 

## This is an immidiate improvement, but still somewhat misleading:
barplot(table(Choice,Timing),main="Mosquito choice by Timing of release",
        xlab="Timing of release relative to beer consumption", 
        ylab="No. Choosing Participants (dark)", 
        col = c("darkblue", "lightblue","darkblue", "lightblue"))

## Better is to show the actual distribution of mosquito choice by Beer status using
## percentages:
prop.table(table(Choice,Timing),margin=2)

## This can be directly inserted into the barplot command:
barplot(prop.table(table(Choice, Timing),margin=2),
        main="Distribution of Mosquito Choice by Timing of release",
        xlab="Timing of release relative to beer consumption", 
        ylab="Proportion Choosing Participants (dark)", 
        col = c("darkblue", "lightblue"))

## Now the scales are comparable and it's clear that a greater proportion of 
## mosquitoes were attracted to the participants after they consumed the beer.

## The above code demonstrates the flexibility of R in creating plots.  Try 
## experimenting with different colors.  Also, it's probably more appropriate 
## to change the column ordering so that the 'Before' column is first.  Try using
## your knowledge of R's indexing system to do this on your own.  One possible 
## answer appears at the end of this tutorial. (A)

## Now that you've looked at the data, a natural question that arises is:
## "Did drinking the beer really increase the attractiveness of the participants?
## (as far as the mosquitoes are concerned, that is!)" -or- "Could the observed
## difference be due to chance?"

## There are a number of statistical tests that could be used to answer this question.
## (for example, you could use a permutation test)
## Possibly the simplest method would be to use a Chi-square test of independence.
## The null hypothesis for this test is that the column and row variables are
## independent.  In this case, we could state the null hypothesis as:
## "Beer consumption has no effect on participant attractiveness".  The chi-square
## test statistic will have an approximate chi-square distribution with 
## (r - 1)*(c - 1) degrees of freedom (where r and c represent the number of rows
## and columns in the table - here df = 1) as long as the number of observations in 
## each cell is not "small".  You can get the chi-square test statistic, df, and 
## p-value from R by using the summary command in conjunction with table():
summary(table(Choice,Timing))

## The resulting p-value is very small, which provides evidence against the null
## hypothesis.  Conclusion: the evidence suggests that beer consumption and 
## attractiveness are not independent(!).  Of course, more research is needed.
## Chi-square test results can also be obtained in R by using the chisq.test()
## command:
?chisq.test

## Perform the same chi-square test that you did previously, but this time, use 
## the chisq.test() command.  Note: you may need to change one of the input arguments
## to get results that exactly match those that you previous obtained. (B)

## Measures of effect
## The odds ratio (OR) and relative risk (RR) can be calculated directly from our table:
table(Choice,Timing)

## Take a minute to perform these calculations by hand - then check your results 
## using R, below.  Also be sure you know how to interpret these measures.  which of these
## measures of effect is most appropriate for the given study? (C)  

## It's good practice to provide confidence intervals (CIs) when reporting ORs and 
## RRs. These convey the degree of uncertainty (due to sampling) that is present in  
## the estimate(s) and represent a range of plausible values for the true measure of 
## effect.  Different methods exist for calculating CIs.  We'll rely on R to do the 
## calculating for us.  One way to obtain CIs is via the 'epiDisplay' package.  In 
## R-studio, it should be listed under the 'Packages' tab.  To load it you need 
## check its box or, alternatively, type: library(epiDisplay). If it isn't installed,
## first run install.packages('epiDisplay'); or you can click the 'Install' button
## and type 'epiDisplay'.

library(epiDisplay)

## Once epiDisplay is loaded, you have access to many commands relevant to 
## epidemiological analysis.  The command cci() provides the OR, CI, and results
## from a number of hypothesis tests associated 2x2 tables. 
## You can get help using:
## ?cci

## Here it makes sense to consider 'Timing' as the exposure (After = exposed, 
## Before = unexposed). 'Choice' could play the role of "disease" (Human = case
## Outdoors = control).
table(Choice, Timing)

## Selecting the appropriate values for the cci command:
cci(369, 221, 215, 219, graph=FALSE)

## Try graph=TRUE too (or because TRUE is the default value of graph, you can omit the
## graph argument). Try to add appropriate labels to the graph too. (D)

## Analogously, metrics associated with the RR can be obtained through epiDisplay's
## csi() command.  Try using csi() to obtain a CI for the RR. Note: csi doesn't 
## plot a graph for a 2x2 table, so you don't need to specify graph=FALSE. (E)

## Based on the CI's for the OR and RR, what conclusions can you draw about the 
## relationship between the variables in the table? (F)

## Note that while it useful to do the calculation above to build understanding,
## in practice, you would avoid entering numbers by hand if you have a dataset. 
## We could have instead used
cc(Choice,Timing)
# or 
cc(MosquitoData$Choice,MosquitoData$Timing)


## Often, it is necessary to control for the effects that a potentially confounding 
## variable may have on an exposure/disease relationship.  When confounding is
## present, it is not possible to obtain an unbiased measure of effect
## One way to determine if confounding is present is through the application 
## of stratified analysis. Consider the following raw dataset from a 
## hypothetical case-control study investigating gender as a risk factor for
## Malaria (adapted from Szklo & Nieto, 2000):

## replicate raw data:
Gender <- c(rep(1,156),rep(0,144))
Malaria <- c(rep(1,88),rep(0,68),rep(1,62),rep(0,82))
Workplace <- c(rep(1,35),rep(0,53),rep(1,53),rep(0,15),rep(1,52),rep(0,10),rep(1,79),rep(0,3))

Gender <- factor(Gender, levels = c(1,0), labels=c("male","female"))
Malaria <- factor(Malaria, levels = c(1,0), labels=c("case","control"))
Workplace <- factor(Workplace, levels = c(1,0), labels=c("indoor","outdoor"))

MalariaData <- data.frame(Gender,Malaria,Workplace)

## take a look at the first few lines of raw data:
head(MalariaData)

## examine the relationship between Gender and Malaria:
table(Gender,Malaria)

## An OR can be obtained using the cci command.  Try it out.  Note: remember 
## the format is cci(caseexp, controlex, casenoex, controlnoex, graph=FALSE).  
## Assume exposed = male. (G)

## You should have found the OR = 1.71.  This represent the "crude" or "unadjusted"
## OR.  It suggests that the odds of malaria is higher for men than for women.  Now,
## let's see what happens when we stratify the data by workplace.  If workplace is 
## unrelated to these data (i.e. not a confounder) then we should get approximately
## the same OR (OR=1.71) as we did before for both levels of workplace.
table(Gender,Malaria,Workplace)

## Compute the ORs for each table separately using cci(). (H) 

## The resulting OR's are both close to 1.00.  This reveals two things: 
## 1) Workplace appears to be a confounder - the stratified ORs differ 
## from the crude OR. 2) Since the stratified ORs are approximately
## equal, workplace does not appear to be an effect modifier 
## (i.e. the ORs do not vary by workplace).  
##
## We can further explore the confounding nature of  Workplace by examining the 
## relationships between workplace & gender and workplace & malaria
table(Workplace,Gender)
cci(68, 88, 13, 131,graph=FALSE) ## Males are more likely to work outdoors

table(Workplace,Malaria)
cci(63, 18, 87, 132, graph=FALSE) ## Malaria is associated with working outside

## Because workplace is associated with both gender and malaria, it is not 
## surprising that it had a confounding effect on the relationship between
## gender and malaria.

## To complete the analysis a Mantel-Haenszel method for stratified data (not 
## covered in this tutorial) could be applied to determine a combined, adjusted
## measure of effect. 
## 
## Stratified analysis is perhaps most useful when variables are categorical and 
## the overall number of variables is small.  When dealing with a larger number of
## variables (e.g. many confounding factors) or continuous explanatory variables,
## generalized linear model methods such as logistic regression can be used to 
## estimate adjusted measures of effect.  

## For those of you that are already familiar with these types of models, you can
## use R to fit a logistic model to these data using the following commands:

my.model <- glm(Malaria=='case' ~ Gender + Workplace, family=binomial)
summary(my.model)

## The adjusted OR for malaria and gender after controlling for workplace can be
## obtained using:
exp(my.model$coefficients) 

## confidence intervals for the ORs can be obtained using:
exp(confint.default(my.model))
