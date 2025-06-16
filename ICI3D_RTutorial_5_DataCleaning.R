## Data management and cleaning in R
## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## African Institute for Mathematical Sciences, Muizenberg, RSA
##
## (C) Juliet Pulliam, 2011, 2012, 2017
##
## The goal of this tutorial is to acquaint you with ways of
## manipulating and cleaning infectious disease data in R.  By
## the end of the tutorial you should be able to:
##
##  * import CSV files into R
##  * ensure imported data is interpreted correctly by R
##  * identify errors and inconsistencies in a data frame
##  * correct identified data errors
##
## NOTE: The comments will guide you through the tutorial but you
## should make sure you understand what the code is doing.  Some
## function arguments are assigned to ?.  This will give you an
## error. You should try out values for these arguments as
## suggested in the comments or find them yourselves in a help file.


######################################################################
## First, you need to import the data you will be working with.
## You should make sure you are in the same
## working directory as your data.

## You presumably downloaded the data and this script to the same place
## You can make sure R studio is "there" by clicking on Session/Set Working
## Directory/To Source File Location

## If you have files in a different place than the script, you can use
##Session/Set Working Directory (or a keyboard shortcut) to navigate there.

## We generally don't recommend using setwd() in scripts, because it causes
## difficulties in sharing and collaboration.

## For this tutorial, we will be using an example dataset that
## resembles (but is not) real data from a measles epidemic in
## Democratic Republic of Congo. The "data" are made available only
## to ICI3D participants and are for use only during the MMED and
## DAIDD clinics, and only for the specified purposes.

## We will be using the 'tidyverse' family of R functions to read in
## and clean the data. Let's get started.

## Data are available at https://github.com/ICI3D/datasets under the dataCleaning folder

library(tidyverse) # Load the tidyverse package
dat  <-  read_csv("tutorial5.csv") # read in the "data"

## The read_csv() function reads data in as a tibble, a data
## structure that is very useful because it can contain different
## data types in different columns (similar to a data frame, but unlike
## a matrix or array).

######################################################################
## Let's look at the data to get a feeling for the size of the
## data set and what variables we have to work with.

dim(dat)			# Determine the number of rows and columns
dat		        # Look at the beginning of the dataset

## For each individual in our data set, we have the following
## information:
##
##  * Province - the province where the case occurred
##  * ZS - the health zone where the case occurred
##  * Age_annes - the person's age in years
##  * Age_Mois - the person's age in months
##  * Sexe - the person's sex
##  * Date_dern_vacci - the date when the person was vaccinated against
##    measles
##  * DDS - the date of symptom onset
##  * test - the result of a diagnostic test for IgM antibodies against
##    measles

## The read_csv() function has read in most of our data as characters.
## Note that this is a different behavior from what would have happened
## using the read.csv() function demonstrated in Thumbi's lecture, and
## this fixes the problem he demonstrated where factors are sometimes
## treated as numbers when they shouldn't be. It also gives us some
## useful information about problems we're likely to discover with
## the dataset as we explore further - for example, the fact that the
## Age_annes column has been read in as characters indicates there are
## likely to be some problematic entries.

## We should always go through our data sets to ensure that
## R treats each variable as the appropriate data type. To do this,
## it will help to know the different ways in which R can store and
## interpret data. To see a list of the options for data types:

## ?typeof

#########
## Hands off the keyboard! Pick up a writing pad and...
#########
##
## Decide what data type each variable in the data set should be.
## Refer to the lectures on study design or data cleaning, if necessary.
##
#########

## Before we start working with the data itself, let's rename the columns
## so they're easier to interpret.

dat <- rename(dat
              , province = Province
              , healthZone = ZS
              , ageYears = Age_annes
              , ageMonths = Age_Mois
              , sex = Sexe
              , dateVaccinated = Date_dern_vacci
              , dateOnset = DDS
              , testResultIgM = test
)

## Here is a cleaned up version of the data key, with the new
## column names and information about the data types we will end up
## with after cleaning the data:
##
##  * province - the province where the case occurred (character)
##  * healthZone - the health zone where the case occurred (character)
##  * ageYears - the person's age in years (integer)
##  * ageMonths - the person's age in months (integer)
##  * sex - the person's sex (character)
##  * dateVaccinated - the date when the person was vaccinated against
##    measles  (character)
##  * dateOnset - the date of symptom onset (character)
##  * testResult - the result of a diagnostic test for IgM antibodies against
##    measles
##
##  We will also create some new variables as we clean the data:
##
##  * ageYearsContinuous - the person's (approximate) age in years, as a
##  continuous
##  variable (double)
##  * vaccinationStatus - whether the person had been previously
##  vaccinated (logical)
##  * testStatus - additional information about the person's diagnostic
##  status (character)

######################################################################
##
## Let's begin by looking at ageYears, which should be stored as a
## numeric value.

mode(dat$ageYears)		# determine the storage mode of a variable

## As noted above, ageYears is being treated as a character, which
## likely indicates a problem with some of the entries; let's take
## a closer look:

distinct(dat,ageYears)

## The distinct() function lists the unique values for a variable or
## or set of variables. So far, everything looks fine, but the
## function only shows us as much of the data as will fit in the
## console window. We need to look at all of the values to indentify
## what's going on:

## CONSOLE (you can try View() instead of print())
print(
  dat
  |> distinct(ageYears)
)

## Scrolling down, we can see the problem: some of the entries have a
## value of 'ND' which of course is not a number, so read_csv()
## decided to treat the entire column as characters. In this dataset,
## 'ND' is a way of denoting missing values, and we can correct
## this easily by telling R that everything in this column should be
## a number. To see what this will do, let's look at the unique values
## again after we cast ageYears as an integer:

## CONSOLE
View(
  dat
  |> distinct(as.integer(ageYears))
)

## We see that 'ND' is no longer in the list of values and has been
## replaced by NA, which stands for 'not applicable' and is R's way
## of treating missing information. We can see that R is treating NA
## differently from 'ND' because the NA row is now greyed out. Let's
## update the dataset to reflect this correction:

dat <- (
  dat
  |> mutate(ageYears = as.integer(ageYears)) # replace
)

## Now let's look more closely at the data in this column to verify
## that the values make sense:

## CONSOLE
print(
  dat
  |> distinct(ageYears)
  |> arrange(ageYears)
)

## This time, instead of just looking at all the values, we've arranged
## them in ascending order, so we can easily see whether all of the
## numeric values are plausible. We can also look explicitly at the range
## of values given:

range(dat$ageYears,na.rm = T)

## Note that the na.rm argument tells the function whether to drop the
## missing values before showing us the range. We have set it to TRUE,
## so we see the range of values after the NAs have been removed.

## Now let's look at the column giving age in months.

mode(dat$ageMonths)		# determine the storage mode of a variable

## The variable is stored as a numeric value, as seems appropriate.
## Let's also verify that this numeric value is not representing a
## factor:

class(dat$ageMonths)		# determine the object class of a variable

## Great! The ageMonths column seems to be in better shape that the
## ageYears column was. Fill in the missing information in the command
## below to take a closer look:

## CONSOLE
print(
  dat
  |> distinct(???) # View distinct values for month ## FIXME
  |> arrange(???) # Arrange values in ascending order ## FIXME
)

## All of the values are numeric, but at least one row has a value of 25,
## which is hard to interpret. In particualr, we need
## to be careful that we understand how the two age columns relate
## to each other, or we could run into trouble at the data analysis phase.

## Let's start by looking at the data with missing values for ageMonths. We
## can also look at the rows that have missing values for ageYears at the
## same time:

print(
  dat
  |> filter(is.na(ageMonths)|is.na(ageYears)) # subset the rows that have NA for either age variable
)

## There is one individual with missing values, and they are missing both year
## and month information. While it's not ideal to have missing data, this is
## a reality in most datasets, and in this case it's not that bad.

## Now let's look at the ageMonths values for the subset of the
## data that has an ageYears value of 0:

print(
  dat
  |> filter(ageYears==0) # subset the rows that have 0 for ageYears
  |> group_by(ageMonths) # group by ageMonths
  |> summarize(count = n()) # count occurrences of each ageYears value
)

## All ageMonths values for children with ageYears of 0 are between 0
## and 11 (inclusive). This seems like a good start. Now let's look the
## ageYears values for the rest of the data:

print(
  dat
  |> filter(???) # subset the rows for ages of at least 1 year ## FIXME
  |> group_by(ageMonths) # group by ageMonths
  |> summarize(count = n()) # count occurrences of each ageMonths value
)

## There is only one row in the dataset where ageMonths is greater than 11,
## so let's take a closer look at that particular row:

print(
  dat
  |> filter(ageMonths==25)
)

## How confusing! Both the ageYears and ageMonths values are 25.
## At this stage, we would like to go back to the source of the data
## and double-check the recorded age for this individual. Let's say we
## go back to the data source and learn that the individual had just
## turned 25, meaning that the months entry should be 0 and there was a data
## entry error where the age in years was accidentally entered in both
## columns. We can easily correct this.

## Let's first set up a test case on a simplified version of the dataset:

print(
  dat
  |> select(ageYears,ageMonths) # reduce the number of columns for test case
  |> filter(ageYears==25) # subset to look only at the data for 25 year olds
  |> mutate( ageMonthsNEW = ifelse(ageYears == 25 & ageMonths == 25, 0, ageMonths)) # correct the typo
)

## It looks like the mutate command is appropriately correcting the typo, so we
## can now be comfortable replacing the values in the actual data frame:

dat <- (
  dat
  |> mutate(ageMonths = ifelse(ageYears == 25 & ageMonths == 25, 0, ageMonths))
)

## Note that it is very difficult to pick up errors in the age variables if they
## don't fall outside the range expected for age or years. To reduce errors that
## are difficult to spot, data are often entered twice by different people
## and then values are compared to look for inconsistencies.

## Now that we have cleaned versions of both ageYears and ageMonths, let's put
## them together to create a new variable ageYearsContinuous that treats age as continuous.
## We can do this using the mutate() function that we saw above. Again, we'll
## first look at the output for a test case and the update the actual data frame:

## CONSOLE
print(
  dat
  |> select(ageYears,ageMonths) # reduce the number of columns for test case
  |> mutate( ageYearsContinuous = ageYears + ageMonths/12)
)

## Scan the values of the columns to make sure the conversion has been done right.
## If you were doing a more complicated transformation, you would probably want to
## build in additional checks to identify potential errors.

## The transformation looks good, so let's update our data frame to add the new variable.
## Now that we have a new variable that contains all of the age information, we can also
## remove the original age variables, which are difficult to interpret.

dat <- (
  dat
  |> mutate( ageYearsContinuous = ageYears + ageMonths/12) # create new variable
  |> select(-ageYears,-ageMonths) # remove original age variables
)

summary(dat)

ggplot(dat, aes(x=ageYearsContinuous)) + geom_histogram()

######################################################################
## PROBLEM
######################################################################
##
## So far we've been correcting errors in a somewhat adhoc manner. In
## the lecture, we saw how to do this is a more robust way
## using a correction table. Follow the examples from the lecture to
## correct the province names for Kasai Oriental and Kasai Occidental
## in the using the correction table kasaiCorrectionTable.csv
##
######################################################################

corTab <- read_csv('kasaiCorrectionTable.csv')
dat <- ... ## FIXME
