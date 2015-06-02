## Data management and cleaning in R
## Meaningful Modeling of Epidemiological Data, 2012
## AIMS, Muizenberg
## (C) Juliet Pulliam, 2011, 2012

## The goal of this tutorial is to acquaint you with ways of
## manipulating and cleaning infectious disease data in R.  By 
## the end of the tutorial you should be able to:
##
##  * import CSV files into R
##  * ensure imported data is interpreted correctly by R
##  * identify errors and inconsistencies in a data frame
##  * correct identified data errors
##  * develop functions to repeat common data-cleaning tasks
##  * save your cleaned dataset with a date tag for version control
##
## NOTE: The comments will guide you through the tutorial but you
## should make sure you understand what the code is doing.  Some
## function arguments are assigned to ?.  This will give you an
## error. You should try out values for these arguments as
## suggested in the comments or find them yourselves in a help file.

######################################################################
## Section 1: Importing and formatting data
######################################################################

######################################################################
## 1A - First, you need to import the data you will be working with.
## As in earlier tutorials, you should make sure you are in the same
## working directory as your data.

getwd() # shows you what directory you are currently in

## Next you should change the working directory to where the data is
## kept, and you can replace it with where you have saved the data set.

setwd("???") # ENTER THE PATH TO THE DIRECTORY WHERE YOUR DATA ARE

## For this tutorial, we will be using a dataset from an outbreak of 
## food-borne gastrointestinal illness in the US in 1940. The clean
## data are available as part of the R package epicalc, but we will 
## be using an "uncleaned" version available on the MMED wiki.

oswego  <-  read.csv("oswegoTutorial.csv")

## The read.csv() function reads data in as a data frame, a data 
## structure that is very useful because it can contain different
## data types in different columns (unlike a matrix or array).

######################################################################
## 1B - Let's look at the data to get a feeling for the size of the 
## data set and what variables we have to work with.

dim(oswego)			# Determine the number of rows and columns
head(oswego)		# Look at the first five rows of data

## For each individual in our data set, we have the following
## information:
##
##  * age - the person's age in years
##  * sex - the person's gender
##  * timesupper - the time the person ate (nearest half hour)
##  * ill - whether the person developed GI illness after the supper
##  * onsetdate - the date of onset of illness for those who became ill
##  * onsettime - the time that the person reported first feeling ill
##    (nearest half hour)
##  * 15 variables indicating whether the person reported eating specific 
##    food items at the supper

#########
## Hands off the keyboard! Pick up a writing implement...
#########
## 
## Decide what data type each variable in the data set should be.
## Refer to the lectures on study design or data cleaning, if necessary.
##
#########

######################################################################
## 1C - Cleaning Numeric Data
##
## We will go through each column in the data set to ensure that 
## R treats each variable as the appropriate data type. To do this, 
## it will help to know the different ways in which R can store and
## interpret data. To see a list of the options for data types:

?mode

## Let's begin by looking at how the age variable is stored.

mode(oswego$age)		# determine the storage mode of a variable

## The variable is stored as a numeric value, as seems appropriate, 
## but if we check the variable's class, we see that the data in this
## column will be treated as a factor by R:

class(oswego$age)		# determine the object class of a variable

## What is going on? It turns out that factors are stored as integers,
## even though R does not normally treat them as numeric values. To see
## this, look what happens when we try to calculate the average age:

mean(oswego$age)		# calculate the average age - ERROR

## This calculation gives us an error because R is not treating the
## the factor as a numeric value. To understand why, let's look at the 
## values in the data vector:

oswego$age				# look at the vector of age values

## Oops! One of the values was entered as "seven" when it should have
## been entered as the number 7. Thus, when the data were imported
## into R, the entire column was read in as character strings and 
## the variable was converted to a factor.

## There are several ways to correct an error of this sort. The most
## straightforward would be to edit the data file directly and re-
## import the data into R, but this option would not leave a record
## of the change. A better option would be to make the change in your
## data-cleaning script. 

## A good way to do this would be to begin by replacing the level
## "seven" with the level "7":

levels(oswego$age)		# the original levels of the factor
levels(oswego$age)[levels(oswego$age)=="seven"] <- "7"
						# change the level "seven" to "7"

## Now let's look at the levels of the factor again:

levels(oswego$age)		# the updated levels of the factor

## Notice that the number of levels has decreased from 47 to 46. This
## is because there was aleady a level "7" and R has (correctly) 
## interpreted the renaming of level "seven" to mean that we want that
## level to become part of the original level "7" (or, rows where the 
## value of the variable is 7).

## The vector of values itself is now:

oswego$age

## All of the remaining levels of the variable look like integers, so
## we can now think about converting the variable to a numeric class.
## We do this by converting the vector first to a set of character
## strings, then into a set of numeric values. To make sure we do this
## properly, we'll create a new variable AGE and check that the 
## conversion has worked the way we want it to before replacing the 
## vector in the data frame.

AGE <- as.numeric(as.character(oswego$age))
						# convert the factor to a numeric class
class(AGE)				# check the conversion
AGE						# view the new version of the variable
oswego$age
AGE						# confirm correct conversion of values

## This looks good!

#########
## WARNING!
#########
## 
## What would have happened if we had converted to a numeric vector
## without passing through a character vector first? Why wouldn't this
## work as we'd like it to?  You can try this to see that it doesn't.
## What is R's interpretation of this conversion?
##
#########

## We can now replace the column in the data frame with our new
## vector:

oswego$age <- AGE			# replace the old values with the new version

## Finally, we should check that all of the values in our numeric age
## vector make sense. To get a quick idea, we can look at the range of
## values in the vector:

range(oswego$age)		# range of age values

## Uh-oh! At least one value is outside the range we expect for age in
## years. We can look into this in more detail by looking at a
## histogram of the data:

hist(oswego$age,		# histogram of age vales
	col="dark grey",	# color of the bars
	xlab="Age in years",# label for the x axis
	main="???",  		# CHOOSE an appropriate title
	breaks=???)			# TRY 10, 0:622, seq(0,640,20)

## Looks like most of the values are in a plausible range. To confirm 
## this, let's look at the subset of the data for which age is over
## 100:

subset(oswego,age>100)	# subset of data with age > 100

## It's only this one value that is a problem (as far as we can tell).
## At this stage, we would like to go back to the souce of the data
## and find out what the actual value should be. Here, it turns out
## there was a data entry error where too many twos were entered, and 
## the value should have been 62. We can easily make this correction.

age.error <- which(oswego$age>100)	# get index of the appropriate row
oswego$age[age.error] <- 62			# replace the old value 
oswego[age.error,]					# look at the corrected row

## Note that it would be very difficult to pick up errors in this 
## variable if they didn't fall ouside a reasonable range for age in
## years. To reduce errors that are difficult to spot, data are often
## entered twice by different people and then values are compared to 
## look for inconsistencies.

######################################################################
## 1D - Cleaning Categorical Data
##
## We'll now repeat this process of examining and correcting
## the data for the next column, sex.

## Again, we'll look at the storage mode and object class to determine
## how R is currently treating the variable:

mode(oswego$sex)		# storage mode
class(???)				# object class

## R seems to think that sex is a numeric value and is treating the
## variable as an integer. This is strange. Let's see if we can figure
## out why, by looking at the values in the vector:

range(oswego$sex)		# range of values

## The values of the vector range from -1 to 2, which explains why R is
## considering them to be integer but not what the values mean. Let's
## take a closer look:

table(oswego$sex)		# table of how many times each value occurs

## Hmm... There are only 3 values and mostly the values are 1 and 2.
## Given the name of the column you can probably guess that 1 and 2 
## are a numeric code for Male and Female, but you would have to find
## out from the data source or code book which is which. If possible,
## you should also go to the source to find out whether the value -1
## is a typo or has some sort of meaning (such as standing in for a
## missing value).

## In this case, it turns out the value -1 was a typo and should have
## been a 1. Also, 1 stands for Female and 2 stands for Male.

## First, let's correct the value we know is wrong:

sex.error <- which(oswego$sex==-1)	# get index of the appropriate row
oswego$sex[sex.error] <- 1			# replace the erroneous value
oswego[sex.error,]					# look at the corrected row

## Let's look at the table of values again, now that we've made the 
## correction:

table(oswego$sex)		# table of how many times each value occurs

## Now all that's left is to format the variable so that R interprets
## these values as categories. To do this, we'll use the factor()
## function. First, let's look at the help file for this function. You
## should read through the arguments and understand what they mean. It
## may be useful to remember the discussion of factors and levels from
## the second Introduction to R tutorial.

?factor					# help for factor()

## Now let's make our factor as a new vector SEX: 

SEX <- factor(oswego$sex,	# input the vector of values from the data set
	levels=c(1,2),		# these are the current values of the variable
	labels=c("Female","Male"))
						# these are categorial names for the values

## Note that our new vector is still stored as a set of numeric values
## (as are all factors) even though R will correctly interpret the 
## object as a factor with categorical values:

mode(SEX)				  # storage mode
class(SEX)				# object class

## To make sure we've made the conversion correctly, let's compare the
## table of values for the original vector and our new vector:

table(oswego$sex)		# table of values from original vector
table(SEX)				  # table of values from new vector

## These are comparable, so we can now replace the orginal vector with
## the new one:

oswego$sex <- SEX			# replace the old values with the new version

######################################################################
## 1C - Cleaning Data on Dates and Times
##
## The next column represents the times that people ate, and it turns
## out that R has an object class specifically for date data.

## As before, we'll look at the storage mode and object class to determine
## how R is currently treating the variable:

mode(oswego$timesupper)  	# storage mode
class(oswego$timesupper)				# object class

## Again, R seems to think the variable has a numeric value and is treating the
## variable as an integer. This doesn't seem too unreasonable, so let's see
## what the range of values looks like:

range(oswego$timesupper)  	# range of values

## Well, that doesn't help, except to tell us that some values are missing
## for this variable. Let's try again, this time removing the missing values:

range(oswego$timesupper,na.rm=T)    # range of values, without missing values

## This looks like it may be telling us the time in hours and minutes, which
## is, in fact, the case. It seems a bit odd, though, that someone would eat
## supper at midnight, as seems to be indicated by the value of 0. Let's see
## what the distribution of values looks like:

table(oswego$timesupper)   	# table of how many times each value occurs

## Almost everyone with a known supper time ate between 18:30 and 22:00, and
## these seem like normal supper times, but the values 0 and 1100 make less
## sense. Let's see if we can discern whether these values are plausible.

## A first step might be to look at the other information we have on these two
## individuals:

subset(oswego,timesupper<1600) # rows for individuals with odd supper times

## The first individual did not get ill, so no data are available on the date
## of onset or the onset time. Therefore, we cannot use this information to help
## us out. In this scenario, we cannot go back to the original data source and
## find out whether this person really did eat supper at midnight (in part because
## the outbreak occurred in 1940!)

######################################################################
## PROBLEM 1
######################################################################
## 
## What do you think is the best course of action in this scenario? 
## How likely do you think it is that the supper time for this individual
## is accurate? Is it better to leave this datum as is, or to change
## the value of time supper to indicate that this value is missing (ie,
## set it to be NA)?
##
## Try to think of arguments in both directions, then make a decision
## about what to do.
## 
## If you decide that the best option is to set the value to NA, then do
## this now by adding appropriate lines of code below.
##
######################################################################

## The second individual listed as eating before 4pm, and 8 year old boy
## who is listed as eating supper at 11am, did get ill and has an onset
## time listed as 3pm. Let's pretend for now that we know the supper in
## question occurred on April 18, which also happens to be the onset date
## for this case. 

######################################################################
## PROBLEM 2
######################################################################
## 
## Repeat Problem 1 for the second individual. Does having the additional
## information about the onset date and time help you make your decision,
## or does it make the decision more difficult. Why?
## 
## If you decide that the best option is to set the value to NA, or to
## make some other change to the dataset based in the early supper time
## for this case, then do this now by adding appropriate lines of code
## below.
##
######################################################################

## Once you are satisfied that you have corrected any likely errors in
## the timesupper column, it is time to convert the data to the date 
## format. To do this, we will use the strptime() function.

?strptime                # help file for strptime()

## The first argument of strptime() is the value input that you want to
## convert to a date/time format, and the second argument specifies the 
## format for interpreting the date/time information. Under "Details" in
## the help file, you will find information about how to specify the 
## current format for conversion. In our case, the times are given as
## hours (00-23) and minutes (00-60), which are specified by the strings
## "%H" and "%M", respectively. So let's use this information and try to
## convert the column to a date/time format, creating a new variable
## SUPPER from the output:

SUPPER <- strptime(oswego$timesupper,format="%H%M")  # convert timesupper
                                                     # to date/time

## Let's see what we get:
                   
head(SUPPER)

## Wait a minute... we said the date was April 18, 1940, but R does not know 
## this and has, by default, interpreted the date to be today. To correct this,
## we'll have to tell R we want it to use a different date. Since the
## date is the same for all of the times, this is relatively straightforward.

## Let's create a string called supperdate, which defines the date we want to 
## use, then paste this together with the string that represents the time:

supperdate <- "1940-04-18"         # April 18, 1940
supper.datetime <- paste(supperdate,oswego$timesupper) # supper date and time

## Now, see what this new vector, representing the dates and times that 
## you want in the timesupper column, looks like:

head(supper.datetime)

## The next step is to use strptime() to convert this vector of strings to the
## date/time class. Notice that we now have to include the conversion for the
## date in the format argument as well.

SUPPER <- strptime(supper.datetime,format="%Y-%m-%d %H%M")  # convert to date/time

## Let's see if this worked like we wanted it to:

head(SUPPER)

## That's looks better! We can now replace the column in the data
## frame with our new vector:

oswego$timesupper <- SUPPER  		# replace the old values with the new version

######################################################################
## PROBLEM 4
######################################################################
## 
## Check the values of the information in the two columns onsetdate
## and onsettime. Correct any suspected errors.
##
## Now, use the columns onsetdate and onsettime to create a new column,
## onset, of the date/time object class that includes full information
## about the timing of onset. First, check the values in each of the 
## two columns and correct any suspected errors.
##
######################################################################


######################################################################
## PROBLEM 5
######################################################################
## 
## The data in all of the remaining columns are currently formatted
## as factors with the levels "yes" and "no". Figure out how to convert
## a factor of this type to a logical, which has the values TRUE and
## FALSE. (Hint: Be careful to account for potential missing data, but
## note that catching any data errors would be virtually impossible - this
## is an example of where entering data multiple times is essential for
## avoiding data errors.)
##
## Convert the ill column from yes/no (stored as a factor) to a logical 
## format. Check the new format confirm that the number of TRUE
## values matches the number of "yes" entries in the oringial column,
## and confirm that the number of FALSE values matches the number of
## "no" entries in the oringial column.

######################################################################
## PROBLEM 5
######################################################################
##
## Write a function that can be used to convert a yes/no factor into
## a logical, and use your function to convert the remaining columns of
## data.
##
######################################################################

## Now that you've finished cleaning the dataset, you should save your
## cleaned dataset with a date tag for version control. First, create
## a variable to represent today's date:

today <- format(Sys.Date(),"%d%b%Y")   # today's date

## Now, create the filename to use, which incorporates the date, so 
## you will be able to easily track version of your data file:

fn <- paste("oswegoClean",today,".Rdata",sep="") # filename
fn

## Save the dataframe with your cleaned data:

save(oswego,today,file=fn)

