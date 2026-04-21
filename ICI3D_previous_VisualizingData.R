## This tutorial has been REPLACED by another Tutorial 4 ☺

## Visualizing infectious disease data in R
## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## African Institute for Mathematical Sciences, Muizenberg, RSA
## (C) Steve Bellan, 2010
## Updated by Juliet Pulliam, 2018

## NOTE: The comments will guide you through the tutorial but you
## should make sure you understand what the code is doing.  Many
## plotting parameters are assigned to ?.  This will give you an
## error, you should try out different values for these parameters as
## suggested in the comments or find them yourselves in a helpfil.

######################################################################
## Section 1: Plotting prevalence
######################################################################

######################################################################
## 1A - First, you need to import the data you want to plot!  To do
## this you need to make sure you are in the same working directory as
## your data.

getwd() # shows you what directory you are currently in

## Next, you should change the working directory to where the data is kept.
## Define a variable "path" that gives the file path to the directory 
## where the data are stored.

path <- ??? # Replace the question marks with a character string telling R
						# where to look for the data

## and you can replace it with where you have saved the data set.
setwd(path)

######################################################################
## 2A - Loading and exploring the data.  

bots.dat <- read.csv('HIV_Botswana.csv')
head(bots.dat, 5)

## Let's plot a pie chart of the HIV prevalence in 1994. 

pie(c(bots.dat$prevHIV[bots.dat$year == 1994], # Proportion HIV+
      1 - bots.dat$prevHI[bots.dat$year == 1994]), # Proportion HIV-
    labels = c("HIV +", "HIV -"),
    col = c("red","blue"),
    main = "HIV prevalence in Botswana, 1994")

## Change the above code to plot the prevalence of HIV in 2001.



## While pie charts are good for showing % breakdown between
## categories, bar plots can do the same with another variable
## added.

barplot(bots.dat$prevHIV,
        col = "red")

## This is a good start but we should ALWAYS label axes and have a
## title.

barplot(bots.dat$prevHIV * 100,         # *100 yields % 
        names.arg = bots.dat$year,      # labels bars by year
        col = "red",
        xlab = "year",
        ylab = "% HIV+",
        main = "HIV Prevalence in Botswana, 1990-2007")

## This is much better, but it doesn't really show the HIV- population
## like the pie chart did.  If we want this we can make what's called
## a "stacked bar plot".  To do this we need a matrix with two rows,
## the first that specifies the proportion HIV+ and the second the
## proportion HIV-. We'll have to bind two rows using rbind():

?rbind

prev.frame <- 100*rbind(bots.dat$prevHIV,1 - bots.dat$prevHIV)
prev.frame
barplot(prev.frame,
        names.arg = bots.dat$year,       # labels of the bars
        xlab = "year",
        ylab = "% of population",
        main = "HIV Prevalence in Botswana, 1990-2007",
        col = c("red","blue"),
        beside = FALSE,                 # Stacks bars
        space = 0,                    # TRY 1, .2, 0
        border = F)                   # TRY TRUE & FALSE

## We still need to show our audience that red means HIV+ and blue
## means HIV- so they don't think its the other way around. To do this
## we can add a legend.
?legend

legend("top",                      # Location of the legend
       c("HIV-", "HIV+"),
       col = c("blue", "red"),
       pch = ???,                     # TRY 15 or 19 or other integers
       bg = ???,                      # TRY various colors
       cex = ???)                     # TRY .5, 1, 2       

## Alternatively we can just plot prevalence as a series of points.
plot(bots.dat$year,
     bots.dat$prevHIV *100,
     xlab = "year",
     ylab = "% of population",
     main = "HIV Prevalence in Botswana, 1990-2007",
     ylim = c(0,30))                    # Sets y axis limits.

######################################################################
## Section 2: Plotting incidence.
######################################################################

## We'll be plotting measles data in this example. The data was
## provided by Benjamin Bolker and is available
## online from the International Infectious Disease Data Archive
## (IIDDA) at http://iidda.mcmaster.ca.


measles.Lon <- read.csv("measlesCleanLon.csv")

######################################################################
## 2A - Now let's explore the data we've imported.
head(measles.Lon)                       # Shows first 5 rows
tail(measles.Lon, 20)                   # Shows last 20 rows

names(measles.Lon)                     # Shows variable names

dim(measles.Lon)                       # (rows, columns) of data
nrow(measles.Lon)                      # rows of data
ncol(measles.Lon)                      # columns of data

######################################################################
## 2B - Now we should check to see what types (classes) of variables
## are in this data set.  Let's ask R what class it thinks our
## variables are:

class(measles.Lon$cases)
## Great! It recognized that the number of cases is an integer. Let's
## check the date variable.

class(measles.Lon$date)
## Uh oh, it thinks that date is a factor.  Factors are categorical
## variables, but time is continuous.  To deal with this, you will
## want to convert the dates to a format that R understands as a date.
## One way to do this is using the build-in as.Date() function.

measles.Lon$date <- as.Date(as.character(measles.Lon$date))
head(measles.Lon)
class(measles.Lon$date)
range(measles.Lon$date)

## Now R turned the date into strings of characters, but recognizes
## them as real dates which is why it gave the correct range.

######################################################################
## 2C - Now we're ready to make our first plot.

plot(measles.Lon$date,                  # X variable
     measles.Lon$cases)                 # Y variable

## Not bad, right?  But those circles are a little bit big and make
## the pattern more difficult to follow.  Let's try some other
## options.
plot(measles.Lon$date,
		 measles.Lon$cases,
		 type = ii,                      # TRY "p","l","b","s","h"
		 xlab = "Time",                     # x axis label
		 ylab = "# cases (weekly)",         # y axis label
		 main = "London Measles Incidence, 1944-1994" # plot title
)
## Much better!

######################################################################
## 2D - What other information can we add to make this
## plot more insightful?  Well, measles vaccine was introduced
## publically in 1968. Let's see if this helps us understand
## the trends better.

## First let's define the year in which vaccination began in R's time
## format:n
vaccine.year <- as.Date("1968-01-01")

arrows(vaccine.year, 5000,             # 1st (x,y) coordinate of arrow
       vaccine.year, 4500,              # 4500 and 2000
       length = .1,                   # TRY .1 and 1
       lwd = 2,                      # TRY 2 and 4
       lty = 1,                      # TRY 1 and 2
       col = "red")

## Great! But now we should label this arrow, so that readers know
## what it means.

text(vaccine.year, 5000,                # (x,y) coordinate of text
     "beginning of vaccination",        # text to plot
     pos = 2,                         # TRY 1,2,3 and 4
     col = "red")
######################################################################
## PROBLEM 1A
######################################################################
## The MMR vaccine (measles, mumps, rubella) vaccine was
## introduced to London in 1988 and administered more widely than the
## measles vaccine itself.  WRITE CODE to add a blue, labeled arrow
## that shows when this occurred.
######################################################################

######################################################################
## PROBLEM 1B
######################################################################
## Let's compare the measles data from London with that from
## Liverpool. We'll add the Liverpool data to the plot with the
## lines() function. You should modify the below code to use different
## colors, or line types and a legend to distinguish between the two
## incidence data sets. Choose your graphical parameters so you can
## see both the data sets as clearly as possible.

## NOTE: Many of the graphical parameters you might be interested in
## changing are described in:

?par

measles.LP <- read.csv("measlesCleanLP.csv")
measles.LP$date <- as.Date(measles.LP$date)

## lines() & points() add more lines/points to an already open plot

?lines()

######################################################################
######################################################################



######################################################################
## 2E - Does it look like there is seasonality in measles incidence?
## Let's see if there might be. To do this we're going to sum up all
## the cases for each month of the year and then plot by month using a
## boxplot.

month.char <- format(measles.Lon$date,
                     format = ???)    # TRY "%B", "%m", "%b" and
                                        # "%b-%Y".  Pick the value
                                        # that gives you months as
                                        # three letters.
head(month.char,50)
## Now we should really tell R that month is a factor with levels
## equal to month.abb, a default vector in R that gives:
print(month.abb)

month.char <- factor(month.char,levels=month.abb)
head(month.char,40)

## We're going to try to look at measles incidence seasonality with
## several different plots. Often its useful to have multiple panels
## in a plot window if you want to see many plots side by side:

par(mfrow = c(1,2))                     # This says that the next two
                                        # plots will be plotted in a
                                        # window with 1 row of
                                        # panels and 2 columns.
## some other common graphics tweaks you may find handy
par('ps'=17,  ## set font size to 18
    bty = 'n', ## turn off box around plot
    lwd = 2, mar = c(6,7,6,.5),
las = 2) ## set all line widths to twice as big

## First lets do a simple scatterplot.
plot(as.numeric(month.char),
     measles.Lon$cases,
     xaxt = "n",                  # Tells R not to plot an x-axis so
                                  # we can do it manually
     bty = "n",                   # Doesn't plot a box around the plot
     xlab = "",                   
     ylab = ???,                  # WHAT IS AN APPROPRIATE LABEL?
     main = "Weekly Measles Incidence\n in London by Month",
     pch = 16,                         # TRY 5, 19, 20, 21
     cex = 1)                         # TRY 3, 1, .4

## In the above example, we did not plot an x-axis so that we could do
## it manually. We'll label x axis with the vector:


axis(1,
     at = 1:12,
     labels = month.abb)

## This gives us some idea about the distribution of weekly incidence
## in months, but there are so many points on top of each other we
## can't really see the mean.  Let's use tapply() to get mean weekly
## incidence by month.

?tapply                                 # WHAT DOES tapply() do?

mean.by.months <- tapply(measles.Lon$cases, month.char, mean)

print(mean.by.months)

## Now let's add a line to the plot using lines(), which takes x & y
## inputs like plot() but adds data to a plot that's already open.

lines(1:12,
      mean.by.months)

## WOW! The means are quite low on this plot.  This means there are a
## lot of points clustered at the bottom of each month. Let's plot a
## boxplot. Boxplots are good for showing the distribution of samples
## by the level of another variable. First, read about the function.

?boxplot

boxplot(measles.Lon$cases ~ month.char,
        range = 1,                    # TRY 1 and 3
        ylab ='',                     # ADD AN APPROPORIATE LABEL
        bty = "n",
        main ="Seasonality in \nmeasles incidence")

## Both these curves focus closely on the distribution of weekly
## incidence but don't show trends in the mean very well.  This is
## because the y axis is scaled so large to include really big
## values. If we make a barplot of just the monthly means we can focus
## more on them.

barplot(mean.by.months,
        names.arg = names(mean.by.months),
        ylab = "mean weekly incidence",
        main = "Mean Weekly Incidence Aggregated \nby Month in London, 1944-1994")

## Say you have decided that there is not enough space between the
## three plots.  If you'd like to change the margins you can use the
## following code:

par(mfrow=c(1,3), mar=c(4,4,4,6))       # Each of the elements of the
                                        # "mar" vector specify the
                                        # margins for the bottom,
                                        # left, top & right of the
                                        # plot, respectively. Change
                                        # the third number to a 2 and
                                        # see what happens.

## some other common graphics tweaks you may find handy
par('ps'=18,  ## set font size to 18
    bty = 'n', ## turn off box around plot
    lwd = 2) ## set all line widths to twice as big
    
    

######################################################################
## PROBLEM 2
######################################################################
## Compare the seasonality of London and Liverpool's measles incidence
## in a single plot where the mean weekly incidence of measles is
## displayed with a line for each city.  Remember to use a legend!.
######################################################################


######################################################################
## SECTION 3 - Histograms
######################################################################

## In this section you'll learn how to plot and manipulate
## histograms. The data we'll play with is the number of hookworm eggs
## per gram of feces counted from a large population of people and
## whether or not they wear shoes.

par(mfrow=c(1,1),                       # Let's reset our paneling and
    mar=c(4,4,4,4))                     # margin parameters back to
                                        # their original values.

hookworm <- read.csv("hookworms.csv")

head(hookworm)
nrow(hookworm)

## Check out some of the parameters of the histogram.

?hist

hist(hookworm$epg)

## That's rather ugly and doesn't show us much about the data.  This
## is because the bins are extremeley big so we don't see much
## variability.  If we make the bins smaller we'll see more
## information. The way to do this is to set the breaks between
## bins. To divide the data into "pretty" bins we can use the pretty
## function.

?pretty

my.breaks <- pretty(range(hookworm$epg),100)

hist(hookworm$epg,
     breaks = my.breaks,
     xlab = "eggs per gram",
     main = "Egg Per Gram Distribution")

## This looks much better, but we still cannot see the data clearly
## because most of the EPG values were < 10,000 and the xlimit goes
## out to 40,000 because the distribution is very skewed.  To get a
## better look at the majority of the data we can restrict ourselves
## to a smaller x axis limit.  Let's add even more breaks to improve
## our resolution.

my.breaks <- pretty(range(hookworm$epg),1000)

hist(hookworm$epg,
     breaks = my.breaks,
     xlim = c(0, 10000),
     xlab = "eggs per gram",
     main = "Egg Per Gram Distribution")

## Notice how many of the EPG counts were near 0! We couldn't see this
## when the bins were much larger. If we make the bins even smaller
## what happens?

######################################################################
## PROBLEM 3
######################################################################
## Create breaks that are spaced at intervals of 10, and 1 on two
## panels side by side and compare how the data is presented.
######################################################################

######################################################################
## PROBLEM 4
######################################################################
## Create two histograms side by side on panels that show the
## distribution of EPG for people who do and do not where shoes.  Be
## very careful with your axes limits! If two plots have different
## scaled axes then visual comparisons may lead to incorrect
## impressions.
###################################################################### 


######################################################################
## PROBLEM 5
######################################################################
## The function pdf() can be used to create pdf files of your
## graphics.  Similarly, the function jpeg() can be used to create
## *.jpg graphics files. Use pdf() to create one pdf file with the
## plots from PROBLEMS 1,2,3 & 4.

## To initialize a pdf you use the line:

## pdf("myfilename.pdf")

## Then any graphics you create in R afterwards go into that pdf file
## until you turn off the graphics device by running the line of code:

## dev.off()

## By default R will add each plot window as a different page to your
## pdf file.  You can fiddle with this option by trying
## pdf("myfilename.pdf", onefile = FALSE)

###################################################################### 

