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

path <- "~/Dropbox/Mentorship/MMED/visualizingData" # Replace the question marks with a character string telling R
# where to look for the data

## and you can replace it with where you have saved the data set.
setwd(path)

######################################################################
## 2A - Loading and exploring the data.  

bots.dat <- read.csv('HIV_Botswana.csv')
head(bots.dat, 5)

## For this exercise, we will be using ggplot package so it is best we export it into our R
## environment
library(ggplot2)

## We can first plot a pie chart to understand the proportion of HIV positive and negative
## people in a specific year.
## First, we would need to transpose the data to long format as ggplot prefers data that way
## We will create a column called prevHIVneg which will have the proportion who are negative

bots.dat$prevHIVneg<- 1-bots.dat$prevHIV ## create a new column to calculate the proportion HIV -
## we transpose the data to long format using a function called melt which is in the reshape2 package
library (reshape2)
bots.dat1<-melt(bots.dat, id=1)

## We now use ggplot to make a visualise the proportion of HIV positive and negaive people in 1994
ggplot(bots.dat1[bots.dat1$year == 1994,], aes(x="", y=value, fill=variable))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void()+ # remove background, grid, numeric labels
  scale_fill_manual(values=c("blue", "red"),labels=c("HIV-", "HIV+"))+
  labs(fill="Group") # rename the legend title

## Change the above code to plot the prevalence of HIV in 2001.



## While pie charts are good for showing % breakdown between
## categories, bar plots can do the same with another variable
## added.

?geom_col #to understand more about this function

ggplot(bots.dat, aes(y=prevHIV, x=year))+
  geom_col(fill="red")


## This is a good start but we should ALWAYS label axes and have a
## title.

ggplot(bots.dat, aes(y=prevHIV, x=year))+
  geom_col(fill="red")+ 
  theme_bw()+
  labs(x="Year", y="% HIV+",   # label the x and y axis
       title="HIV Prevalence in Botswana, 1990-2007") # give the graph a title

## This is much better, but it doesn't really show the HIV- population
## like the pie chart did.  If we want this we can make what's called
## a "stacked bar plot".  To do this we can use the transposed dataset,
## bots.dat1 we earlier created which specifies the proportion that is HIV+ and
## HIV-. 



head(bots.dat1)
bots.dat1$variable<- 
  bots.dat1$value<- bots.dat1$value*100 ## change the proportion to percentage

ggplot(bots.dat1, aes(x=year, y=value, fill=variable))+geom_col()+
  theme_bw()+ ## change this to theme_void(), theme_classic() to see the differences
  scale_fill_manual(values=c("blue", "red"), labels=c("HIV-", "HIV+"))+
  labs(x="Year", y="% of population", title="HIV Prevalence in Botswana, 1990-2007", fill="")


## Alternatively we can just plot prevalence as a series of points.

ggplot(bots.dat, aes(x=year, y=prevHIV*100))+geom_point()+theme_bw()+
  labs(x="Year", y="% of population", title="IV Prevalence in Botswana, 1990-2007")

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

ggplot(measles.Lon, aes(x=date, y=cases))+
  geom_point()+
  theme_bw()
## Not bad, right?  But those circles are a little bit big and make
## the pattern more difficult to follow.  Let's try some other
## options.
ggplot(measles.Lon, aes(x=date, y=cases))+
  geom_xx()+ ##TRY geom_line(), geom_point(), geom_line()+geom_point()
  theme_bw()+
  labs(x="Time", y="# cases (weekly)", title="London Measles Incidence, 1944-1994")
## Much better!

######################################################################
## 2D - What other information can we add to make this
## plot more insightful?  Well, measles vaccine was introduced
## publicly in 1968. Let's see if this helps us understand
## the trends better.

## First let's define the year in which vaccination began in R's time
## format:n
vaccine.year <- as.Date("1968-01-01")

# Now let us include this in the plot

ggplot(measles.Lon, aes(x=date, y=cases))+
  geom_line()+ 
  theme_bw()+
  labs(x="Time", y="# cases (weekly)", title="London Measles Incidence, 1944-1994")+
  annotate("segment",x=vaccine.year,
           xend=vaccine.year, y=5000, # try change the value of y and visualise the plot
           yend=4500, color="blue",
           arrow=arrow(length=unit(0.05,"npc") # define the size of the arrow
           ))
## Great! But now we should label this arrow, so that readers know
## what it means.

ggplot(measles.Lon, aes(x=date, y=cases))+
  geom_line()+ 
  theme_bw()+
  labs(x="Time", y="# cases (weekly)", title="London Measles Incidence, 1944-1994")+
  annotate("segment",x=vaccine.year, 
           xend=vaccine.year, y=5000,
           yend=4500, color="blue",
           arrow=arrow(length=unit(0.05,"npc") # define the size of the arrow
           ))+
  annotate( "text",  x=vaccine.year, y=5000, # x and y coordinates of the text
            label="beginning of vaccination",
            color="red"
  )


######################################################################
## PROBLEM 1A
######################################################################
## The MMR vaccine (measles, mumps, rubella) vaccine was
## introduced to London in 1988 and administered more widely than the
## measles vaccine itself.  WRITE CODE to add a blue, labeled arrow
## that shows when this occurred.
######################################################################
mmr.vaccination<-as.Date("1988-01-01")
ggplot(data=measles.Lon, aes(x=date, y=cases))+
  geom_line()+ 
  theme_bw()+
  labs(x="Time", y="# cases (weekly)", title="London Measles Incidence, 1944-1994")+
  annotate("segment",x=mmr.vaccination,
           xend=mmr.vaccination, y=2000,
           yend=1500, color="blue",
           arrow=arrow(length=unit(0.05,"npc") # define the size of the arrow
           ))+
  annotate( "text",  x=mmr.vaccination,
            label="MMR vaccination",
            y=2100, color="red"
  )

######################################################################
## PROBLEM 1B
######################################################################
## Let's compare the measles data from London with that from
## Liverpool. We'll add the Liverpool data to the plot with the
## lines() function. You should modify the below code to use different
## colors, or line types and a legend to distinguish between the two
## incidence data sets. Choose your graphical parameters so you can
## see both the data sets as clearly as possible.

measles.LP <- read.csv("measlesCleanLP.csv")
measles.LP$date <- as.Date(measles.LP$date)

## We can try to first combine the two datasets, for Liverpool and London
measles.LP$country<- "Liverpool"
measles.Lon$country<-"London"

measles.data<-rbind(measles.LP, measles.Lon)


## We can now plot the data for both countries 

ggplot(measles.data, aes(x=date, y=cases, group=country, colour=country))+ # while plotting, group the data for each country
  geom_line()+
  theme_bw()+
  scale_color_manual( values = c("blue", "red"))+
  labs()# add a x, y axis label and a title for the graph


######################################################################
######################################################################



######################################################################
## 2E - Does it look like there is seasonality in measles incidence?
## Let's see if there might be. To do this we're going to sum up all
## the cases for each month of the year and then plot by month using a
## boxplot.

measles.Lon$month <- format(measles.Lon$date,
                     format = "%b")    # TRY "%B", "%m", "%b" and
# "%b-%Y".  Pick the value
# that gives you months as
# three letters.
head(measles.Lon$month,50)
## Now we should really tell R that month is a factor with levels
## equal to month.abb, a default vector in R that gives:
print(month.abb)

month.char <- factor(measles.Lon$month,levels=month.abb)
head(measles.Lon$month,40)

## We're going to try to look at measles incidence seasonality with
## several different plots. 

## First lets do a simple scatterplot.

ggplot(measles.Lon, aes(x=month, y=cases))+geom_point(shape=16)+ # TRY 1, 4, 5,8, 20
  theme_bw()+
  labs(x="Month", #y=??, # WHAT IS AN APPROPRIATE LABEL?
       title="Weekly Measles Incidence\n in London by Month")
  

## This gives us some idea about the distribution of weekly incidence
## in months, but there are so many points on top of each other we
## can't really see the mean.  Let's use tapply() to get mean weekly
## incidence by month.

?tapply                                 # WHAT DOES tapply() do?

mean.by.months <- tapply(measles.Lon$cases, measles.Lon$month, mean)
class(mean.by.months)
mean.by.months<-data.frame(mean.by.months) ## convert the array into a dataframe
mean.by.months$month<- rownames(mean.by.months) ## add a column for months

mean.by.months$month<-factor(mean.by.months$month,levels=month.abb) # arrange them according to the 
#order of the months in the calendar
print(mean.by.months)


## Now let's add a line to the plot using lines(), which takes x & y
## inputs like plot() but adds data to a plot that's already open.

plot1<-ggplot(measles.Lon, aes(x=month, y=cases))+geom_point(shape=16)+ # TRY 1, 4, 5,8, 20
  theme_bw()+
  geom_line(data=mean.by.months, aes(x=month,y=mean.by.months, group=1), colour="red")+
  labs(x="Month", #y=??, # WHAT IS AN APPROPRIATE LABEL?
       title="Weekly Measles Incidence\n in London by Month")
plot1

## WOW! The means are quite low on this plot.  This means there are a
## lot of points clustered at the bottom of each month. Let's plot a
## boxplot. Boxplots are good for showing the distribution of samples
## by the level of another variable. First, read about the function.

?geom_boxplot

plot2<-ggplot(measles.Lon, aes(x=month, y=cases))+geom_point(shape=16)+
  geom_boxplot()+
  labs(x="", y="", main="Seasonality in \nmeasles incidence")+## add appropriate x and y axis labels
  theme_bw()
plot2
## Both these curves focus closely on the distribution of weekly
## incidence but don't show trends in the mean very well.  This is
## because the y axis is scaled so large to include really big
## values. If we make a barplot of just the monthly means we can focus
## more on them.
plot3<-ggplot(mean.by.months, aes(x=month,y=mean.by.months))+geom_col()+
  labs(y="mean weekly incidence", 
       title="Mean Weekly Incidence Aggregated \nby Month in London, 1944-1994")
plot3
library(gridExtra)


## To observe all three plots in one window,
## we will use a function called grid.arrange()  from the
## gridExtra package that allows one this functionality.

grid.arrange(plot1, plot2, plot3)



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


hookworm <- read.csv("hookworms.csv")

head(hookworm)
nrow(hookworm)

## Check out some of the parameters of the histogram.

?geom_histogram
ggplot(hookworm, aes(epg))+geom_histogram()


## That's rather ugly and doesn't show us much about the data.  This
## is because the bins are extremeley big so we don't see much
## variability.  If we make the bins smaller we'll see more
## information. The way to do this is to set the breaks between
## bins. To divide the data into "pretty" bins we can use the pretty
## function.

?pretty

my.breaks <- pretty(range(hookworm$epg),100)

ggplot(hookworm, aes(epg))+stat_bin( breaks=my.breaks)+
  theme_bw()+
  labs(x="eggs per gram", title="Egg Per Gram Distribution")



## This looks much better, but we still cannot see the data clearly
## because most of the EPG values were < 10,000 and the xlimit goes
## out to 40,000 because the distribution is very skewed.  To get a
## better look at the majority of the data we can restrict ourselves
## to a smaller x axis limit.  Let's add even more breaks to improve
## our resolution.

my.breaks <- pretty(range(hookworm$epg),1000)

ggplot(hookworm, aes(epg))+stat_bin( breaks=my.breaks)+
  theme_bw()+xlim(0, 10000)+ # set the x axis limit
  labs(x="eggs per gram", title="Egg Per Gram Distribution")


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
## distribution of EPG for people who do and do not wear shoes.  Be
## very careful with your axes limits! If two plots have different
## scaled axes then visual comparisons may lead to incorrect
## impressions.
###################################################################### 


######################################################################
## PROBLEM 5
######################################################################
## The function ggsave() can be used to create pdf files or *.jpg files 
#of your graphics.  
## To save all these graphics in one graph, use the grid.arrange function:




###################################################################### 
