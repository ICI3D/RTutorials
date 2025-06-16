## Visualizing infectious disease data in R tidyverse
## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## African Institute for Mathematical Sciences, Muizenberg, RSA

## Some Rights Reserved
## CC BY-NC 4.0 (https://creativecommons.org/licenses/by-nc/4.0/)
## Created by Mutono Nyamai and Jonathan Dushoff June 2021
## Based on the earlier Visualizing infectious disease data in R by Steve Bellan
## Updated by Evans Omondi in 2023

######################################################################
## Section 1: Plotting prevalence
######################################################################

######################################################################
## 1A - First, you need to import the data you want to plot!  To do
## this you need to make sure you are in the same working directory as
## your data.

getwd() # shows you what directory you are currently in ## CONSOLE
## NOTE: We use CONSOLE to mark commands that should not normally be part of a saved script, either because they are interactive exploration or because they will be different for different setups (like the next one)
## You should probably get in the habit of typing these things in your console window instead of your script window (not necessarily while doing tutorials ☺).

## Next, you should change the working directory to where the data is kept.
## Define a variable "path" that gives the file path to the directory
## where the data are stored.

path <- ?? # FIXME Replace the question marks with a character string telling R
  # where to look for the data
  ## NOTE: We use FIXME to tag things that won't work unless you fix them
  
  ## and you can replace it with where you have saved the data set.
  setwd(path) ## CONSOLE

######################################################################
## 2A - Loading and exploring the data.
## We will use the tidyverse packages. 
## You can read more about them here:https://www.tidyverse.org

library(tidyverse)
botswana.data <- read_csv('HIV_Botswana.csv')
head(botswana.data, 5)

## We can first plot a pie chart to understand the proportion of HIV positive and negative
## people in a specific year.
## First, we would need to transpose the data to long format as ggplot prefers data that way

## We will create a column called prevHIVneg which will have the proportion who are negative
## "mutate" is a way to manipulate data frames with readable code
## Note the use of the "pipe" operator |> to pass information from one step to the next
botswana.data<- botswana.data|>
  mutate(prevHIVneg= 1-prevHIV)

## we transpose the data to long format using a function called pivot_longer 
?pivot_longer # run this to read more about this function ## CONSOLE

botswana.long<-botswana.data|>
  pivot_longer(cols=prevHIV:prevHIVneg, names_to="type", values_to="prevalence")

## ggplot is part of the tidyverse
## It is a way of specifying graphs using a "grammar" rather than specifying everything that you want on the graph
## We use ggplot to visualise the proportion of HIV positive 
## and negative people in 1994
ggplot(botswana.long[botswana.long$year == 1994,], aes(x="", y=prevalence, fill=type)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() + # remove background, grid, numeric labels
  scale_fill_manual(values=c("blue", "red"),labels=c("HIV-", "HIV+")) +
  labs(fill="Group") # rename the legend title

## Now let's add some labels to show the prevalence percentages:

ggplot(botswana.long[botswana.long$year == 1994,], aes(x="", y=prevalence, fill=type))+
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) +
  theme_void()+ # remove background, grid, numeric labels
  geom_text(aes(label = paste0(prevalence*100, "%")), position = position_stack(vjust = 0.5), color = 'white')+
  scale_fill_manual(values=c("blue", "red"),labels=c("HIV-", "HIV+"))+
  labs(fill="Group") # rename the legend title

## TRY Change the above code to plot the prevalence of HIV in 2001.

## While pie charts are good for showing % breakdown between
## categories, bar plots can do the same with another variable
## added.

?geom_col #to understand more about this function ## CONSOLE

ggplot(botswana.data, aes(y=prevHIV, x=year))+
  geom_col(fill="red")

## This is a good start but we should ALWAYS label axes and have a
## title.

ggplot(botswana.data, aes(y=prevHIV, x=year))+
  geom_col(fill="red")+
  theme_bw()+ ##remove this or change this to theme_void(), theme_classic() to see the differences
  labs(x="Year", y="% HIV+",   # label the x and y axis
       title="HIV Prevalence in Botswana, 1990-2007") # give the graph a title

## This is much better, but it doesn't really show the HIV- population
## like the pie chart did.  It might be cleaner to show this information just as a
## series of points.

ggplot(botswana.data, aes(x=year, y=prevHIV*100))+
  geom_point()+
  theme_bw()+ ## TRY this: theme_void(), theme_classic() to see the differences
  labs(x="Year", y="% of population", title="HIV Prevalence in Botswana, 1990-2007")

######################################################################
## Section 2: Plotting incidence.
######################################################################

## We'll be plotting measles data in this example. The data was
## provided by Benjamin Bolker and is available
## online from the International Infectious Disease Data Archive
## (IIDDA) at http://iidda.mcmaster.ca.

measles.London <- read_csv("MeaslesCleanLon.csv")

######################################################################
## 2A - Now let's explore the data we've imported.

head(measles.London)                       # Shows first 5 rows ## CONSOLE
tail(measles.London, 20)                   # Shows last 20 rows ## CONSOLE
names(measles.London)                     # Shows variable names ## CONSOLE
dim(measles.London)                       # (rows, columns) of data ## CONSOLE
nrow(measles.London)                      # rows of data ## CONSOLE
ncol(measles.London)                      # columns of data ## CONSOLE

## You can also view in the Rstudio data panel or by using View()

######################################################################
## 2B - Now we should check to see what types (classes) of variables
## are in this data set.  Let's ask R what class it thinks our
## variables are:

class(measles.London$cases)
## Great! It recognized that the number of cases is an integer. Let's
## check the date variable.

class(measles.London$date)
## Uh oh, it thinks that date is a factor.  Factors are categorical
## variables, but time is continuous.  To deal with this, you will
## want to convert the dates to a format that R understands as a date.
## One way to do this is using the build-in as.Date() function.

measles.London<- measles.London|>
  mutate(date = as.Date(date))
head(measles.London) ## CONSOLE
class(measles.London$date) ## CONSOLE
range(measles.London$date) ## CONSOLE

## Now R turned the date into strings of characters, but recognizes
## them as real dates which is why it gave the correct range.

######################################################################
## 2C - Now we're ready to make our first plot.

ggplot(measles.London, aes(x=date, y=cases))+
  geom_point()+
  theme_bw()
## Not bad, right?  But those circles are a little bit big and make
## the pattern more difficult to follow.  Let's try some other
## options.
ggplot(measles.London, aes(x=date, y=cases))+
  geom_xx()+ ## FIXME TRY geom_line(), geom_point(), geom_line()+geom_point()
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

ggplot(measles.London, aes(x=date, y=cases))+
  geom_line()+
  theme_bw()+
  labs(x="Time", y="# cases (weekly)", title="London Measles Incidence, 1944-1994")+
  annotate("segment",x=vaccine.year,
           xend=vaccine.year, y=5000, # TRY: change the value of y
           yend=4500, color="blue",
           arrow=arrow(length=unit(0.05,"npc") # define the size of the arrow
           ))
## Great! But now we should label this arrow, so that readers know
## what it means.

ggplot(measles.London, aes(x=date, y=cases))+
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
ggplot(data=measles.London, aes(x=date, y=cases))+
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
  )+
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
## PROBLEM 1B
######################################################################
## Let's compare the measles data from London with that from
## Liverpool. We'll add the Liverpool data to the R environment and
## combine the two datasets using rbind(). We will then plot the
## datasets for the two countries in one plot and look at the
## cases over measles over time.

measles.Liverpool <- read.csv("MeaslesCleanLP.csv")
measles.Liverpool$date <- as.Date(measles.Liverpool$date)

## We can try to first combine the two datasets, for Liverpool and London
measles.Liverpool$country<- "Liverpool"
measles.London$country<-"London"

measles.data<-rbind(measles.Liverpool, measles.London)

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

measles.London<- measles.London|>
  mutate(month =format(date, format = "%b"))    # TRY "%B", "%m", "%b" and# "%b-%Y".  
#Pick the value
# that gives you months as
# three letters.
head(measles.London)
## Now we should really tell R that month is a factor with levels
## equal to month.abb, a default vector in R that gives:
print(month.abb)

measles.London<- measles.London|>
  mutate(month=factor(month, levels = month.abb))

head(measles.London)

## We're going to try to look at measles incidence seasonality with
## several different plots.

## First lets do a simple scatterplot.

ggplot(measles.London, aes(x=month, y=cases, ))+geom_point(shape=16)+ # TRY 1, 4, 5,8, 20
  theme_bw()+
  labs(x="Month", #y=??, # WHAT IS AN APPROPRIATE LABEL?
       title="Weekly Measles Incidence\n in London by Month")

## This gives us some idea about the distribution of weekly incidence
## in months, but there are so many points on top of each other we
## can't really see the mean.  Let's use summarise() to get mean weekly
## incidence by month.

measles.London.mean<- measles.London|>
  group_by(month)|>
  summarise(mean_cases=mean(cases))|>
  mutate(month=factor(month, levels=month.abb)) ## arrange the months in the order 
#they appear in the calendar

head(measles.London.mean)

## Now let us visualise the data

plot1<-ggplot(measles.London, aes(x=month, y=cases))+geom_point(shape=16)+ # TRY 1, 4, 5,8, 20
  theme_bw()+
  geom_line(data=measles.London.mean, aes(x=month,y=mean_cases, group=1), colour="red")+
  labs(x="Month", #y=??, # WHAT IS AN APPROPRIATE LABEL?
       title="Weekly Measles Incidence\n in London by Month")
plot1

## WOW! The means are quite low on this plot.  This means there are a
## lot of points clustered at the bottom of each month. Let's plot a
## boxplot. Boxplots are good for showing the distribution of samples
## by the level of another variable. First, read about the function.

?geom_boxplot ## CONSOLE

plot2<-ggplot(measles.London, aes(x=month, y=cases))+geom_point(shape=16)+
  geom_boxplot()+
  labs(x="", y="",title="Seasonality in \nmeasles incidence")+## add appropriate x and y axis labels
  theme_bw()
plot2
## Both these curves focus closely on the distribution of weekly
## incidence but don't show trends in the mean very well.  This is
## because the y axis is scaled so large to include really big
## values. If we make a barplot of just the monthly means we can focus
## more on them.
plot3<-ggplot(measles.London.mean, aes(x=month,y=mean_cases))+geom_col()+
  labs(y="mean weekly incidence",
       title="Mean Weekly Incidence Aggregated \nby Month in London, 1944-1994")+
  theme_bw()
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

hookworm <- read_csv("Hookworms.csv")

head(hookworm) ## CONSOLE
nrow(hookworm) ## CONSOLE

## Check out some of the parameters of the histogram.

?geom_histogram ## CONSOLE
ggplot(hookworm, aes(epg))+geom_histogram()

## That's rather ugly and doesn't show us much about the data.  This
## is because the bins are extremely big so we don't see much
## variability.  If we make the bins smaller we'll see more
## information. The way to do this is to set the breaks between
## bins. To divide the data into "pretty" bins we can use the pretty
## function.

?pretty ## CONSOLE

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
## Create breaks that are spaced at intervals of 10, and 1000 on two
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
##HINT: Look at facet_grid function to help you with this
######################################################################

?facet_grid ## CONSOLE

## BONUS: Try to put the number of eggs on a log scale (google ggplot log1p or ask us for help).

# What are the advantages and disadvantages of a log scale?
# Which axis or axes are appropriate for log scaling of these histograms?

######################################################################
## PROBLEM 5
######################################################################

## The function ggsave() can be used to create pdf files or *.jpg files
#of your graphics.

## Investigate ggsave and create some pdf files

######################################################################
  
