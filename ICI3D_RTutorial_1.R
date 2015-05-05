## Tutorial 1: Introduction to R and Its Quirks
## David M. Goehring 2004
## Juliet R.C. Pulliam 2008,2009
## Steve Bellan 2010

## Meaningful Modeling of Epidemiological Data, 2010
## AIMS, Muizenberg

###################################################################### 
## SECTION A. How to Use These Tutorials
###################################################################### 
## Ground Rules
#################### 
## Pay attention!
#################### 
## You should understand everything that you are typing in. Ask
## yourself, "Do I know what each thing I am typing means?" Getting
## through the exercises as fast as possible is not the goal here,
## true comprehension is. If there is anything you do not understand,
## ask!
#################### 
## Spare the copy and paste!
#################### 
## If you are like me, you hate busy-work. To that end, I hope you'll
## make use of the copy and paste functions to transfer text from this
## file to R. That said, especially early on, you will learn a LOT
## from the typos that inevitably occur when entering text into R. If
## you never allow yourself to make a typo as you go through the
## tutorial, you will be baffled when you are working on your projects
## and get seemingly random errors from, say, missing parentheses. So,
## for now, avoid copy and paste as much as possible -- it's in your
## best interest.
#################### 
## Wander off!
#################### 
## DON'T feel compelled to stay on track. You should aim to finish the
## tutorials within the specified lab period, but it's in your
## interest to say to yourself, "Hey, this is weird. I wonder what
## happens if I tweak this a bit:" That's how you'll really get the
## hang of things. So never be afraid to try anything and if your
## program doesn't behave as you expect it to, ask for an
## explanation. If you do not finish the tutorials during the lab
## period, you may work on the during the afternoon or evening work
## periods.
#################### 
## Make notes in this tutorial!
#################### 
## These tutorials should serve as a good reference, and the more you
## mark them up to make them clear to YOU the more handy they will
## be. Stumble upon a function you think is handy? Write it
## down. Think something I asked you to do is misleading? Correct it.
#################### 
## Don't get discouraged!
#################### 
## Learning any new language is always cumulative. Over the next
## couple of weeks, I'd recommend doing basic calculations in R rather
## than on a calculator, just so you grow more comfortable with the
## interface. Even though you might find yourself challenged, I expect
## you will be greatly rewarded by your accomplishments, even as soon
## as at the end of the first tutorial.
#################### 
## Procedure
#################### 
## BASICALLY, each tutorial is a series of guided exercises to help
## you develop an intuition for solving problems using R. At the end
## of each tutorial you will be asked to complete a collection of
## benchmark questions, which you should be able to answer with the
## tools learned in the tutorial. The questions are there for you to
## be able to test whether you learned the concepts covered. You can
## ask the instructors to confirm that your answers are correct.


## So, without further ado, let us proceed to R!

###################################################################### 
## SECTION B. Getting Familiar with R: More Than Just Pirate Talk
###################################################################### 

## By the end of this tutorial you should: 

##  Be familiar with the R interface 
##  Understand how R handles variables and assignments 
##  Know what R is finicky about 
##  Know how to use R functions 
##  Know how to use the help features of R 
##  Be introduced to data vectors 
##  Know how to access R's intrinsic datasets 
##  Know how to produce basic graphics in R 

## R as a calculator 

## Make sure R is up and running. R knows how to do just about any
## mathematical operation you can think of. So let's try out a few:
3 + 4 
5 * 5

## Note how R returns the answer for you.  The important thing to pay
## attention to here is the order of operations which R uses to process
## requests. This is roughly the same you learned in beginning
## algebra, so that

2 + 2 * 3 

## is not performed left to right. Predictably, parentheses are the
## way to tell R to prioritize certain operations over others, as in

(2 + 2) * 3 

## If you don't remember your order-of-operations lessons from long
## ago, no worries. R won't care if you go crazy with the parentheses,
## and you'll ensure that R is behaving the way you want it to.

(2 + (2 * (3))) 


####################
## Comments in R
#################### 

## Sometimes you will want to annotate your R code so it is easier for
## others (e.g., your instructors or collaborators) to understand. The
## number symbol (or pound symbol, or hash mark, or octothorpe), #, is
## used for this purpose.  You'll notice that these tutorials make
## extensive use of this tool.  But note that you can also add
## comments to the end of active code:

5 / 7 # This divides five by seven

## R ignores everything that comes after the symbol on the line. I
## will use this in the tutorials to indicate when you should expect
## an error or problem with a line of code, as in

(5*+ 2) / 7 #BAD 

## This way you will be able to tell when I am asking you to do
## something stupid.

## Making things stick 

## So far, everything you have done has been transient; it has been
## visible on your screen but you have no way of bringing those
## results back when you want to re-use them. Rarely will you want
## to use R in this fashion. You will want to perform a lot of
## steps on your data and store all the results of all the
## steps. To do this, you need to assign values to variables in the
## R system. The two-character pictographic arrow, <-, is the
## assignment operator in R. Try

x <- 3 

## storing the value 3 in the variable x (without any output). The
## value is then retrievable simply by entering

x 

## Putting this concept together with R's utility as a calculator, you
## can do assignment and calculation on the same line, as in

y <- 5 - 2 

## And variables and numbers can be used in calculations together, to
## make new variables, as in

z <- y + 2 * x 

## Remember you can check what the value of a variable is at any time,
## just by "calculating" it by itself, i.e.,

z 

## Note that variable names can be words and they can be as long as
## you want. Also note that the assignment operator can be used in the
## other direction, ->.

## While x, y, and z are easy to type, more memorable names such as
## weight or total, might be more useful for reminding you what values
## they hold.


## You may have noticed that assignment occurs last in the order of
## operations, which means R won't get confused if you assign a
## variable to itself, as in

z * z -> z 

## Hands off the keyboard! Pick up a writing implement:
## x <- 8
## x -> y
## 2 -> x
## What do x and y equal?

## Now execute the commands by removing the "##" at the beginning of
## the lines and compare your answers.


## R is case-sensitive, so the variables x and X are
## unrelated. Variable names can contain only letters, numbers, and
## the period.

## Furthermore, R uses a number of likely variable-name candidates as
## functions, making them confusing choices as variables. The worst of
## these are c, q, t, C, D, F, I, and T. Avoid them.

## You may encounter a synonymous version of the assignment operator
## <- in some code, =. Though it is one character shorter, it can be
## confusing as to in which direction the assignment is being made and
## is often confused with the logical-test-for-equality operator, ==,
## which we will come to later. While I recommend not using the =
## assignment operator, you should be aware of it.


#################### 
## Functions are your friends
#################### 
## While basic operations, such as addition, are invaluable features
## of R, the real core of R comes in the form of functions. Functions,
## in general, take arguments and convert them into output. That
## output can be a detailed statistical test, a chart or graph, or a
## simple summation -- the important thing to realize is that
## fundamentally all functions in R obey similar rules. A function in
## R is anything that can be specified by a name with a set of
## parentheses after it -- containing the function's arguments.
## Arguments are the input a function needs to perform its task and
## produce the desired output.

## This will become slightly clearer when I discuss the help utility
## below, but arguments can either be required or they can take on
## default values if unspecified. Because it would be tricky to keep
## track of the order of 10 possible inputs, which all might be
## optional, R allows the naming of these arguments in the
## function. This makes R functions easy to write and read, after a
## bit of acclimation.

## Take the log() function. It has one required argument, the number for
## which you want the logarithm, obviously. But it also has one
## optional argument, base. This argument defaults to e if omitted.

## So, you can determine the natural log of something quite easily, as
## in,

log(10) 

## To get the log base-10 of 10, you can write either 

log(10, 10) 

log(10, base=10) 

## "So, why would I ever want to do the second option?" you ask. Here,
## it would not much matter. It would be slightly easier to understand
## what is going on to a reader of your code in the second case, but
## maybe not enough useful to justify the extra typing.

## But as we move to more complex functions, especially the graphic
## functions like plot, the number of potential arguments is huge. Do
## you want a sub-caption for the y-axis? Most likely not, but when
## you do need one it is very valuable argument to have waiting in the
## wings. In those functions, the first couple of arguments will be
## specified without a name and then the rest will look like the
## base=10 argument above. For example, almost every plot you produce
## will need a label on the x-axis, say, so you will find yourself
## typing xlab= a lot.

####################
## Repeating lines of code
#################### 
## Have you tried pressing the up-arrow? If your cursor is at the
## command prompt in the Console window, R will recall each of your
## previous commands as you repeatedly press the up-arrow. If you are 
## working from a script in R Studio, you can place the cursor on the line
## you want to run and click the "Run" button at the top of the window. On
## a Mac, you can press CMD-Enter, instead of "Run." To repeat multiple lines
## of code from the script, highlight the lines and press "Run" or CMD-Enter.

## Save yourself the time and energy of re-typing by re-using what you have
## already entered.  This can be especially helpful if you made a
## small typographical error in a long command.

#################### 
## Making things go away
#################### 

## Above, you were introduced to the assignment procedure in R. The
## more variables you have assigned or imported, the more cluttered
## your memory will become. You can view what variables are stored in
## R's memory at any time by typing

ls() 

## This may look weird, given what I just told you about function
## arguments above, but it just turns out that the ls() function has
## arguments, but they all take on default values when none are
## specified.

## If you are working in Rstudio, you can also see the variables and
## functions defined in your workspace by clicking on the "Workspace"
## tab, which will show not only the variable and function names but also
## the size of defined variables and the arguments for each function.

## If you've identified a specific variable or set of variables that
## you would like to remove from memory, you need only type

rm(z) 

## The variable "z" can be replaced by any number of variables,
## separated by commas.

## Alternatively, one often needs to clear the memory from R and start
## fresh -- especially because R will, by default, start up with the
## same variables in memory as when you last closed the program.

## The function here is a little bit convoluted, but instructional,

rm(list = ls()) 

## Here you are telling R, "I'm not going to give you individual
## variable names, instead I am giving you a list of variable
## names. To generate that list, perform the function that creates the
## list of all variables stored in memory, ls()."

#################### 
## Getting help
#################### 

## When you do not know what a function does or what arguments is
## takes, R's in-line help system is exceptionally handy. The easiest
## way to access help on a specific function, such as log, is

?log

## But equivalently, you can type

help(log)

## When the item you need help on is an operator, rather than a
## standard function (like the addition operator, +) only the second
## of these will work, and you must include the operator in quotes, as
## in

help("+") 


## Help files in R can seem intimidating. The trick is to know what
## you don't need to know. Often you will end up using only a few of
## the arguments. Most arguments will have standard, acceptable
## defaults noted by something like name=FALSE. You do not need to
## understand every argument in a function to get it to do what you
## want.

## The descriptions can be helpful, but you'll also want to look at
## the examples. The examples tend to be targeted at common scenarios
## and are most often well framed.

## Lastly, the "see also" section may be useful if you are looking for
## a function but only know the name of a similar function.


## Conversely, you might know what a function does, but not know R's
## name for it. There is no incredibly satisfactory solution to this,
## but a good first step is to search within the in-line help
## documentation for the concept:

help.search("average") 

## Or, equivalently,

??average

## The next step is to search the R website, http://www.r-project.org,
## for any help it can provide. The easiest way to do this from within
## R is to type

RSiteSearch("average") 

## With these tools in hand, you should be ready to troubleshoot most
## of your R foibles yourself!


## This tutorial will not have every function and method you need.
## You should rely firstly on the help facilities of R -- which are
## very good. Of course, the instructors are also happy to help with
## difficulties as they occur, particularly as you develop a need for
## new methods while working on your projects.

######################################################################  
## SECTION C. Introduction to Vectors
######################################################################

#################### 
## What is a vector?
####################

## Statisticians worry about multiple numbers, not single numbers like
## we have seen so far. So it is not surprising that most of the
## computation you will perform in R will involve more complex data
## structures.

## The simplest such structure is the vector, which is nothing more
## than a list of numbers, stored in the same place. All the rules you
## learned about assignment, above, still apply here, but there are
## some complications.

#################### 
## Inputting your own vector
#################### 
## In order to generate vectors of any length, R has a somewhat
## non-intuitive method, the function c(). The name "c" is short for
## concatenate, if that helps you remember its very important role in
## R. The function takes any number of arguments, which have values of
## numbers or vectors.  Some examples:

my.vector <- c(1,2,3)

my.other.vector <- c(x,y) 

my.last.vector <- c(my.vector, 365, my.other.vector) 

## It is pretty straightforward, once you get the hang of it.


## Hands off the keyboard! Pick up a writing implement:
## some.ages <- c(1,4,2)
## some.ages <- c(some.ages,3,some.ages+1)
## What does some.ages equal?

## Now decomment the above code by removing the "##" to execute the
## commands and compare your answers.

#################### 
## Generating random vectors
#################### 
## It is a common programming task to want to generate a list of
## random numbers. R has a number of different functions for this, but
## I will show you rnorm(), which generates random numbers from the
## standard normal distribution (more details about working with
## probability distributions are provided in Tutorial III).

## Today we will use just one basic argument for the rnorm() function,
## the number of random numbers we want in our vector. Say we want 110
## of these random numbers, then typing

my.randoms <- rnorm(110) 

## will generate these and store them in our designated variable.

#################### 
## Functions on vectors
#################### 
## Many functions take a vector as an argument, rather than (or in
## addition to) a single number. Now that you know how to generate
## vectors to specification, these can be very handy. Some examples:

median(my.last.vector) 

mean(my.randoms) 

sin(rnorm(10)) 

c(1,2,3)^3 

## Depending on the nature of the function, the result here is a
## single value or a transformed vector with the same length as the
## original. The possibilities of input data structures and output
## data structures are countless, but I hope you will realize the
## diversity of roles which functions can play in your analyses.

#################### 
## Basic plots
#################### 
## Producing a mediocre figure in R is trivial. With most data
## structures you will get something just by typing

plot(my.last.vector) 

## that is, the function plot, with the argument being whatever
## variable name you wish you visualize; however, this simplicity is
## usually unwanted and unhelpful, as in the case

plot(my.randoms) 

## Not surprisingly, random numbers do not have an interesting
## sequence.

## We can get closer to something useful by changing the arrangement
## of our data, for example,

plot(sort(my.randoms)) 

## which sorts our variable before plotting it, producing a better
## sense of the distribution of values.

## Or we can change the type of plot or characteristics of the plot,
## as in

hist(my.randoms) 

## to produce a histogram showing the distribution of the numbers we
## have drawn from the standard normal distribution.

## In these last few examples I have used arbitrary or random
## psuedodata, in lieu of real data, to help you understand the
## visualization of vectors.  Consequently, labeling axes in this
## extreme case is irrelevant.  You should generally label, with a
## title and axes, every figure with data. More on this below.

###################################################################### 
## SECTION D. Introduction to Obtaining and Visualizing Datasets
######################################################################

#################### 
## Where to find data
#################### 
## Importing data into R from your computer is fairly straightforward,
## but for now, we will use only the datasets contained in R default
## packages.  Generating a list of these available datasets is as
## simple as entering

data() 

## but here R stiffs you a little, by not looking in every available
## package. It warns you of this at the bottom of its response and
## suggests that you should try

data(package = .packages(all.available = TRUE)) 

## which gives you a complete list, importantly noting in its output
## which package each dataset comes from.

## Obtaining and understanding datasets Once you have found a dataset
## you wish to use (and we will use Chatterjee-Price Attitude Data,
## attitude, as an example), you load it into memory by entering

data(attitude) 

## For many of those datasets found by the second, more thorough,
## method above this will produce an error, because it is only looking
## in the default package. Get around this by adding an argument
## specifying the package of the dataset, here using catsM, a variant
## of which was seen in-class

data(catsM, package="boot") 

## After performing this step, the data are loaded into the same-named
## variable in R's memory, (usually, but not always) as a data frame,
## another type of data structure. More suited to statistical
## analysis, data frames are an alternative to standard data
## structures you may have experience in mathematical programming such
## as matrices or arrays. While they have incredible sophistication, I
## won't be able to introduce most of this here. We will be sticking
## to absolute basics; for a more in-depth treatment of data frames
## and other data structures in R, see Travis Porco's website:
## http://www.mathepi.com/comp/index.html.

## First, you will likely want to take a look at the data itself, just
## to see what it has in store. As you have learned, that can be done
## simply by typing the name of your new variable on a line by itself

attitude 

## The resulting chart (most often) will have named rows and columns
## which correspond to the datum for those conditions and property. In
## this example, individuals are numbered 1 to 30 and each one has
## corresponding numerical answers to different questions regarding
## their attitude.

## You can also just view the top of the dataset (which allows you
## to also view the column names) by typing

head(attitude)

## Another critical step when working with any built-in dataset is to
## look at the accompanying information on the dataset, accessed by
## typing

help(attitude)

## or, when necessary, by analogy to the package issues introduced
## above,

help(catsM, package="boot") 

## This text will include key information about the procedures and
## assumptions involved in data collection, in addition to a basic
## understanding of the data's components. The file's examples also
## provide starting points for visualization or analysis of the data.

## In addition to data frames, some R datasets include more
## specialized collections of data, such as time series or distance
## charts. You will know if you have such a dataset from the help-file
## associated with the dataset (or by its failure to perform
## predictably to data-frame commands below).  Feel free to use the
## help system to find ways to visualize these other collections of
## data! NOTE: In a few days we'll be doing a whole tutorial on
## visualizing infectious disease data.

## Figures are produced the same way as above, with a couple of new
## considerations.

## What analyses are appropriate and meaningful will be highly
## contingent on the structure of the data. Quick, dirty, and
## moderately interesting results are produced independently-by-column
## with simple commands on the data frame as a whole, for example,

summary(attitude) 

boxplot(attitude) 

## For data frames, the columns are often compared in various ways, so
## a special notation, the dollar sign, $, is reserved for accessing
## the individual columns. The syntax is simply
## data.frame$column.name. To see what this means to R we can type the
## notation for the first column in our attitude data frame, the
## rating column

attitude$rating 

## The result is simply a vector of the values, ordered identically to
## the original column.  This means you can invoke any function with
## which you have familiarity on vectors, as in,

hist(attitude$complaints) 

plot(sort(attitude$critical)) 

## Other functions will look at the relationship among columns, about
## which much can be said.

## The simplest example is to add a second vector to our plot
## function, which will place the values of two columns on each of the
## axes.

plot(attitude$critical, attitude$privileges) 

## Once you have found useful figures to output, you must ensure they
## are appropriately labeled. The arguments to most graphical
## functions include (among many more) xlab, ylab, and main. Here is
## an example of a finalized plot.

plot(catsM$Bwt,catsM$Hwt,
     xlab="Male Cat Body Weight (kg)",
     ylab="Male Cat Heart Weight (g)",
     main="Body Weight vs. Heart Weight for Male Cats over 2 Pounds")

## Always label your axes (and provide units, when relevant). R will
## sometimes generate labels by default, but you will need to ask
## yourself whether these labels are sufficiently informative (they
## usually are not). Figures should also generally have descriptive
## titles.

## Armed with the help files and this proto-introduction to plotting,
## you should begin to see how you can tell a story with a set of
## data.

## Note how I separated the plot command above into four lines
## that were properly indented.  Some coders find this much easier to
## read & debug though it depends on individual preference.  EMACS
## users will find that EMACS often formats this for you automatically
## if you press enter while still inside a function, or unclosed
## parantheses expression.

## One final new procedure: 
## The command library, used in the manner 

library("stats4") 

## allows you to access functions for specific tasks. The core R
## installation comes with a variety of functions installed but
## unavailable by default -- the library function makes the functions
## and data from those packages available. Working along with
## preinstalled packages and accessing their datasets will be easy
## with a single application of the library command in each R session.

## If a library function call fails on your machine, you need to
## install the library package. On most computers connected to the
## internet, this should be a simple matter of typing

install.packages("date") 

## (obviously substituting the desired package between the quotes). R
## will ask you to select a mirror site for download and in a few
## moments the package is ready to be made available with the library
## function,

library("date") 

## You may find that the install.packages() command doesn't work on
## public or cluster computers because you do not have permission to
## install software on these machines. If this is the case, you will
## most likely have to contact the system administrator and have them
## install the desired packages for you.

## This concludes Tutorial I. The benchmark questions on the following
## page should help you assess how well you have learned the
## material. You should feel free to ask for help from your
## instructors and from your peers when you find yourself stuck or
## baffled.

###################################################################### 
## E. Benchmark Questions
###################################################################### 

#################### 
## 1. Perform this calculation in R:
##
## 2.5 * exp( 1.95 * +/- sqrt( 10) )
## (Hints: The +/- sign can be represented by a vector.) 
####################

#################### 
## 2. Enter the following data as two vectors named week and cases.

## Week	Number of cases
## 1	        0
## 2	        0
## 3	        0
## 4	        1
## 5	        2
## 6	        0
## 7	        0
## 8	        0
## 9	        0
## 10	0
## 11	0
## 12	0
## 13	2
## 14	0
## 15	0
## 16	3
## 17	0
## 18	0
## 19	0
## 20	0
## 21	1
## 22       0
## 23	0
## 24	0
## 25	0
## 26	2
## 27	0
## 28	0
## 29	0
## 30	0
## 31	0
## 32       0
## 
## (Hint: The vector week can be created easily using the R function
## seq() or the : operator.)
#################### 


#################### 
## 3. Generate an epidemic curve using your newly-created vectors and
## the barplot() function.  Label your x- and y-axes, give your figure
## a title, set the y-axis to go from 0 to 6, and remove the spaces
## between bars.

## Examine the epidemic curve and determine whether you think the data
## represent a common source, point source, or propagated epidemic (or
## some combination of these). Explain your reasoning.
#################### 

