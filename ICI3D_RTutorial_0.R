## Introduction to R Studio - A tutorial
## Clinic on Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data Program
## African Institute for Mathematical Sciences, Muizenberg, Cape Town, RSA
##
## Dario Fanucchi 2012
## Updated by Juliet R.C. Pulliam 2014, 2023
## 
## Some Rights Reserved
## CC BY-NC 4.0 (https://creativecommons.org/licenses/by-nc/4.0/)
##
##
## RStudio is an Integrated Development Environment (IDE) for the R programming 
## language. Much like Emacs, Vim, Gedit and other editors; RStudio provides 
## facilities for editing source files, running lines or sections of code in the 
## R console, typing into the console directly, and exploring/changing the 
## working directory. RStudio also offers some R-specific behavior, such as
## keeping track of the variables currently in the workspace and showing the
## command history.
##
## This tutorial serves as an introduction to the RStudio IDE, so that you can
## maximize your productivity when working through R tutorials. Once you've
## worked through the tutorial, you should be able to:
##
##  - understand the main windows in RStudio (Editor, Console, Environment,
##  History, Help, Plots, Packages)
##  - run code from the console window or the source window
##  - browse the installed packages and load libraries
##  - make use of the Help system
##  - use shortcut keys
##
## If you are not reading this file from within RStudio, you can open it in
## RStudio in one of several ways:
##
##  - Open the file using "File"->"Open File" in the RStudio editor
##  - At the terminal/cmd write the following:
##  rstudio introRstudio.R (or, on a Mac: open -a RStudio introRstudio.R)
##  - In RStudio in the files window (bottom right of the screen or Ctrl+5 to
##  activate), browse to the location of the file by clicking the "..." to the
##  top-right of the window, and then open the directory. Click on the file in
##  the files window and it will load in the editor.
##
## Note that - throughout the tutorial - Mac users should use the command (open
## apple) key if the keystroke sequence calls for the control key but does not
## appear to function as expected.

################################################################################
# 1. Using RStudio to run line-by-line from scripts
################################################################################

# In RStudio you can execute commands line by line or in the console.
# Copy the following command (without the "#") into the console and press enter:
#    plot(c(1,2,3,4,5), c(1,2,3,4,5))
#
# You'll notice that a plot appears in the plots window as a result of the
# command.
# Instead of typing directly in the console you can type the command in a .R
# script like this one and run it in the console.

# Place your cursor on the line below and press "Control+Enter"

plot(c(1,2,3,4,5), c(1,2,3,4,5), "l")

# Notice how a plot appears in the plot window corresponding to this command.
# Don't worry about exactly how the plot command works yet.

# Anything that you run from a script using "Control+Enter" gets typed directly
# in the R console. You can equivalently copy everything out in the console and 
# it will work fine, but it is generally easier to work in the editor and use
# "Control+Enter" to execute commands in the console.

################################################################################
# 2. Variables, the Environment, and basic arithmetic
################################################################################

# In R you can assign a value to a variable by using the "<-" operator.
# Run the following line of code by pressing "Control+Enter"

x <- 10

# Make sure the Environment window is visible by typing "Control+8".
# Notice that the Environment window now contains a variable x, with a value of
# 10. 

# To remove x from the workspace use the "remove" function:

remove(x)

# Notice how x no longer shows up in the Environment window. Typing x now produces
# an error:

x  # will produce an error: object 'x' not found!

# R can be used to perform arithmetic in a similar fashion to a calculator.
# For instance, you can type this:

(1242 - 241.1) * 32.21

# When you press Control+Enter, the cursor automatically moves to the next line
# in the console. This way, you can run scripts one line at a time by repeatedly
# pressing Control+Enter.

# The following code does some basic arithmetic.  Run it line by line:

x <- 2              # ask someone to guess a number between 1 and 10
y <- x * 9          # tell them to multiply x by 9
d1 <- floor(y / 10) # get the first digit  (floor(x) gives the integer part of x)
d2 <- y %% 10       # get the second digit (a %% b gives the remainder when a is divided by b)
d1 + d2 - 4         # tell them to add the digits and subtract 4: they are left with the number 5!

# The code above performed an old classic arithmetic "teaser". Starting with any
# number (x). Multiply it by 9, add the digits of the result, subtract 4 and
# your answer will always be 5.

# Try changing the initial value of x to some other number between 1 and 10 and
# see if it works.
# Notice how it is possible to add comments next to code to explain what it
# does.

# Notice how as you ran the code lines above, the variables d1, d, x and y were
# added to the workspace with their values
# You can see in the workspace that d1 and d2 are indeed the first and second
# digit of y respectively.

# Since we're done with these variables, we'll keep our workspace clean by
# removing them:

rm(x, y, d1, d2) # The rm() function is equivalent to the remove() function used above


################################################################################
# 3. Running sections of a script
################################################################################

# Before we begin this next section, you may want to clear your console so that
# things are less cluttered. Simply type "Control+L", and the console will be
# cleared. If you wish to view your previous commands, you can still place the
# cursor in the console and press the up arrow to see them.

# You can press Control+Up-arrow (with your cursor in the console) to see your
# command history in a pop-up window. If you don't want to reach for a mouse,
# you can switch back to the editor from the console with "Control+1"

# Sometimes you don't want to run your code line-by-line but rather in a batch.
# In RStudio you can simply highlight the code you want to run and press
# "Control+Enter" just as before to run all the code you highlighted.

# The code below computes the prime numbers in the range 1-100 and writes them
# in the console. There are many control structures and commands in this code, and 
# it might feel excessive to run it line-by line. Don't worry about how it works 
# for the moment (although there are line-by-line comments for the very
# enthusiastic reader). In order to run it all in one shot, highlight the whole 
# section and press "Control+Enter".

x <- rep(TRUE, 100)              # make a vector of 100 elements all equal to TRUE
prim <- NULL                    # Empty list of currently known primes
x[1] <- FALSE                   # 1 is not a prime number
for(i in 2:100){                # go through the list from 2 to 100
  if(x[i] == TRUE) {                  # if i is known to be prime...
    prim <- append(prim, i)             # ... add it to the list of known primes
    if(i <= 50)                           # ... if i has multiples in [1,100] ...
      for(j in i*(2:(100/i))  ) x[j]=FALSE    # ... and set all multiples of i to be NOT prime
  }
}
print(prim)                     # print the list of discovered primes to the console

# Notice that all the code gets "typed" into the console when you press
# Control+Enter. The list of primes below 100 is produced.

################################################################################
# 3. Vectors in R
################################################################################

# Look at your workspace (Ctrl+8 if it isn't showing). There are two variables i
# and j that are single valued variables similar to those we've seen before. i
# has a value of 100L. The L just refers to the data type associated with i: in 
# this case it is a "long integer". 
# There are also two other variables in your workspace that are not as familiar:
# x and prim. These are vectors.
# Next to prim is a summary of the structure of the data it contains:
# int [1:25]. This says that prim is a vector containing 25 integers.

# We can make a new vector by using the "c" (concatenate) command:

myvect <- c(1.1, 2.2, 3.3, 4.4, 5.5, 4.4, 3.3, 2.2, 1.1)

# Notice that it is summarized as num [1:9]. It contains 9 elements, each of
# which is a numeric data type.

# We now have quite a few variables hanging around in the workspace.

# Clean up the entire workspace by clicking the "Clear All" button, which looks 
# like a broom, or typing the following code:

rm(list=ls())

################################################################################
# 5. Some RStudio-specific tips and tricks
################################################################################

##### Using code completion and Help

# Code Completion is an extremely useful feature that has been very well
# implemented in RStudio. If you begin typing the name of some function, you can
# get RStudio to complete it for you (or provide a list of possible completions)
# by simply hitting the TAB key at the end of the line so far. If several
# completions are possible, they are displayed in a list
# below the cursor. Hitting TAB twice will automatically complete using the
# first option available.
# For example, try completing some of the following phrases by hitting TAB
# (see what options you have):

plo
do.
len
strs
med

# Note that if there is only one option available, hitting TAB will automatically
# complete the command (as with "strs" above).

# Autocompletion works both in the source editor and in the console.

# Notice how the autocompletion also gives you a bit more information about the
# functions. If you need still further information, you can make use of the Help
# option: Press F1 while hovering over a valid R function or command. For
# example, try the completion of the last statement above.

# Press F1 over the line of code below (fn+F1 on a Mac):

median

# Notice that the help window (by default in the bottom right corner) loads
# the help for "Median Value". This feature is very useful indeed, and it can
# increase your productivity in R substantially to be able to obtain help very
# quickly on functions you wish to use in this fashion.

##### Console History and History tab

# As previously mentioned, it is possible to obtain previous statements typed
# into the console by pressing the up arrow when inside the console.
# If you would like a bit more control over your previous statements, you can
# open the History window.
# The History window can be activated by typing Ctrl+4, or by clicking on the
# History tab. Inside the History window, you can scroll through all code
# previously run in the console.
# Double clicking an entry in the History window (or clicking Enter on it)
# will cause it to copy to the console, where it can be evaluated.
# Alternatively, you can click the "To Source" button (or type Shift+Enter)
# to send the highlighted line(s) to the current location in the source editor.

# As an example, run the following lines of code:

x <- 1
y <- 1

# and now run the following block of code

y <- y * (x + 1)
x <- x * (x %% 2 + 1) + 1

# Place the cursor in the source below this comment, then look in the history
# window for the above two lines, highlight them and click Shift+Enter. Observe
# how the lines get copied below. You can type this several times to generate
# several copies.
# You can then use Ctrl+1 to get back to the source editor and run the code.
# Alternatively you could send the code directly to the console from the
# History. Try both.

# ^ ^ ^ Place cursor above this line when inserting from history ^ ^ ^

##### Including Packages

# To load an already-installed package, you can go to the packages tab (Ctrl+7),
# and select the package that you want to use. For instance, you can include the
# Matrix package in this fashion. You can also do this in code as follows:

require("Matrix")

# once you've included a package, you can make use of functions from that
# package. You can click the name of a package in your Packages window to get
# help for that specific package. Here are some examples using the Matrix
# package

M <- Matrix(0, 2, 2, sparse=FALSE)
M[1,1] <- 2; M[1,2] <- 3
M[2,1] <- 3; M[2,2] <- 4
M
det(M)


################################################################################
# 6. Running the whole script
################################################################################

# There are a few more options for running code in RStudio other than typing
# "Control+Enter". They are provided here with their shortcuts for completeness.
#     1. Run the ENTIRE script:                              "Control+Alt+R"
#     2. Run the script FROM THE BEGINNING UP TO THIS LINE:  "Control+Alt+B"
#     3. Run the script FROM THIS LINE TO THE END:           "Control+Alt+E"
# These can all be useful in different contexts. You can try them out by moving
# to various places in this script and typing these key combinations. (Note,
# however, that there is an error produced on line 96, so you will not be able
# to run the entire script in this case.)
#
# For additional keyboard shortcuts, see Tools > Keyboard Shortcuts Help.

################################################################################
# Appendix. List of useful RStudio Shortcut keys
################################################################################

# [ For a COMPLETE list of keyboard shortcuts type "Alt+Shift+K" (on Linux
# or Windows) or "Option+Shift+K" (on a Mac). Note that some of the shortcuts
# below work differently on Macs.]

# Ctrl+1          (Move cursor to Editor)
# Ctrl+2          (Move cursor to Console)
# Ctrl+3          (Show Environment window)
# Ctrl+Enter      (Editor: run a line, or a highlighted section of code)
# Ctrl+L          (clear the console)
# Ctrl+Up-arrow   (Console: see list of previous commands; Editor: go to top of
# file; use Cmd+Up-arrow on a Mac)
# Ctrl+Alt+R      (Editor: Run the entire script)
# Ctrl+Alt+B      (Editor: Run the script from beginning to this point)
# Ctrl+Alt+E      (Editor: Run the script from this point to end)
# F1              (Editor: Get help on whatever is under the cursor)
# F2              (Editor: Show source code for whatever is under the cursor -
# if it exists)
# Ctrl+Shift+C    (Editor: Toggle Comment on selected region)
