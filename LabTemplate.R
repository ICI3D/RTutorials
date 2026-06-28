#####################################################################

## Lab: LAB TITLE [no number]

#####################################################################

## Clinic on the Meaningful Modeling of Epidemiological Data
## International Clinics on Infectious Disease Dynamics and Data (ICI3D) Program
## https://www.ici3d.org

## Attribution: ORIGINAL (date)
##              SUBSEQUENT (date)
##              YOU (date) ## if you did something interesting
## Last update: YOU (date)

## Some Rights Reserved
## CC BY-NC 4.0 (https://creativecommons.org/licenses/by-nc/4.0/)

#####################################################################

## The GOAL of this lab is FILL IN HERE
## By the end of this lab, you should be able to:
##
##  * CREATE a horrific monster
##  * REFUSE TO CREATE a horrific monster
##  * WRITE A SCRIPT to automatically log the actions of the monster that you are definitely not going to create
##
## NOTE: The comments will guide you through the lab but you
## should make sure you understand what the code is doing.  Some
## code may have question marks and cause errors. 
## FIXME: Take out the above line if no question marks in this lab.
## You should fill these in; often you will find suggestions in the comments

## Before you start, it is a good idea to clear your workspace.
## In rstudio, do this with Session/Restart
## There is also a hotkey, which you should memorize and use frequently
## ctrl-shift-F10 (or cmd-shift-F10 on Mac)
## In plain R, just quit `q()` and then start again

######################################################################
## Section 1: Creating a monster capable of devouring Tokyo
######################################################################

## We will now define a function that will create a Godzilla-like monster

## It is important NEVER to run this function

create_godzilla <- function(x){
	return (log(x/(1-x)))
}

godzilla(?)
