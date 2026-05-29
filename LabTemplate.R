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

## GOAL of this lab is.
## By the end of this lab, you should be able to:
##
##  * CREATE a horrific monster
##  * REFUSE TO CREATE a horrific monster
##  * WRITE A SCRIPT to automatically log the actions of the monster that you are definitely not going to create
##
## NOTE: The comments will guide you through the lab but you
## should make sure you understand what the code is doing.  Some
## code may have question marks and cause errors. 
## You should fill these in; often you will find suggestions in the comments

## Before you start, it is a good idea to clear your workspace.
## In rstudio, do this with Session/Restart
## There is also a hotkey, which you should memorize and use frequently
## In plain R, just quit and restart `q()`

######################################################################
## Section 1: Creating a monster capable of devouring Tokyo
######################################################################

## We will now define a function that will create a Godzilla-like monster

## It is important NEVER to run this function

create_godzilla <- function(x){
	return (log(x/(1-x)))
}

godzilla(?)
