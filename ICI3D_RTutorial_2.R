

#' @title Tutorial 2: More on Vectors, Data Frames, and Functions
#'
#' @author David M. Goehring 2004
#' @author Juliet R.C. Pulliam 2008, 2009
#' @author Steve Bellan 2010, 2012
#' @author Carl Pearson 2024
#' @author Stanley Sayianka, Reshma Kassanjee 2025
#'
#' Clinic on Meaningful Modeling of Epidemiological Data
#' International Clinics on Infectious Disease Dynamics and Data Program
#' African Institute for Mathematical Sciences, Muizenberg, Cape Town, RSA
#'
#' @license Some Rights Reserved,
#' CC BY-NC 4.0 (https://creativecommons.org/licenses/by-nc/4.0/)
#'
#' @description
#' By the end of this tutorial you should:
#' * Be able to retrieve useful subsets of your data
#' * Understand more about data frames
#' * Know the methods and uses of logical values in R
#' * Be able to generate and use factors
#' * Know how to write your own generic functions
#'
#' @section A. Accessing Vector Elements
#'
#' @subsection Beyond Numbers: Relational and Logical Operations in R
#'
#' So far everything you have done in R has involved numbers or
#' vectors of numbers. To properly exploit R’s complexity, you need to
#' become familiar with relational and logical operations in R.
#'
#' Relational operations work just like numerical operations, in terms
#' of how they are processed. Return for a moment to our first
#' calculation from the last tutorial, an addition problem:

3 + 4

#' The analogous calculation of a single relational operation is
#' something like

5 > 4

#' "Is 5 greater than 4?” Yes. And R tells you that this is a TRUE
#' statement. Or,

1 + 1 < 1

#' Makes sense, right?
#'
#' The greater-than, >, and less-than, <, symbols are
#' straightforward. Similarly, R has greater-than-or-equal-to and
#' less-than-or-equal-to symbols, >= and <=, respectively.
#'
#' Slightly less intuitive are the relational operators for equality,
#' ==, and inequality, !=. Try

x <- 4

x == 1 + 3

y <- x != 4

#' This last example demonstrates that variables can hold logical
#' values. These relational operators also operate on logical values,
#' as in,

y == FALSE

#' Logical operations are operations that only make sense when
#' performed on TRUE and FALSE values. These will likely be familiar
#' to you, the central operations being AND, OR, and NOT.
#'
#' The operators used in R are standard: &, |, and !,
#' respectively. Let’s see them in action:

!TRUE
to_be <- FALSE
to_be | !to_be
FALSE & (TRUE | FALSE)

#' By combining logical and relational operations, we can make complex
#' inquiries about values.
#'
#' Hands off the keyboard! Pick up a writing implement…
#'
#' a <- TRUE != (4 > 3)
#' b <- a | 1 + 1 == 4 - 2
#' c <- !FALSE & (log(Inf) == Inf + 1)
#'
#' What do a, b & c equal?  Now execute the commands and compare
#' your answers.



#' Note that R has special values for infinity (Inf), not-a-number
#' (NaN), and not-applicable, NA. These generally behave sensibly – a
#' mathematical operation on not-a-number is obviously not a number as so is
#' returned as NaN. Things are less simple when using logical and relational
#' operators. Consider 4 != NaN. In one respect, the answer perhaps
#' should be TRUE; that is, 4 definitely isn’t equal to
#' not-a-number. But, striving for consistency, R returns NA, much as
#' it would for a mathematical operation. Even worse is the situation

x <- NaN
x == NaN

#' You might think that this is a reasonable test for whether x has a
#' numerical value, but it won’t work for the same reason mentioned above.
#' In general, keep this trickiness in mind and remember there is a special
#' function is.nan() for determining whether x is defined as not-a-number:

is.nan(x)

#' There is also a special function is.finite() for determining whether
#' x is a valid (finite) number:

is.finite(x)

#' This is all getting thrown at you in very quick succession,
#' especially if you do not have much experience programming in other
#' languages. It is worth noting that information about these
#' operations can be pulled up at any time by typing help("&”) or
#' help(">") or the using help() function with any of the other
#' symbols used in these operations.
#'
#' @subsection Vectors of Logical Values
#'
#' As a shorthand, TRUE and FALSE can be entered as T and F. This
#' allows for rapid entry of vectors of logical values, for example:

logical_vec <- c(TRUE, TRUE, FALSE, TRUE)
logical_vec

#' Unfortunately, and rather inexplicably, T and F cab be reassigned
#' to any arbitrary values. This will render most code utterly
#' unpredictable. So, never, never, never do this:

T <- 4 # REALLY BAD, BUT NO ERROR IS PRODUCED

#' And, if you ever do something like this (though you shouldn’t!),
#' make sure you quickly do this:

rm(T) ## which will set T (or F) back to its default logical value.

#' Aside: to ensure your code is robust, we recommend always spelling out
#' TRUE and FALSE for logical values. If you have a library like `lintr`
#' installed, you can use it to check your code `lintr::lint("your_code.R")`
#'
#' Relational or logical operations also act on vectors to produce
#' vectors of logical values, as in,

x <- rnorm(10)
x < 0
y <- (x > -.5) & (x < .5)
!y

#' This will be especially handy when we look at the concept of
#' indexing, below.
#'
#' @subsection Generating Sequences
#'
#' There are many occasions in R when you need a patterned sequence of
#' numbers. As mentioned in the previous tutorial, most counting can
#' be accomplished by use of the seq() function. If you haven’t
#' already done so, it is worth taking a look at the help-file on
#' seq() because it has a few arguments that can make your life
#' easier.

?seq

#' For example, seq() can generate a vector of a certain length
#' between certain endpoints by typing

x <- seq(0, 1, length.out = 20)

#' giving you a vector of length 20 between 0 and 1; to confirm this, type

length(x)

#' A very common need in R is to generate vectors with an interval of
#' 1 between each element. R has a shorthand for this using the colon
#' notation, as follows:

y <- 5:10

#' This generates a vector that counts from 5 to 10, inclusive. Note that
#' : is generally treated first in the order of operations.
#'
#' Don’t underestimate the value of the colon notation.  Even for
#' typing a vector of length 2, like "(1,2)” or "(2,1),” using the c
#' function to generate the vector is pretty tedious (e.g., c(1,2)).
#' These vectors can be generated in three quick characters by typing
#' 1:2 or 2:1, respectively.  I will also point your attention to the
#' rep() function, for repeating sequences, which can also save time.
#'
#' @subsection Indexing
#'
#' R has an incredibly useful way of accessing items from a
#' data set. Each item in a data set has its own index, or numbered
#' location, in the object’s structure. Square brackets are used to
#' extract an item or items from a data set, but it is crucial to
#' understand that there are two completely distinct ways in which
#' brackets are used to access items. I will consider the two methods
#' for accessing a vector of length n in turn below.
#'
#' The first option: Logical
#' Requirements: Logical vector of length n
#' Use it for: Finding a subset of data based on a rule
#'
#' Logical indexing works as if you’ve asked your indexing vector the
#' question, "Do you want this item?” for each of the items in the
#' vector.

x <- 1:5

x[c(TRUE, FALSE, FALSE, FALSE, TRUE)]

#' If we combine this logical indexing with the relational and logical
#' operators you learned above, we have an exceptionally powerful tool
#' to retrieve data that meet any set of criteria.

y <- rnorm(10000)

hist(y[!((y > -2) & (y < 0))])

#' I will give more insight below when I discuss indexing data
#' frames. Stay tuned.

#' In any operation in R, vectors will be automatically repeated until
#' they reach the necessary length for the operation to make
#' sense. For example, note the results of

1:6 + 1:2

#' The same repetition holds for logical vectors. For this reason, you should
#' be very cautious using operations on vectors that differ in length.
#'
#' The second option: Numerical
#' Requirements: Value or vector of any length with values
#' (1 to n) OR (-n to -1)
#' Use it for: Single item retrieval or shuffling, sorting, and repeating
#'
#' Accessing single items with brackets and a single index should be
#' straightforward

x <- 3 * (0:5)
x[4]

#' One tedious way of creating a new vector of values from a vector’s
#' elements would be

c(x[2], x[3], x[4]) #TEDIOUS

#' So R makes it much easier by allowing a vector of indices to
#' generate a vector.  Thereby, the command above becomes

x[2:4]

#' There is nothing preventing you from accessing any element any
#' number of times.

x[c(2, 2, 2, 5, 5, 5)]

#' Additionally, R allows you to use negative indices, indicating
#' which items you want to exclude, as in,

x[c(-1, -6)]

#' This is fine and productive as long as you remember never to mix
#' negative and positive indices – R will not know what you want it to
#' do:

x[c(-1, 4)] # BADCODE

#' @subsection Sorting
#'
#' In Tutorial 1, you were introduced to the sort() function, which is
#' handy.
#'
#' Now that you have been introduced to indexing, you may have an
#' inkling of how much more powerful the sorting functions of R can
#' become.
#'
#' As an introduction, let’s say you have a 4-element vector,

my_vector <- 5:8

#' Using numerical indexing, we can manually re-order this vector by
#' calling each of its indices once in our preferred order, for
#' example

my_vector[c(2, 3, 4, 1)]

#' or, for a quick reversal

my_vector[4:1]

#' Now, manually generating the vector of indices is not monumentally
#' useful, which is where the function order() comes in. As a
#' demonstration, imagine we have a vector of student names and a
#' corresponding vector of student heights (in meters).

student_names <- c(
  "Dario", "Hloniphile", "Steve", "Innocent", "Abigail", "Cynthia"
)

student_heights <- rnorm(6, 1.7, .12)

#' What we definitely don’t want to do is to perform sort() on each of
#' these vectors independently. This will eliminate the pairing of the
#' name to the height. So how can we sort one vector and have the
#' other vector align correctly? Try order() on the names,

order(student_names)

#' Note that it returns the indices in the right order, not the values
#' themselves.
#'
#' From what you learned above, you know it is now an easy matter to
#' sort both of our vectors, as follows,

student_names[order(student_names)] # same effect as sort()
student_heights[order(student_names)]

#' And, obviously, sorting the names by the heights is exactly
#' analogous, and it will make for a pretty plot

barplot(
  student_heights[order(student_heights)],
  names.arg = student_names[order(student_heights)],
  ylab = "Height (m)", main = "Student Heights"
)

#' I have conveniently skipped over an important concept, because R
#' handles it fairly intuitively, but I want to mention the
#' terminology. The variable student_names and the results of ls(), for
#' example, are called vectors of strings, or character arrays. R
#' handles them conveniently, so we don’t need to worry too much about
#' them, but knowing the terminology will improve your understanding
#' of R's in-line help documents.
#'
#' @section B. Data Frames and Alternatives
#'
#' @subsection Data frames, redux
#' 
#' Before we cover advanced topics of data frames, I wanted to point out the
#' function data.frame(), which puts data together to form data
#' frames. This is a key alternative to using the prefab data frames
#' that you used in the previous tutorial.
#'
#' First I want to generate a vector of student class-years to
#' correspond to the student_names before creating a data frame (Freshmen
#' as 1, Sophomores as 2, etc.).

student_years <- c(4, 2, 2, 3, 1, 3)

#' Now making a data frame is easy (each argument will just add more
#' columns to the data), the only trick being that we have to assign
#' the constructed data frame to a variable, as follows:

student_df <- data.frame(student_names, student_heights, student_years)
student_df

#' Voila! Your own data frame.
#'
#' You may want to have better column headings than the redundant variable
#' names. There are various options to accomplish this. One option is
#' to use the names() function with assignment notation. Let’s take a look:

names(student_df)

#' What we see is a vector of strings corresponding to the current column
#' names. We can change these by assigning replacement strings to the
#' indexed values or by substituting our own vector of strings.

names(student_df) <- c("names", "heights", "years")
student_df

#' If we think that "years" is ambigous and might be confused with a student's
#' age, we could rename just that column using numerical indexing, e.g.:

names(student_df)[3] <- "class.years"
student_df

#' There is also a similar option, row.names() to access and modify the
#' the row names. By default, the row names are a series of integers indicating
#' the row number:

row.names(student_df)

#' The assignments above are the first of many examples in R that seem
#' to defy logic: it seems as though we’re assigning something to a
#' function, which shouldn’t make sense because a function isn’t a
#' variable. In fact, you can think of the functions names() and
#' row.names() as "access functions” – they do not perform an action,
#' but merely grant access to a property of the argument variable, and
#' this is why we can make assignments of the sort seen above.
#'
#' @subsection Indexing data frames
#'
#' As with vectors, brackets and logical or numerical vectors are still
#' the way to access data frames, but with a slight complication,
#' because data frames are multidimensional. The solution (which also
#' holds for matrices, etc.) is to separate the two dimensions with a
#' comma. R treats the first entry as the row number and the second
#' entry as the column number; thus, to access the second column of
#' the fourth row, type

student_df[4, 2]

#' Or the second column of the last three rows,

student_df[4:6, 2]

#' There are two further complications.
#'
#' To access an entire row or entire column, leave the index blank, as
#' in,

student_df[, 1] # FIRST COLUMN
student_df[3, ] # THIRD ROW
student_df[, ] # ENTIRE FRAME, equivalent to "student_df"

#' @subsection Tidyverse
#'
#' In R, data frames are a common and versatile way to store and work with 
#' tabular data. However, there are other object types that you may find useful.
#' For example, tibbles (from tidyverse) behave like data frames but print more
#' cleanly and are widely used in with modern R workflows. Data tables (from the
#' data.table package) are another alternative, designed for speed and efficient
#' handling of large datasets. Choosing the right structure depends on the
#' context of your analysis and the tools you prefer to use. 
#' 
#' In the last tutorial, we learned about the tidyverse.
#' Tidyverse is a group of R packages like dplyr, stringr, ggplot2, and more.
#' You can find the full list here: https://www.tidyverse.org
#'
#' Instead of loading all tidyverse packages using the library(tidyverse) command,
#' we can load only the one we need. For now, we just need the dplyr package.
#' 
#' We now return to indexing data frames, repeating what we did above 
#' using dplyr to access rows and columns in a data frame; and extending our 
#' examples, demonstrating both base R and dplyr implementations.


library(dplyr)

#' To select a column, we use the select() function.
#' The first argument is the data frame, the second is the column we want.
#' We can use either the column number or the column name.

select(.data = student_df, 1)       # selects the first column by position
select(.data = student_df, names)   # selects the column called 'names'

#' In select(), the .data argument is where we put the data frame.
#' The next argument is the column(s) we want to choose.

#' We can also use the pipe symbol (|>) to make the code easier to read.
#' Think of the pipe as saying "and then".
#' For example, instead of writing select(.data = student_df, 1),
#' we can write: student_df |> select(1)
#' This means: "take student_df and then select column 1"

student_df |> select(1)       # selects the first column
student_df |> select(names)   # selects the column called 'names'

#' If we want to save the result into a new data frame, we can assign it like this:

student_names_data <- student_df |> select(names)

#' The select() function gives us a one-column data frame.
#' If we want just a vector (not a data frame), we can use the pull() function.

student_df |> pull(1)       # gets the first column as a vector
student_df |> pull(names)   # gets the 'names' column as a vector

#' To get a specific row instead of a column, we use the slice() function.

student_df[3, ]             # base R: gets the third row
student_df |> slice(3)      # dplyr: gets the third row

#' We can also use the values in names() or row.names() as indices:

student_df["4", ]
student_df[, "class.years"]

#' Putting all of this together, we can quickly generate subsets of
#' our data. For example, we can create a data frame that includes
#' only the students with height greater than the mean height:

tall_students <- student_df[student_df$heights > mean(student_df$heights), ]
tall_students

#' A tidyverse-style way to filter rows is to use the `filter()` function.
#' This helps us select only the rows that meet a condition.
tall_students <- filter(student_df, heights > mean(heights))
tall_students  

#' Inside `filter()`, we do not use the `$` operator (like student_df$height).
#' This is because `filter()` automatically looks for column names
#' within the data frame you pass as the first argument.

#' The same code using a pipe:
tall_students <- student_df |> filter(heights > mean(heights))
tall_students

#' Or sort our data by various aspects:

student_df[order(student_df$class.years), ]

#' Alternatively, using the `arrange` function from dplyr, we have
student_df |> arrange(class.years)        # in ascending order
student_df |> arrange(desc(class.years))  # in descending order


#' @subsection Introduction to factors
#'
#' When performing statistical analyses, we often want R to look at a
#' set of data and compare groups within the data to one another. For
#' example, you have the data frame containing data on students in a
#' course. There are columns of data representing the students' heights
#' and class.years. How can you look at the means of height by class.year?
#'
#' Or, another example, you have sampled a number of rabbits and have
#' a column for weights before a diet treatment and a column for
#' weights after a diet treatment and a third column stating the diet
#' treatment (e.g, "none,” "grain diet,” and "grapefruit diet”). How
#' can you evaluate the change in weight as affected by diet?
#'
#' The answer to these questions is to use factors.
#'
#' Many of the data sets that come with R already have their data
#' interpreted as factors. Let’s take a look at a data set with
#' factors:

data(moths, package = "DAAG")
help(moths, package = "DAAG")
moths

#' (Note that you may have to install the DAAG package in order to
#' load these data. Do you remember how to do this? If not, ask a neighbor
#' for help!)
#'
#' The help file for the moths data set tells us that our last column,
#' habitat, is a factor. What does this mean?
#'
#' See what happens when we pull up this column by itself:

moths$habitat

#' It looks pretty standard, at first, but then we notice that it is
#' more than just a list of habitat names – it has another component,
#' levels.
#'
#' Factors have levels. Levels are editable, independent of the data
#' itself. To see the levels alone, you can type

levels(moths$habitat)

#' When called that way, it has the identity of a vector of strings.
#'
#' The levels() function behaves just like the names() and row.names()
#' functions (i.e., weird), and you can make assignments or
#' reassignments to the levels - e.g.,

levels(moths$habitat)[1] <- "NEBank"

#' Factors come in exceptionally handy when performing statistical
#' tests, but the various plot functions can give you an idea of uses
#' of a factored variable, such as,

boxplot(moths$meters ~ moths$habitat)

#' The tilde, ~, used in a number of contexts in R, can generally be
#' read as "by,” which gives a general explanation of its use here –
#' visualizing transect length ("meters") by habitat type ("habitat").
#' 
#' Recall, we used the ggplot2 package to make prettier plots
#' We will replicate the `boxplot` above:

## theme_bw is a ggplot “style” that makes things a bit less fancy and more sharp, you may think it looks more scientific
library(ggplot2); theme_set(theme_bw())
ggplot(data = moths) + 
  geom_boxplot(mapping = aes(x = habitat, y = meters))

#'
#' @subsection Making a factor
#'
#' Now that you know how to employ a factored variable
#' the next step is to know how to make a factor out of a
#' variable. The general syntax is:

x <- factor(c("A", "B", "A", "A", "A", "B"))

#' For vectors of strings, like that one. The results are usually fine
#' as is.
#'
#' But let’s go back to our student_df data frame. We listed
#' class.years as a number 1 through 4, but these are discreet
#' categories with well-defined names. A more elegant solution is to
#' factor the column of the data frame, much like is seen with moths.

student_df$class.years <- factor(student_df$class.years)
levels(student_df$class.years)

#' Not ideal, but we can use reassignment to change the names of the
#' years.

levels(student_df$class.years) <- c("Freshman", "Sophomore", "Junior", "Senior")

#' With satisfying (preliminary) results available with:

student_df
boxplot(student_df$heights ~ student_df$class.years)

#' Try to replicate the above plot using ggplot2.

#' @subsection Applying functions to data frames
#'
#' Many functions you might like to apply to your data frames will
#' produce unpredictable results.
#'
#' A few work nicely:

nyc_air <- airquality[, c("Wind", "Temp")]
nyc_air
summary(nyc_air)

#' But others that you might try do not work as you want:

sum(nyc_air) # sums wind and temperature together
mean(nyc_air) # returns an error message ## FIXME (or don't, see below)

#' One solution to these troubles is to use the function apply(),
#' which performs the function named in the third argument on the
#' first argument by the index specified by the second argument
#' (in this case, by column).

apply(nyc_air, 2, sum)
apply(nyc_air, 2, mean)
apply(nyc_air, 2, var)

#' @section C. Composing your own functions
#'
#' A more advanced (and very important) topic
#'
#' So far in R we have used the functions that come with R and its various
#' packages; however, since you will often want to perform the same series
#' of actions on different objects, R makes it relatively straight-forward
#' to compose your own generic functions and store them in R’s memory.
#'
#' Before you start writing a function you need to have your mind set
#' on three things:
#'
#' * What you want to give the function as input
#' * What you want the function to do
#' * What you want the function to give as output
#'
#' @subsection A trivial example
#'
#' Imagine you need to repeatedly transform sets of data, but your
#' transformation is "non-standard.” For this example, I’m imagining that you
#' want the natural logarithm of the data, plus one. We know how to perform
#' these operations on a number we have stored in a variable, no problem,

x <- 1:10

log(x) + 1

#' But what we would really like is a named function which will do
#' this in one step, log.plus.one().
#'
#' What we will do is make an assignment to log.plus.one, but rather
#' than assigning a value (or vector, etc.), we assign a function
#' which we define on the spot. We use the command function, which
#' looks like a function but is not a function. Instead, it’s
#' a control element of the R language – it isn’t executed like a
#' function, but rather it informs R to treat the code around it in a
#' special way.
#'
#' The command function has an interesting syntax. Its arguments are
#' the names of variables which will serve as the arguments for your
#' function (the first of three bullets, above). Then, after this
#' parenthetical bit, comes the meat of the function – what you want
#' it to do and what you want it to give back to you (the last two
#' bullets, above). In our log.plus.one() case, what we want it to do
#' and what we want it to give back happen to be the same thing,
#' therefore we can define it very simply, as follows,

log.plus.one <- function(y) log(y) + 1

#' Cool! Let’s test it out:

log.plus.one(x)

#' It behaves just like we would want it to.
#'
#' Aside: you may also see short functions define using the "lambda" syntax:

log.plus.one <- \(y) log(y) + 1

#' @subsection A separate little world
#'
#' Wait a second. I used y in my function definition but called the
#' function with my variable x as the argument. What happened to y?

y

#' The variable is untouched by the function.
#'
#' In order to keep functions fully generic, when you give the
#' function command, R generates a separate, untouchable variable
#' space which has no interactions with your R workspace. This means
#' that the names of your function arguments (and any variables
#' assigned within your function) can be anything you find convenient
#' – there is never any risk of a conflict with your active variables.
#'
#' @subsection Longer functions
#'
#' Either because the function is too complex to be
#' executed on a single line or because you want to make the
#' function’s methods clearer, you will often generate functions
#' longer than one line. For this purpose, R introduces another type
#' of bracket, curly brackets, { }. These are control brackets and
#' indicate the contents should be treated as a unit.
#'
#' As a final example,

(function(x, y) { z <- x^2 + y^2; x + y + z })(0:7, 1)

#' Note that the function is written on two lines, but this isn’t an
#' issue because of the brackets.  Note also that this function is
#' anonymous. It is never assigned, but used in place.
#'
#' A common tendency when first learning to program is to write code
#' in a condensed form (such as the anonymous inline function defined
#' above) so that it is difficult to follow what is going on when you
#' return to the code later on (or when your instructor is helping you
#' find a bug that is keeping your code from working correctly). While
#' writing code in this way takes a certain amount of cleverness and
#' demonstrates that you have understood the concepts, it is better
#' practice to write out your code so that it is easy to follow. This
#' includes using plenty of whitespace, to make your code easy to
#' read and thoroughly commenting your commands as you go.
#' The example above is therefore better written as follows:


#' @title Sum Values and Sum of Their Squares
#' @description
#' A function that takes two numerical values as input and returns the sum of
#' the values plus the sum of their squares
#'
#' @param x A numerical vector
#' @param y A numerical vector
#'
#' @details Note that `x` and `y` must be compatible lengths, or the recycling
#' rules
#'
#' @return A numerical value, x + y + x^2 + y^2
sum_vals_plus_sum_sqs <- function(x, y) {
  z <- x^2 + y^2  # define z as the sum of the values’ squares
  ss <- x + y + z # add the values to the sum of their squares
  return(ss)      # and return the result as output
}

#' Perform the above function with x equal to the numbers from 0 to
#' 7 and y equal to 1:

sum_vals_plus_sum_sqs(0:7, 1)

#' @section Benchmark Questions
#'
#' This concludes Tutorial 2. Because there are some advanced topics
#' here that require practice to get your head around, you should
#' make sure to work through the benchmark questions before you
#' move on to Tutorial 3.
#'
#' @question Semantics?
#'
#' R sometimes uses confusingly similar names for distinct concepts.
#' Define for yourself: names, factors, levels. When would you use each?
#'
#' @question Subsetting
#'
#' You need a subset of the mtcars data set that has only every other
#' row of data included.
#'  a. Do this with numerical indexing.
#'  b. Do this with logical indexing.
#'
#' @question Function Definition
#'
#' Write a function, `jumble()`, that takes a vector as an argument and
#' returns a vector with the original elements in random order. (Note: R
#' does have a built-in function that can do this, but the point of this
#' question is rather for you to build a new function using the tools that
#' have been introduced in the tutorials so far.)
