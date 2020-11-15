# R is like a fast calculator

232047294879347 * 242424 / 12313 + 2

# in R <- is used to assign a value to a variable. You can also use =

test_variable <- 232047294879347 * 242424 / 12313 + 2
test_variable

#you can use rm() to remove a variable, object
rm(test_variable)
test_variable = 2
test_variable

# you do not have to remove to update or change the variable
# and you can change the variable using it's own value
test_variable <- test_variable * test_variable
test_variable

#### The datatype of each variable is inferred

#### R's datatypes ####

# Logical is either TRUE or FALSE
variable <- FALSE 
class(variable)

# Numeric is a number that can include fractual parts, i.e. 2.34 
variable <- 2.34 
class(variable)

# Integer is a whole number
variable <- 2L 
variable
class(variable)

##### Sidetrack

# Integers are cheaper than Numerics
object.size(c(1L,2L,3L,4L,5L)) < object.size(c(1,2,3,4,5))
# Unless they are empty
object.size(numeric()) == object.size(integer())
# Than they are both 48bytes
object.size(numeric())

##### Back to data types

# You can also express complex numbers
variable <- 3 + 2i
variable
class(variable)
# And plot them
plot(variable)

# Strings are called characters in R
variable <- "hello world"
variable
class(variable)

# And there is also raw data
variable <- charToRaw(variable)
variable
class(variable)

# raw objects are small. empty raw objects are also 48bytes though

object.size(raw())

### The types you will use most are characters, numeric, and logical

#### R's data structures  ####

### Technically everything that you have produced so far are vectors
### Vectors can be combined using the c() function

VectorA <- 2
VectorB <- c(1L,2L)
c(VectorA,VectorB)
class(c(VectorA,VectorB))
class(c(VectorB))

### A list is an R-object which can contain many different types
### like vectors, functions and even another list inside it.

list(VectorA, VectorB, list(VectorA, VectorB), mean)

### A matrix is a two-dimensional rectangular data set of ONE data type. 
### It can be created using a vector input to the matrix function.
### Create a matrix.
M <- matrix( c('a','a','b','c','b','a'), nrow = 2, ncol = 3, byrow = TRUE)
M

# Hold on, how does the matrix() function work
?matrix

# Compare
M <- matrix(c('a','a','b','c','b','a'), nrow = 2, ncol = 3, byrow = FALSE)
M

# Got it?

### A factor stores the vector and all its unique values
# Create a vector.
apples <- c('iOS','iPhone','MBP','iPhone','MBP')
# Create a factor object.
factor_apples <- factor(apples, levels = c("iOS", "iPhone", "MBP", "Mac"), ordered = T)
factor(apples)

# Have a look
apples
factor(apples)
factor_apples
nlevels(factor_apples)
factor_apples[4]
as.character(factor_apples[4])

### Data Frames are mixed data presented in tabular form
# Create the data frame.
data_types <- data.frame(
  types = c("logical", "numerical","integer", "complex", "character", "raw"), 
  usage_probability = c(1, 1, 1, 0.1, 1, 0.5), 
  got_it = c(rep("yes", 5), "no"),
  answer = rep(42L, 6)
)
View(data_types)

# Aside from Base R function, you can include
# third party function, objects, and data
# via library

library(tidyverse)

# but before you can use the library, you have to
# install it with the install.packages() function

install.packages("tidyverse")

# you only have to do it once on each new system
# or after upgrading your R version

library(tidyverse)

# The tidyverse package is a library that contains
# a lot of useful features and it is the package, I use
# in almost every R programme
# It has also change the dataframe type to a more modern
# object called tibble

data_types_tibble <- as_tibble(data_types)

# Let's have a look
data_types
data_types_tibble

# Trees (e.g. when we want to parse JSON, XML)  
## In R trees are just lists of lists
## (R likes copying (a lot), so if you want to store the data as a more complex graph, you need to use a graph db)

tree <- list(NodeA = list(NodeA_A = 1, NodeA_B = 2), NodeB = list(NodeB_A = 3, NodeB_B = list(NodeB_B_A = 4, NodeB_B_B =5)))

# Helper function for displaying the components of a list as a tree #
# Source: https://stackoverflow.com/questions/18122548/display-names-of-column-of-recursive-list-as-tree ####

nametree <- function(X, prefix = "") {
  if( is.list(X) )
    for( i in seq_along(X) ) { 
      cat( prefix, names(X)[i], "\n", sep="" )
      nametree(X[[i]], paste0(prefix, "  "))
    }
}

nametree(tree)

# whole tree
tree
# left child: list(NodeA_A = 1, NodeA_B = 2)
tree[[1]]
tree$NodeA

# right child list(NodeB_A = 3, NodeB_B = list(NodeB_B_A = 4, NodeB_B_B =5))
tree[[2]]
tree$NodeB

# left child of right child: 3
tree[[2]][[1]]
tree$NodeB$NodeB_A

