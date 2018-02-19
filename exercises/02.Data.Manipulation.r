###################################################
### Creation and Assignment
###################################################
a <- 1                  # Create an object "a" and
                        #    assign to it the value 1.
a <- 1.5                # Wipe out the 1 and make it 1.5 instead.
class(a)                # What class is it?
class(a) <- "character" # Make it a character
class(a)                # What class is it now?
a
a <- "Richard"           # Wipe out the 1.5 and make it "Richard" instead.
b <- a                  # Create an object "b" and assign to it
                        #    whatever is in object a.
a <- c(1,2,3)           # Wipe out the "Richard" and make it a vector
                        #    with the values 1, 2, and 3.
                        #    Never make c an object!
b <- c(1:3)             # Wipe out the "Richard" and make it a vector
                        #    with the values 1, 2, and 3.
b <- mean(a)            # Assign the mean of the object a to the object b.
ls()                    # List all user-created objects
rm(b)                   # Remove b

###################################################
### Numeric
###################################################
a <- 2                # create variable a, assign the number 2 to it.
class(a)              # what is it?
is.numeric(a)         # is it a number?
b <- 4                # create variable b, assign the number 4 to it.
(a + b) ^ 3           # basic math
a == b                # test of equality (returns a logical)
a < b                 # comparison (returns a logical)
max(a,b)              # largest
min(a,b)              # smallest

###################################################
### String
###################################################
a <- "string"        # create variable a, assign the value "string" to it.
class(a)             # what is it?
is.numeric(a)        # is it a number?
is.character(a)      # is it a string?
b <- "spaghetti"     # create variable b, assign the value "spaghetti" to it.
paste(a, b)          # join the strings
paste(a, b, sep="")  # join the strings with no gap
d <- paste(a, b, sep="")
substr(d, 1, 4)      # subset the string

###################################################
### Factor
###################################################
a <- c("A","B","A","B")   # create vector a
class(a)                  # what is it?
is.character(a)           # is it a string?
is.factor(a)              # is it a factor?
a <- factor(a)            # make it so
levels(a)                 # what are the levels?
table(a)                  # what are the counts?
a <- factor(c("A","B","A","B"), levels=c("B","A"))
                          # create a factor with different levels

###################################################
### Logical
###################################################
a <- 2               # create variable a, assign the number 2 to it.
b <- 4               # create variable b, assign the number 4 to it.
d <- a < b           # comparison
class(d)             # what is it?
e <- TRUE            # create variable e, assign the value TRUE to it.
d + e                # what should this do?
d & e                # d AND e is True
d | e                # d OR e is also True
d & !e               # d AND (NOT e) is not True

###################################################
### Missing Data
###################################################
a <- NA              # assign NA to variable A
is.na(a)             # is it missing?
class(a)             # what is it?
a <- c(11,NA,13)     # now try a vector
mean(a)              # agh!
mean(a, na.rm=TRUE)   # Phew! We've removed the missing value
is.na(a)             # is it missing?

###################################################
### Vector
###################################################
a <- c(11,12,13) # a is a vector
a[1]             # the first object in a
a[2]             # the second object in a
a[-2]            # a, but without the second object
a[c(2,3,1)]      # a, but in a different order
a + 1            # Add 1 to all the elements of a
length(a)        # the number of units in the vector a
order(c(a,b))    # return the indices of a and b in increasing order
c(a,b)[order(c(a,b))] # return a and b in increasing order
a <- c(11,NA,13) # a is still a vector
a[!is.na(a)]     # what are the elements of a that aren't missing?
which(!is.na(a)) # what are the locations of the non-missing elements of a?

###################################################
### Exercise
###################################################
# Create a vector d with elements 12,5,NA,6,9,2,NA,17
# Remove NA's and save results in vector e
# Extract all values of e that are bigger than 5
###################################################

###################################################
### Vectorization
###################################################
diameters <- rgamma(n=1000000, shape=2, scale=20)
basal.areas <- rep(NA, length(diameters))
system.time(
            for (i in 1:length(diameters)) {
              basal.areas[i] <- diameters[i]^2 * pi / 40000
            }
)

system.time(
            basal.areas <- diameters^2 * pi / 40000
            )

###################################################
### Dataframe
###################################################
setwd("data/")
ufc <- read.csv("ufc.csv")      # ufc is a dataframe
is.data.frame(ufc)                      # we hope
dim(ufc)                                # the size of the dimensions (r,c)
names(ufc)                              # the labels of the columns
ufc$height[1:5]                         # first 10 heights
ufc$species[1:5]                        # first 10 species
ufc[1:5, c(3,5)]                        # first 5 species and heights
ufc[1:5, c("species","height")]         # first 5 species and heights again
table(ufc$species)

###################################################
### Drop NA's
###################################################
ufc <- ufc[ufc$species != "",]
ufc$species <- factor(ufc$species)
table(ufc$species)

###################################################
### Create new variables
###################################################
ufc$dbh.cm <- ufc$dbh/10            # Dbh now in cm
ufc$height.m <- ufc$height/10       # Height now in metres
str(ufc)

###################################################
### Create new Dataframe with subset
###################################################
temp <- data.frame(my.species=ufc$species,
                   my.dbh=ufc$dbh.cm)
temp[1:5,]

###################################################
### Extracting data
###################################################
ufc$height.m[ufc$species=="LP"]         # Heights of lodgepole pine
mean(ufc$height.m[ufc$species=="LP"], na.rm=TRUE)
                                        # Average height of lodgepole pine
###################################################
### Sapply
###################################################
sapply(ufc[,4:7], mean, na.rm=TRUE)

sapply(ufc, class)

###################################################
### provides the indices of the ob-servations in order of decreasing height
###################################################
ufc$species[order(ufc$height.m, decreasing = TRUE)][1:3]

###################################################
### tapply
###################################################
tapply(ufc$height.m, ufc$species, mean)   # Average height by species

tapply(ufc$height.m, ufc$species, mean, na.rm=TRUE)   # Average height by species

format(tapply(ufc$height.m, ufc$species, mean, na.rm=TRUE), dig=3)

##################################################
### Exercise
##################################################
# What are the mean diameters by species?
# What are the three species with the largest median slenderness (height/diameter)
#    ratios?
##################################################

###################################################
### How would we pull out the identity of the median height tree of the species
### that was eighth tallest on average?
###################################################
(ht.bar.by.species <- tapply(ufc$height.m, ufc$species, mean, na.rm=TRUE))

(species.order.by.ht <- order(ht.bar.by.species, decreasing = TRUE))

(species.by.ht <- levels(ufc$species)[species.order.by.ht])

(sp.8 <- species.by.ht[8])

(m.ht.8 <- median(ufc$height.m[ufc$species == sp.8], na.rm=TRUE))

ufc[which(ufc$height.m==m.ht.8 & ufc$species == sp.8),]

# Single operation
ufc[which(ufc$height.m==median(ufc$height.m[ufc$species ==
  levels(ufc$species)[order(tapply(ufc$height.m, ufc$species, mean,
  na.rm = TRUE), decreasing = TRUE)][8]], na.rm = TRUE) &
  ufc$species==levels(ufc$species)[order(tapply(ufc$height.m,
  ufc$species, mean, na.rm = TRUE), decreasing = TRUE)][8]),]

##################################################
### Exercise
##################################################
# What is the identity of the tallest tree of the species that was the 
# fattest on average?
##################################################

###################################################
### Matrices
###################################################
(mat.1 <- matrix(c(1,0,1,1), nrow=2))
(mat.2 <- matrix(c(1,1,0,1), nrow=2))
solve(mat.1)    # This inverts the matrix
mat.1 %*% mat.2 # Matrix multiplication
mat.1 + mat.2   # Matrix addition
t(mat.1)        # Matrix transposition
det(mat.1)      # Matrix determinant


apply(ufc[,4:7], 2, mean, na.rm=TRUE)

###################################################
### Lists
###################################################
(my.list <- list("one", TRUE, 3))
my.list[[2]]

my.list[2]

(my.list <- list(first = "one", second = TRUE, third = 3))
names(my.list)
my.list$second
names(my.list) <- c("First element","Second element","Third element")
my.list
my.list$`Second element`

###################################################
### Merging
###################################################
(params <- data.frame(species = c("WP", "WL"),
                      b0 = c(32.516, 85.150),
                      b1 = c(0.01181, 0.00841)))

(trees <- ufc[ufc$species %in% params$species & !is.na(ufc$height.m),][1:3,])

(trees <- merge(trees, params))

(trees$volume <-
 with(trees, b0 + b1 * (dbh.cm/2.54)^2 * (height.m*3.281))*0.002359737)

###################################################
### Reshaping
###################################################
(trees <- data.frame(tree = c(1, 2), species = c("WH", "WL"),
                    dbh.1 = c(45, 52), dbh.2 = c(50, 55),
                    ht.1 = c(30, 35), ht.2 = c(32, 36)))

(trees.long <- reshape(trees,
                       direction = "long",
                       varying = list(c("dbh.1","dbh.2"),
                                      c("ht.1","ht.2")),
                       v.names = c("dbh","height"),
                       timevar = "time",
                       idvar = "tree"
                       ))
# direction tells R to go wide or go long,
# varying   is a list of vectors of column names that are to be stacked,
# v.names   is a vector of the new names for the stacked columns,
# timevar   is the name of the new column that differentiates between successive
#           measure-ments on each object, and
# idvar     is the name of the existing column that differentiates between 
#           the objects.

###################################################
### Sorting
###################################################
ufc[order(ufc$height.m, decreasing=TRUE),][1:5,]

ufc[order(ufc$plot, ufc$species, ufc$height.m),][1:5,]

