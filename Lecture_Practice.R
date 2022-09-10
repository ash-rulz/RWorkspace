# Lecture 1 - Day 1

x <- c(1,2,3,4)
x

all(x < 5)#Checks if all the values in the vector satisfies the condition
any(x > 5)#Checks if any value satisfies the condition
which(x == 2)

#To check equality
y <- sqrt(2)
(y * y) == 2 #Not identical
isTRUE(all.equal(y*y,2)) #Need to wrap it in isTRUE else, when it is false, it gives some weird o/p
identical(2.0, 2)

#Vector
vect <- c(6,7,8,9)
q <- c(x,x,8)
q #x keeps repeating

z <- c(5,2,-3,8)
z[which(z*z > 8)]

vect < 8 #Returns True/False for each element in vector
vect[vect < 8] #Returns the element in the vector which is true

which(vect < 8) #Returns the index
vect[which(vect < 8)]

vect[c(1,3)] #Chooses the 1st and 3rd element

vect_red = vect[c(-1,-3)]
vect_red

x <- 1:5
x[c(1, 2)] <- 2:3
x
x[-1] <- 4:1
x

#Matrices
mat <- matrix(c(1,2,3,4,5,6), nrow=2)
mat
mat[c(1,2), c(2,3)] #Select rows 1 & 2 and columns 2 & 3
mat[mat>4]
mat[mat>4] <- 75 #Assigning values to certain elements which satisfies some condition
mat

is.matrix(mat)
is.array(mat)

str(mat[1,])

#List
lst <- list(a=47, b=11)
lst
lst[1] #Displays both the name and the value
lst[[1]] #Displays only the value

z <- list(a="abc",b=12)
z$c <- 1
z
z[1] <- NULL
names(z)[1]

x <- "a"
lst[which(names(lst) == x)]

b <- list(a = list(b = list(c = list(d = 1))))
b

x <- list(1:3, 4:9, 10:12)
x
#Extracts the 2nd element from the list and converts it into a vector
sapply(x, "[", 2)

#Creating a list of length = random var 
#http://adv-r.had.co.nz/Functionals.html
l <- replicate(20, runif(sample(1:10, 1)), simplify = FALSE)
out <- vector("list", length(l))
str(l)
l[[1]]

#Factors
y <- factor(c("a","b","a","a","b"))
y
tab <- table(y) 
tab
as.vector(tab)

f1 <- factor(letters)
f1
levels(f1) <- rev(levels(f1))
f1

f2 <- rev(factor(letters))
f2

f3 <- factor(letters, levels = rev(letters))
f3

#is.vector, is.atomic, is.list
x <- c(a = 1, b = 2)
is.vector(x)
attributes(x)

y <- list(c("GFG", "gfg"), matrix(c(1, 2, 3)))
y
attributes(y)
is.vector(y)

z <- "Geeks"
z
attributes(z)
is.vector(z)

is.atomic(x)
is.atomic(y)
is.atomic(z)

is.list(x)
is.list(y)
is.list(z)

#Character converted to integer
as.integer("one") #Creates NA

#NA in logical vector
log_vect <- c(TRUE, FALSE, TRUE, NA)
log_vect
sum(log_vect)
is.logical(log_vect)

int_vect <- c(1L,2L,3L,NA)
int_vect
is.integer(int_vect)
typeof(int_vect)
class(int_vect)

#Array
b <- array(1:12, c(2,3,2))
b

x1 <- array(1:5, c(1, 1, 5))
x2 <- array(1:5, c(1, 5, 1))
x3 <- array(1:5, c(5, 1, 1))

x1
x2
x3

#Data Frame
df <- data.frame(x = 1:3, y = c("a", "b", "c"))
df
length(df)
str(df)
df$x<2 #Returns boolean which can be used to access the elements

df <- data.frame(x = 1:3)
df
df$y <- list(1:2, 1:3, 1:4)
df
nrow(df)
ncol(df)
str(df)

#Sorting the data frame
iris
index_v <- order(iris$Petal.Length)
index_v
iris[index_v, ]

#Joining vector with a data frame
grade <- c(1,2,2,3,1)
info <- data.frame(
  grade = 1:3,
  desc = c("Excellent", "Good", "Poor"),
  fail = c(F,F,T)
)
id <- match(grade, info$grade)
info[id,]

#Function
u <- 1
testFunc <- function(x){
  x <- x + 1
  u <- u + x
  return(u)
}
testFunc(8)

#Name masking
f <- function() {
  x <- 1
  y <- 2
  c(x, y)
}
f()
rm(f)

#Closure
j <- function(x) {
  y <- 2
  function() {
    c(x, y)
  }
}
k <- j(1)
k()

#Closure example
power <- function(exp1){
  function(x){
    x^exp1
  }
}
sqr <- power(2)
sqr(3)
cube <- power(3)
cube(4)

rm(a)
#A fresh start
j <- function() {
  if (!exists("a")) {
    a <- 1
  } else {
    a <- a + 1
  }
  return(a)
}
j()

# Dynamic Lookup
f <- function() {
  return(x)
}
x <- 20
f()
codetools::findGlobals(f)

dotFunctionTest <- function(...){
  lst <- list(...)
  print(length(lst))
}
dotFunctionTest(1)


#Functionals
mtcars
str(mtcars)
mt_new_car = NULL
mt_new_car[] <- lapply(mtcars, function(x) x / mean(x))
mt_new_car
mtcars[] <- lapply(mtcars, function(x) x / mean(x))
mtcars
?lapply

mtmeans <- lapply(mtcars, mean)
mtmeans
mtmeans[] <- Map('/', mtcars, mtmeans)
mtmeans[1]
mtmeans_new <- Map('/', mtcars, mtmeans)
mtmeans_new[1]

sapply(mtcars, is.numeric)

fun <- c(mean, median)
temp_iris <- lapply(iris[,1:3], function(x) c(mean(x), median(x)))
temp_iris

#Misc
replicate(5, runif(10), simplify = FALSE) #To get a List of 5 with random sample
replicate(5, runif(10)) #To get a Matrix of 5 cols with random sample

pulse <- round(rnorm(22, 70, 10 / 3)) + rep(c(0, 5), c(10, 12))
pulse
group <- rep(c("A", "B"), c(10, 12))
group

trans <- list(
  disp = function(x) x * 0.0163871,
  am = function(x) factor(x, labels = c("auto", "manual"))
)
trans
mtcars
#Loop through the list which contains the functions and apply the function to the 
#matrix. Apply to the 2 columns in the matix.
for(var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}
mtcars

?do.call

f <- function(...){
  print(missing(...))
}
f(123)
?ls

h <- function() {
  x <- 10
  function() {
    x
  }
}
i <- h()
x <- 20
i()

x <- runif(1)
x
x %<a-% runif(1) #Recomputed everytime x is run


i <- 0
new_counter2 <- function() {
  i <<- i + 1
  i
}
i
new_counter2()
new_counter2()
i
new_counter3 <- function() {
  i <- 0
  function() {
    i <- i + 1
    i
  }
}
counter3fun <- new_counter3()
counter3fun()
i
