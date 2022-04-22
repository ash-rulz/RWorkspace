#To create a vector, use the combine function
MyFirstVector <- c(1,5,3,6)
MyFirstVector

is.numeric(MyFirstVector)
is.integer(MyFirstVector)#Double

IntVector <- c(1L, 5L, 3L, 6L)
is.integer(IntVector)

#Within a vector, we can find only 1 type
CombVector <- c(1L,5L,3L,"Hello")
CombVector #Converted into a char vector

#Generate a sequence
seq(1,10,2)

#Replicate 3 ten times
rep(3,10)

#Accessing elements in a vector
MyFirstVector[2]
MyFirstVector[-1]#Gives everything except 1st element
MyFirstVector[-3]
MyFirstVector[1:3]
MyFirstVector[c(1,2,3)]#Same as above
MyFirstVector[c(-2,-3)]

#Loops in R with vector
x=rnorm(10)
x

for (i in x) {
  print(i)
}

#Adding 2 vectors
N <- 50
v1 <- rnorm(N)
v2 <- rnorm(N)
v_add = v1 + v2
v_add

#Function
?rnorm() #To get help
