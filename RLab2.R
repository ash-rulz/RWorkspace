name <- "Aswath Mandakath Gopinath"
liuid <- "aswma317"

# 1.1 Conditional statments
## 1.1.1 sheldon_game()
calculate_result <- function(p1, p2, high_vect){
  #If both player 1 and player 2 has the same, then it is a draw
  #If P2 has chosen an element from the higher vect, then player 2 wins
  #Otherwise its a draw
  if(p1 == p2) {result <- "Draw!"}
  else if(p2 %in% high_vect) {result <- "Player 2 wins!"}
  else {result <- "Player 1 wins!"}
  return(result)
}
sheldon_game <- function(player1, player2){
  # Initialization
  all_vect <- c("rock", "paper", "scissors", "lizard","spock")
  #Get all the combinations that can beat the said choice
  rock_high_vect <- c("paper", "scissors", "spock")
  liz_high_vect <- c("scissors", "rock")
  spock_high_vect <- c("lizard")
  sci_high_vect <- c("spock")
  paper_high_vect <- c("scissors", "lizard")
  result <- NULL
  
  if((player1 %in% all_vect) && (player2 %in% all_vect)){
    if(player1 == "rock"){
      result <- calculate_result(player1, player2, rock_high_vect)
    }
    else if(player1 == "paper"){
      result <- calculate_result(player1, player2, paper_high_vect)
    }
    else if(player1 == "scissors"){
      result <- calculate_result(player1, player2, sci_high_vect)
    }
    else if(player1 == "lizard"){
      result <- calculate_result(player1, player2, liz_high_vect)
    }
    else if(player1 == "spock"){
      result <- calculate_result(player1, player2, spock_high_vect)
    }
  }
  else{
    stop("The input values are not valid")
  }
  return(result)
}

sheldon_game("lizard", "spock")

# 1.2 for loops
## 1.2.1 my_moving_median
my_moving_median <- function(x, n, ...){
  #Check if the inputs are valid
  na_rm <- list(...)
  if(!is.atomic(x)) stop("Not a vector") #Check if the vector is atomic
  if(!is.numeric(n)) stop("Not integer") #Check if n is numeric
  
  median_vect <- NULL
  for(i in seq_along(x)){
    if((i+n) > length(x)) break
    vect_sub <- x[i:(i+n)]
    if(length(na_rm)==0){#Can use missing() as well
      median_vect <- c(median_vect, median(vect_sub))
    }else{
      median_vect <- c(median_vect, median(vect_sub, na.rm = na_rm[[1]]))
    }
  }
  return(median_vect)
}
my_moving_median(x = c(5,1,2,NA,2,5,6,8,9,9), n=2)


## 1.2.2 for_mult_table
for_mult_table <- function(from, to){
  if(!is.numeric(from) || !is.numeric(to)) stop("Inputs not valid")
  vect <- seq.int(from,to,1)
  mat <- NULL
  for(i in vect){
    mat <- cbind(mat,i*vect)
  }
  rownames(mat) <- vect
  colnames(mat) <- vect
  return(mat)
  # col_vect <- c(from:to)
  # row_vect <- c(from:to)
  # return(outer(col_vect, row_vect,'*'))
}
for_mult_table(1,3)

# 1.3 While loops
## 1.3.1 find_cumsum
find_cumsum <- function(x, find_sum){
  if(!is.atomic(x)) stop("Invalid input")
  
  cum_sum <- 0L
  i = 1L
  while ((i<= length(x)) && (cum_sum <= find_sum)) {
    cum_sum = cum_sum + x[i]
    i = i + 1
  }
  return(cum_sum)
}
find_cumsum(1:10, 1000)

## 1.3.2 while_mult_table()
while_mult_table <- function(from, to){
  row_index <- 1L
  col_index <- 1L
  mat_dim <- (to-from)+1
  vect <- c(from:to)
  mat <- matrix(NA, nrow = mat_dim, ncol = mat_dim)
  
  while(row_index <= mat_dim){
    while(col_index <= mat_dim){
      mat[row_index,col_index] <- vect[row_index] * vect[col_index]
      col_index <- col_index + 1
    }
    col_index <- 1L
    row_index <- row_index + 1
  }
  rownames(mat) <- from:to
  colnames(mat) <- from:to
  return(mat)
}
while_mult(7,12)

# 1.4 repeat and loop controls
## 1.4.1 repeat_find_cumsum()
repeat_find_cumsum <- function(x, find_sum){
  if(!is.atomic(x)) stop("Invalid input")
  
  cum_sum <- 0L
  i = 1L
  repeat{
    if((i > length(x)) || (cum_sum > find_sum)) break
    cum_sum = cum_sum + x[i]
    i = i + 1
  }
  return(as.numeric(cum_sum))
}
repeat_find_cumsum(x=1:100, find_sum=500)
repeat_find_cumsum(x=1:10, find_sum=1000)

## 1.4.2 repeat_my_moving_median()
repeat_my_moving_median <- function(x, n, ...){
  #Check if the inputs are valid
  if(!is.atomic(x)) stop("Not a vector") #Check if the vector is atomic
  if(!is.numeric(n)) stop("Not integer") #Check if n is numeric
  
  #Initialization
  median_vect <- NULL
  na_rm <- list(...)
  i <- 1L
  
  repeat{
    if((i+n) > length(x)) break
    vect_sub <- x[i:(i+n)]
    if(length(na_rm)==0){#Can use missing() as well
      median_vect <- c(median_vect, median(vect_sub))
    }else{
      median_vect <- c(median_vect, median(vect_sub, na.rm = na_rm[[1]]))
    }
    i <- i + 1
  }
  return(median_vect)
}
repeat_my_moving_median(x = 1:10, n=2)
repeat_my_moving_median(x = 5:15, n=4)
repeat_my_moving_median(x = c(5,1,2,NA,2,5,6,8,9,9), n=2)
#1.5 Environment
## in_environment()
in_environment <- function(env){
  return(ls(env, all.names = TRUE))
}

env <- search()[length(search())]
env
funs <- in_environment(env)
funs[1:5]

#1.6 Functionals
## 1.6.1 cov()
cov <- function(X){
  if(is.null(X) || !is.data.frame(X)) stop("Invalid Input")
  return(unlist(lapply(X, function(y) (sd(y)/mean(y)))))
}
cov(X = iris[1:4])
is.data.frame(iris[1:4])

# 1.7 Closure
## 1.7.1 moment()
moment <- function(i){
  if(!is.numeric(i)) stop("Invalid input")
  function(x){#x is the vector
    return(sum(((x - mean(x)))^i/length(x)))
  }
}
m1 <- moment(1)
m2 <- moment(2)
m1(1:100)
m2(1:100)
