name <- "Aswath Mandakath Gopinath"
liuid <- "aswma317"

# 1.1 Vectors
## 1.1.1 my_num_vector()
options(digits = 22)
my_num_vector <- function(){
  return(c(log(11, base = 10), cos(pi/5),
           exp(pi/3), (1173%%7)/19))
}

v1 <- my_num_vector()
v1

## 1.1.2 filter_my_vector()
filter_my_vector <- function(x, leq){
  return(replace(x, x >= leq, NA))
}

x <- c(2,9,2,4,102)
leq <- 4
filter_my_vector(x,leq)

## 1.1.3 dot_prod(a, b)
dot_prod <- function(a, b){
  c <- as.vector(a%*%b)
  return(c)
}

a <- c(5:10)
b <- c(20:25)
c <- dot_prod(a,b)
c
#is.atomic(c)

## 1.1.4 approx_e(N)
approx_e <- function(N){
  e_vect <- 1
  for (i in 1:N) {
    if (i==1) {
      e_vect <- c(e_vect, 1)
    }else{
      e_vect <- c(e_vect,(1/factorial(i)))
    }
  }
  return(sum(e_vect))
}

for (i in 1:20) {
  e_vect <- c(e_vect, approx_e(i))
}
e_vect
#For N = 8, the constant e comes to 2.71827
#Starting from N = 9, the value becomes 2.71828 and it remains the same for higher N values

# 1.2 Matrices
## 1.2.1 my_magic_matrix()
my_magic_matrix <- function(){
  magic_mat <- cbind(c(4,3,8), c(9,5,1), c(2,7,6))
  return(magic_mat)
}

magic_mat <- my_magic_matrix()
magic_mat

## 1.2.2 calculate_elements()
calculate_elements <- function(A){
  return(length(A))
}
new_mat <- cbind(magic_mat, magic_mat)
calculate_elements(new_mat)

## 1.2.3 row_to_zero()
row_to_zero <- function(A, i){
  new_mat <- A
  new_mat[i,] <- 0
  return(new_mat)
}

new_mat <- row_to_zero(magic_mat, 1)
new_mat


## 1.2.4 add_elements_to_matrix
add_elements_to_matrix <- function(A, x, i, j){
  A[i,j] = A[i,j] + x
  return(A)
}

add_mat <- add_elements_to_matrix(magic_mat, 10, 2 , 3)
add_mat

# 1.3 Lists
## 1.3.1 my_magic_list()
my_magic_list <- function(){
  return(list(info="my own list", my_num_vector(), my_magic_matrix()))
  
}
magic_list <- my_magic_list()
magic_list

## 1.3.2 change_info()
change_info <- function(x, text){
  x$info <- text
  return(x)
}
a_list <- change_info(magic_list, "Some new info")
a_list

## 1.3.3 add_note()
add_note <- function(x, note){
  x <- c(x, note = note)
  return(x)
}
b_list <- add_note(a_list, "This is a magic list!")
b_list

## 1.3.4 sum_numeric_parts
sum_numeric_parts <- function(x){
  return(sum(as.numeric(unlist(x)), na.rm = TRUE))
}
sum_numeric_parts(b_list)

# 1.4 data.frames
## 1.4.1 my_data.frame()
my_data.frame <- function(){
  my_df <- data.frame(
    id = c(1,2,3),
    name = c("John", "Lisa", "Azra"),
    income = c(7.30, 0.00, 15.21),
    rich = c(FALSE, FALSE, TRUE)
  )
  return(my_df)
}
my_data.frame()

## 1.4.2 sort_head()
sort_head <- function(df, var.name, n){
  ordered_vect <- order(df[,var.name], decreasing = TRUE)
  df <- df[ordered_vect,]
  return(head(df, n))
}
sort_head(df = iris, var.name = "Petal.Length", n = 5)

## 1.4.3 add_median_variable()
add_median_variable <- function(df, j){
  #med <- apply(df, 2, median, na.rm = TRUE)[j]
  col_vect <- as.numeric(df[,j])
  med <- median(col_vect, na.rm = TRUE)
  #print(med)
  # df <- cbind(df,apply(df, 1, f, j, med))
  df <- cbind(df,apply(df, 1, function(x) if(x[j]>med) "Greater" else if(x[j]<med) "Smaller" else "Median"))
  colnames(df)[ncol(df)] <- "compared_to_median"
  return(df)
}
f <- function(x, j, med) if(x[j]>med) "Greater" else if(x[j]<med) "Smaller" else "Median"
add_median_variable(faithful,2)
data("ChickWeight")
df1 <- ChickWeight
df1
add_median_variable(df1,1)

## 1.4.4 analyze_columns()
analyze_columns <- function(df, j){
  temp_i <- 1L
  for(i in j){ #Sub-optimal
    col_vect <- as.numeric(df[,i])
    mean_val <- mean(col_vect)
    median_val <- median(col_vect)
    sd_val <- sd(col_vect)
    v <- c(mean = mean_val, median = median_val, sd = sd_val)
    if(temp_i == 1){
      first_v <- v #This is the first vector to be supplied to the list.
      temp_i <- temp_i + 1
    }
    else{
      sec_v <- v  #This is the second vector to be supplied to the list.
    }
  }
  #Create the correlation matrix
  mat <- cor(df[,j])
  #Here we create the final list
  fin_lst <- list(first_v, sec_v, mat)
  names(fin_lst) <- colnames(df)[j]
  names(fin_lst)[3] <- "correlation_matrix"
  return(fin_lst)
}
analyze_columns(faithful,1:2)
