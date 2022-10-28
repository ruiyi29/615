myName <- "Ruiyi Feng"

library(magrittr)
library(readr)
library(tidyr)
library(dplyr)

# Warm Up
# 1
print_order <- function(x){
  y <- c()  
  max <- max(x)
  min <- min(x)
  for (i in 1:3){
    if (x[i] == max){y[1] = x[i]}
    else if (x[i] == min){y[3] = x[i]}
    else{y[2] = x[i]}
  }
  return(y)
}

# 2
print_string <- function(x){
  for(i in 1:x){
    y <- ifelse(i %% 3 != 0 & i %% 5 != 0,i,ifelse(i %% 3 == 0 & i %% 5 != 0,"YES",ifelse(i %% 3 != 0 & i %% 5 == 0,"NO","UNKNOWN")))
    print(y)
  }
}

# 3
calc_sum_of_factor <- function(x){
  j <- 1
  y <- c()
  for(i in 1:x){
    if(x %% i == 0){
      y[j]=i
      j <- j+1
    }
  }
  print(sum(y^2))
}

# 4
intersect <- function(a,b){
  a <- a[!duplicated(a)]
  b <- b[!duplicated(b)]
  ab <- append(a,b)
  logic <- duplicated(ab)
  index <- which(logic == TRUE )
  ans <- ab[index]
  return(ans[!duplicated(ans)])
  }

find_intersect <- function(a,b,c){
  d <- intersect(a,b)
  e <- intersect(d,c)
  return(e)
}

# 5
factorial_base <- function(x) {
  if (x == 0) {
    result <- 1
  } else {
    result <- x
    while(x > 1){
      result <- (x - 1) * result
      x <- x - 1
    }
  }
  return(result)
}

# 6 
T <- function(n){n*(n+1)/2}
perfect_sqr <- function(x){y <- ifelse(sqrt(x)==trunc(sqrt(x)),"TRUE","FALSE")}
num_tri_sqr <- function(n){
  j <- 1
  y <- c()
  for(i in 1:n){
    if(sqrt(i*(i+1)/2)==trunc(sqrt(i*(i+1)/2))){
      y[j] <- i*(i+1)/2
      j <- j+1
    }
  }
}
num_tri_sqr_1 <- function(n){
  sum <- 0
  for(i in 1:n){
    if(sqrt(i*(i+1)/2)==trunc(sqrt(i*(i+1)/2))){
      sum <- sum + (i*(i+1))/2
    }
  }
  print(sum)
}
q6_sum <- num_tri_sqr_1(1500000)

# 2022 H-1B Employer Data Hub
# 1
h1b_2022 <- read_csv("https://www.uscis.gov/sites/default/files/document/data/h1b_datahubexport-2022.csv")

# 3
na_num <- sum(is.na(h1b_2022))
h1b_2022a <- h1b_2022%>%drop_na()
h1b_2022a <- h1b_2022a[!h1b_2022a$State=="-",]
h1b_2022a <- h1b_2022a[!h1b_2022a$City=="-",]

# 4
df_num <- aggregate(cbind(h1b_2022a$`Initial Approval`+h1b_2022a$`Initial Denial`,h1b_2022a$`Continuing Approval`+h1b_2022a$`Continuing Denial`,h1b_2022a$`Initial Approval`,h1b_2022a$`Initial Denial`),by=list(State=h1b_2022a$State),sum)
names(df_num) <- c("State","Init App","Conti App","Approve","Denial")
df_num <- as_tibble(df_num)

# 5
app_num <- sum(df_num$Approve)
den_num <- sum(df_num$Denial)

# 6
city_num <- as.data.frame(table(h1b_2022a$City))
names(city_num) <- c("City","Count")
city_num$City <- as.character(city_num$City)

# 7 
visa_num <- as.data.frame(table(h1b_2022a$NAICS))
names(visa_num) <- c("NAICS","Number")
visa_num$Percentage <- round(100*visa_num$Number/sum(visa_num$Number),3)
visa_num$NAICS <- as.numeric(levels(visa_num$NAICS)[visa_num$NAICS])

# Extra Bonus
non_integer_factorial <- function(x){x*gamma(x)}
 


