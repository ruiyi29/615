myName <- "Ruiyi Feng"

#1
v1 <- c(1:20)

v2 <- c(20:1)

v3 <- seq(1,19,by=2)
 
v4 <- rep(c(3,7,11),times=10)

q <- rep(c(3,7,11),times=11)
v <- head(q,-1)
v5 <- head(v,-1)
  
#2
g <- seq(3,6,by=0.1)
x1 <- exp(g)*sin(g)

#3
i <- seq(10,100,by=1)
sum1 <- sum(i^3+4*i^2)

#4
str1 <- paste("label ", 1:30,sep="")

str2 <- paste("function",1:30,sep="")

#5
vs <- c(1,'function',NA, seq(1,5,2), 0.125)

#6
A <- matrix(data = c(1:9),nrow=3,ncol=3)
A

m1_ans <- A  %*% A  %*% A
m1_ans

#7
B <- matrix(data=rep(c(12,-12,12),times=17),nrow=17,ncol=3)
B

m2_ans <- t(B) %*% B
m2_ans

#8
D <- rbind(c(1,2,3,4,5),c(2,1,2,3,4),c(3,2,1,2,3),c(4,3,2,1,2),c(5,4,3,2,1))
E <- c(7,-1,-3,5,17)
m3_ans <- solve(D,E)

#9
xv <- seq(0,1,by=0.1)
func1_ans <- seq()

#10
cel_to_far <- function(t1){t1*(9/5)+32}
far_to_cel <- function(t2){(t2-32)*(5/9)}

#11
odd <- function(p) p[p %% 2 == 1]
odd_ans <- odd(1:2000)

#12
function12 <- 

#13
modNumber(x,y) <- function(x,y){
  if (x %% y == 0) {
    return(x)
  } else {
    return(x %% y +1)*y
  }
}
 

#14
numberOfWheels <- switch("unicycle","unicycle"="1","bike"="2","car"="4","truck"="16","tricycle"="3","motorcycle"="2")

#15
myFactorial <- function(n){factorial(n)}

#16
myCustomFactorial <- function(x,y){(x+y)*(y-x+1)/2}

#17
customRiverMean <- 

#18
  
longTeeth <- 

#19


#20



