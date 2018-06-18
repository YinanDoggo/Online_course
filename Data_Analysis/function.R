add2 <- function(x,y){
  x+y
  }

above10 <- function(x) {
        use <- x>10
        x[use]
}

above <- function(x,n=10) {
  use <- x>n
  x[use]
}

columnmean <- function(y, removeNA = TRUE) {
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc) {
    means[i] <- mean(y[,i], na.rm = removeNA)
  }
  all = list(nc,means)
  all
}

f <- function(a,b){
  a
  b
}

f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}

y <- 10
f <- function(x) {
  y <- 2
  y^2 + g(x)
}
g <- function(x) {
  x*y
}