above10 <- function(x)
{
  
  use <- x>10
  x[use]
}

columns <- function(y){
  nc<- ncol(y)
  means<-numeric(nc)
  for(i in 1:nc){
    
    means[i] <- mean(y[,i])
    
  }
  means
}


f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}

x <- 5
y <- if(x < 3) {
  NA
} else {
  10
}


h <- function(x, y = NULL, d = 3L) {
  z <- cbind(x, d)
  if(!is.null(y))
    z <- z + y
  else
    z <- z + f
  g <- x + y / z
  if(d == 3L)
    return(g)
  g <- g + 10
  g
}