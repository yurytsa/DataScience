loss_function <- function(y,pred) { 
    return( sum((y - pred)^2)/length(y) )
}

### The gradient descent requires two values: learning rate and number of iterations.

gradientDesc <- function(x, y, learn_rate=1e-6, max_iter=1e3) {
  #plot(x, y, col = "blue", pch = 20)
  conv_threshold <- 1e-4
  n <- length(y)
  m <- runif(1, 0, 1)
  c <- runif(1, 0, 1)
  yhat <- m * x + c
# sum((y - pred)^2)/length(y)
  MSE <- loss_function(y,yhat)
  converged = F
  iterations = 0
  while(converged == F) {
    ## Implement the gradient descent algorithm
    m_new <- m - learn_rate * ((1 / n) * (sum((yhat - y) * x)))
    c_new <- c - learn_rate * ((1 / n) * (sum(yhat - y)))
    m <- m_new
    c <- c_new
    yhat <- m * x + c
    MSE_new <- loss_function(y,yhat)
    if(MSE - MSE_new <= conv_threshold) {
      #abline(c, m) 
      converged = T
      print(paste("Optimal intercept:", c, "Optimal slope:", m))
      return(data.frame(w=m,b=c,loss=MSE_new))
    }
    iterations = iterations + 1
    if(iterations > max_iter) { 
      #abline(c, m) 
      converged = T
      print(paste("Optimal intercept:", c, "Optimal slope:", m))
      return(data.frame(w=m,b=c,loss=MSE_new))
    }
  }
}

x <- Loblolly$age
y <- Loblolly$height

## iterating with the learning rate
library(dplyr)
res <- NULL
learning_rate <- c(1e-1,1e-2,1e-5,1e-6)
number_iterations <- c(1e3,1e4,1e5,1e6,1e7)

res <- NULL
for (i in number_iterations) {
    for (lr in learning_rate) {
      gd <- gradientDesc(x, y, lr,i)
    res <- rbind(res, data.frame(cbind(learning_rate=lr,iter=i, gd)))
  }
}

res <- res %>% arrange(loss)
res

plot(res$loss ~ res$learning_rate, type="l",xlab="Learning rate", ylab="Loss (MSE)")
plot(log10(res$loss) ~ log10(res$learning_rate), type="l",xlab="Learning rate", ylab="Loss (MSE)")

plot(res$loss ~ res$num_iter, type="l",xlab="Number of iterations", ylab="Loss (MSE)")
plot(log10(res$loss) ~ log10(res$num_iter), type="l",xlab="Number of iterations", ylab="Loss (MSE)")
