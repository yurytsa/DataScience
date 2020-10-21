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


age <- datasets::Loblolly$age
height <- datasets::Loblolly$height

# Run the function 
gradientDesc(age, height, 0.00002, 2500000)



library(ggplot2)
plot(height ~ age, xlim=c(0,50), ylim=c(0,80))



mod <- lm(height ~ age)
mod$coefficients

## iterating with the learning rate
library(dplyr)
res <- NULL
learning_rate <- c(1e-1,5e-2,1e-2,5e-3,1e-3,5e-4,1e-4,5e-5,1e-5,5e-6,1e-6)
for (lr in learning_rate) {
  gd <- gradientDesc(age, height, lr, 2500000)
  res <- rbind(res, data.frame(cbind(learning_rate=lr, gd)))
}


options(repr.plot.width = 4, repr.plot.height = 3)
res <- res %>% arrange(learning_rate)
res
plot(res$loss ~ res$learning_rate, type="l",
     xlab="Learning rate", ylab="Loss (MSE)",
     xlim = rev(range(res$learning_rate)))
plot(log10(res$loss) ~ log10(res$learning_rate), type="l",
     xlab="Learning rate", ylab="Loss (MSE)",
     xlim = rev(range(log10(res$learning_rate))))


learning_rate <- c(1e-5,2e-5,3e-5,5e-5,6e-5,7e-5,8e-5,9e-5,1e-4,2e-4,3e-4,5e-4,6e-4,7e-4,8e-4,9e-4,1e-3)
learning_rate


res <- NULL
for (lr in learning_rate) {
  gd <- gradientDesc(mtcars$disp, mtcars$mpg, lr, 2500000)
  res <- rbind(res, data.frame(cbind(learning_rate=lr, gd)))
}


library(dplyr)
res <- res %>% arrange(learning_rate) 
res
options(repr.plot.width = 6, repr.plot.height = 4)
plot(res$loss ~ res$learning_rate, type="l",
     xlab="Learning rate", ylab="Loss (MSE)",
     xlim = rev(range(res$learning_rate)))
plot(log10(res$loss) ~ log10(res$learning_rate), type="l",
     xlab="Learning rate", ylab="Loss (MSE)",
     xlim = rev(range(log10(res$learning_rate))))



## iterating with the number of iterations
library(dplyr)
res <- NULL
number_iterations <- c(1e1,5e2,1e2,5e3,1e3,5e4,1e4,5e5,1e5,5e6,1e6,5e6,1e7)
for (iter in number_iterations) {
  gd <- gradientDesc(mtcars$disp, mtcars$mpg, 1e-5, iter)
  res <- rbind(res, data.frame(cbind(num_iter=iter, gd)))
}


res <- res %>% arrange(num_iter)
res
options(repr.plot.width = 6, repr.plot.height = 4)
plot(res$loss ~ res$num_iter, type="l",xlab="Number of iterations", ylab="Loss (MSE)")
plot(log10(res$loss) ~ log10(res$num_iter), type="l",xlab="Number of iterations", ylab="Loss (MSE)")




points <- datasets::Loblolly


plot(points$height ~ points$age, xlim=c(0,50), ylim=c(0,80))

lines <- data.frame(matrix(ncol = 4, nrow = 0))
names <- c("age1", "age2", "height1", "height2")
colnames(lines) <- names

for(i in seq_along(points$age)) {
  for(j in seq_along(points$age)) {
    if (points$age[i] != points$age[j] || points$height[i] != points$height[j]){
      lines <- lines %>% add_row(age1 = points$age[i], height1 = points$height[i], age2 = points$age[j], height2 = points$height[j])
    }
  }
}

lines$x2 <- ifelse(lines$age1 >= lines$age2 , lines$age1 , lines$age2) 
lines$x1 <- ifelse(lines$age1 < lines$age2 , lines$age2 , lines$age1) 
lines$y2 <- ifelse(lines$age1 >= lines$age2 , lines$height1 , lines$height2) 
lines$y1 <- ifelse(lines$age1 < lines$age2 , lines$height2 , lines$height1) 

lines$slope <- lines$y2 - lines$y1 / lines$x2 - lines$x1
slope = mean(lines$slope)


