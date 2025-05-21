rm(list = ls())

target_f <- function(x){return( x+1/x )}
grid_x <- seq(0, 4, by = 0.1)
plot(x = grid_x, target_f(grid_x))

curr_x <- 3
step_length <- .5
h <- tol <- 1e-6
max_iter <- 1000
iter <- 0

GD <- function(target_f, x_ini, h, step_length,tol,max_iter){
  curr_x <- x_ini
  all_x <- rep(NA, max_iter)
  all_y <- rep(NA, max_iter)
  for (iter in 1:max_iter) {
    all_x[iter] <- curr_x
    all_y[iter] <- target_f(curr_x)
    der <- (target_f(curr_x+h) - target_f(curr_x-h))/(2*h)
    new_x <- curr_x - step_length*der
    if(abs(new_x - curr_x)<=tol){break}
    curr_x <- new_x
    if(iter == max_iter){warning("Max Iteration!")}
  }
  return(list("x"= na.omit(all_x), "y"=na.omit(all_y)))
}
results <- GD(target_f = target_f, x_ini = 3, h = 1e-5, step_length = 5,
   tol = 1e-6, max_iter = 1000)
plot(results$x, results$y, type = 'b')

