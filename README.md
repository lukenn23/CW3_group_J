# CW3_group_J

a <- "z"
b <- -7
c <- 2

quadratics_solver <- function(a, b, c){
  cat("This function solves quadratic equations of the form ax^2+bx+c, where a is not equal to 0.\n")
  
  if(missing(a) | missing(b) | missing(c) | 
     !is.numeric(a) | !is.numeric(b) | is.numeric(c))
  {
    cat("Wrong input. Enter numeric values only!\n")
    return(NULL)
  } 
  
  if(a==0)
  {
    cat("In a quadratic equation 'a' cannot be 0!")
    return(NULL)
  }

  cat("The provided coefficients give the equation: ", a, "x^2 +", b, "x +", c, ".\n")
  D <- b^2-4*a*c
  
  if(D>0)
  {
    x1 <- (-b+sqrt(D))/(2*a)
    x2 <- (-b-sqrt(D))/(2*a)
    cat("The quadratic equation has two real solutions: x1 =", x1, ", x2 =", x2, ".\n")
  }
  else if(D==0)
  {
    x <- -b/(2*a)
    cat("The quadratic equation has one real solution: x =", x, ".\n")
  }
  else
  {
    cat("The equation has no real solutions!")
  }
}
quadratics_solver(a, b, c)
