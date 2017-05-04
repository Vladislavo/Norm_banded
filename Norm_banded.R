# Description
#  Computes the norm of the banded matrix using only the band
#
# Usage
#  Norm.banded <- function(band, x, b, nup=0, nlow=0, norm=2)
#
# Arguments
#  band
#   a matrix containing the (nonzero) bands or a vector with the main diagonal
#  x
#   the solution vector for the matrix represented by the band argument
#  b	
#   right-hand side of the equations, a vector with length = number of rows of A,
#   or a matrix with number of rows = number of rows of A.
#  nup	
#   number of nonzero bands above the diagonal; default is 0
#  nlow	
#   number of nonzero bands below the diagonal; default is 0
#  norm
#   numeric scalar or Inf, -Inf; default is 2
#
# Details
#  Norm.banded returns a scalar that gives some measure of the magnitude of 
#  the elements of Ax-b (residual value), where A represented by the band
#
# Value
#  Numeric scalar (or Inf), or NA if an element of x is NA.

Norm.banded <- function(band, x, b, nup=0, nlow=0, norm=2) {
  if(!is.matrix(band)) as.matrix(band)
  
  n <- length(x)
  aux <- matrix(1,n, data=rep(0,n))
  
  for(i in seq_len(nrow(band))){
    # superior band
    if(i <= nup){
      # take the necessary part of band[i,]*x and shift it left adding zeros
      aux <- aux + c((band[i,] * x)[(nup+2-i):n],rep(0,nup+1-i))
    } else { # further from main diagonal
      # take the necessary part of band[i,]*x and shift it right adding zeros
      aux <- aux + c(rep(0,i-(nup+1)),(band[i,] * x)[1:(n+1-i+nup)])
    }
  }
  residuo <- aux - b
  return(Norm(residuo, norm))
}



# Checking
diag.p <- 1:30
sup1 <- seq(5,145,5)
inf1 <- 35:63

# first with a sparse matrix
A <- spMatrix(30,30,1:30,1:30,diag.p)
A <- A + spMatrix(30,30,2:30,1:29,inf1)
A <- A + spMatrix(30,30,1:29,2:30,sup1)

b <- seq(10,217,7)

x <- solve(A,b)
Norm(as.vector(A%*%x) - b, 2)
# > Norm(as.vector(A%*%x) - b, 2)
# [1] 4.593866e-13

# then with Solve.banded
band <- rbind(c(0,sup1), diag.p, c(inf1,0))

x <- Solve.banded(band, nup = 1, nlow = 1, b)

Norm.banded(band, x, b, nup=1, nlow=1, norm=2)
# > Norm.banded(band, x, 1, 1, b, 2)
# [1] 2.064295e-12