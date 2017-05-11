# Stochastic adjacency matrix used in the PageRank algorithm by Google

stoch.adj.mat <- function(M){
  if(nrow(M) == ncol(M)){
    # by summing the outlinks we get the weights vector
    get.weight.byrow <- function(x) {
      x[x==1] <- 1/sum(x)
      return(x)
    }
    M.adj <- t(apply(A,1,get.weight.byrow))
    M.eig <- eigen(M.adj)
    return(list(stoch.matrix = M.adj, eigenvalues = M.eig$values, eigenvectors = M.eig$vectors)) 
  } else {
    print("Error: Not square matrix.")
  }
}


A = matrix(c(0,0,1,0,0,1,1,0,1,1,0,0,0,0,0,1,1,1,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0),6,6,byrow = T)

stoch.adj.mat(A)