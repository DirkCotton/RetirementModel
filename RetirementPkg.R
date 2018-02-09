# Package of personal functions Dirk Cotton frequently used for retirment plan analysis

#################################
# Create a vector by duplicating (n times) row vector x.
#################################

rep.row <- function(x,n){matrix(rep(x,each=n),nrow=n)} # repeat row function
rep.col <- function(x,n){matrix(rep(x,each=n), ncol=n, byrow=TRUE)} # repeat col function

#################################
# cumprod function that works like MATLAB's cumprod
#################################

cumprod.M <- function(x) {  # convert to cumprod function in Matlab-style
  if (is.vector (x)) {
    return (cumprod(x))
  } else {
    return(apply(t(x),2,cumprod))
  }
}

#################################
# fast multlply of a vector times each row of a matrix
#################################

matTimesvec <- function (mat,vec) {
  return (t(t(mat) * vec))
}

#################################
# ones -- equivalent of MATLAB ones function
#################################

ones <- function (a,b){
  matrix(1,a,b)
}


