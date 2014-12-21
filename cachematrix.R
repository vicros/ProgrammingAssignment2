## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(X = matrix()) {
  M <- NULL
  set <- function(Y) {
    X <<- Y
    M <<- NULL
  }
  get <- function() X
  setInverse <- function(Inv) M <<- Inv
  getInverse <- function() M
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
cacheSolve <- function(X, ...) {
  ## Return a matrix that is the inverse of 'X'
  M <- X$getInverse()
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  data <- X$get()
  M <- solve(data, ...)
  X$setInverse(M)
  M
}
