## Defines special wrapper object around matrix that caches inverse form of the matrix.
## Implements efficient matrix inversion that uses cached matrix economizing on computations

## Provides mechanism for accessing and storing/caching matrix and its inverse form
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv) i <<- inv
  
  getInverse <- function() i
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of a square matrix or returns cached inverse matrix if it's available
cacheSolve <- function(x, ...) {

  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}


###############################################################
## UNIT TEST
testCacheSolve <- function(){
  
  hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
  h8 <- hilbert(8)
  
  cacheSolve(makeCacheMatrix(h8))
}
