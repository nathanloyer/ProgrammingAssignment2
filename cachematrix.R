## Usage:
## x = makeCacheMatrix(matrix(sample(1000000),1000))
##
## inv = cacheSolve(x)
##    - actually calculated
## inv = cacheSolve(x)
##    - retreived from cache

## description:
##    this function returns a cacheMatrix object with the methods
##    get() and cacheSolve()
## input:
##    matrix object
## output:
##    cacheMatrix Object
makeCacheMatrix <- function(cachedMatrix = matrix()) {
  
  cachedInv <- NULL
  
  ## return the matrix
  get <- function() {
    cachedMatrix
  }
  
  ## tests to see if we have an inverted matrix cached, then returns it
  ## or computes the inverted matrix then returns that
  cacheSolve <- function() {
    if(!is.null(cachedInv)) {
      message("getting cached data")
      return(cachedInv)
    }    
    cachedInv <<- solve(cachedMatrix)
    cachedInv
  }
  
  # make our object it's own class with methods and return it
  self = list(get = get, cacheSolve = cacheSolve)
  class(self) <- c('cacheMatrix', 'list', 'matrix')
  self
}

## wrapper around the nested cacheSolve method, just to satisy
## the parameters of the assignment
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  x$cacheSolve()
}

## this overloads the default print for our object, cacheMatrix, 
## so it looks like a normal matrix when you print it
print.cacheMatrix <- function(x) {
  print(x$get())
}
