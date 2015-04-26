## My functions solve for the inverse of a matrix and cache the results, allowing
## R to return the results faster once the results have been cached.
##

## This function makes a list of four functions:
##
## *the first function sets the value of the matrix
## *the second function gets the value of the matrix (only content is)
## *the third function solves the matrix, and sets the value in cache
## *the fourth function gets the value of the solved matrix (the inverse)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
## This function tests if the inverse of the matrix is cached, and will either 
## return the solved value from cache or calculate the solution and return it.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
        ## The first time CacheSolve is run it calculates the inverse of a matrix.
        ## Subsequent runs will print the message "getting cached data" and pull the 
        ## inverse from the cache.
