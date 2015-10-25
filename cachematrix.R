## This is a set of two functions that will take an invertible
## matrix object and see if the inverse has been solved
## if the inverse has been solved and cached, then the inverted
## matrix is returned.  If the inverse has not been solved
## and cached, then cacheSolve will solve and cache the value
## of the inverted matrix. 

#small dummy matrix for testing:
#y <- matrix(1:4, 2, 2)

## Write a short comment describing this function:
## This function takes a matrix object and get and sets
## the value of the matrix and it's inverse.  Returns
## a list of values that set and get the inverse of the
## matrix object x, if it exists.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve checks to see if the inverse of the matrix is cached
## and if it is, then returns the cached value.
## if the inverse is not calculated, cacheSolve will 
## calculate the inverse and cache the value for future use

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
