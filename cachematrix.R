## Put comments here that give an overall description of what your
## functions do

#small dummy matrix for testing:
#y <- matrix(1:4, 2, 2)

## Write a short comment describing this function:
## This function takes a matrix object and get and sets
## the value of the matrix and it's inverse

#9/25: dropped in cache mean....
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve checks to see if the inverse of the matrix is cached
## and if it is, then solve using the cached value
## if the inverse is not calculated, calculate the inverse
## and cache the value for future use

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
