## Put comments here that give an overall description of what your
## functions do

## This function creates a wrapper of the matrix x, which returns a list of functions:
## 1. set: set the value of the matrix
## 2. get: get the value of the matrix
## 3. setInverse: set the cached inverted matrix
## 4. getInverse: get the cached inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function calculates the inverse of a matrix created by makeCacheMatrix.
## It first tries to get the cahced inverse, and return it if it's not null.
## Otherwise, it calculates the inverse using solve() and cache it.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached inverse matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}