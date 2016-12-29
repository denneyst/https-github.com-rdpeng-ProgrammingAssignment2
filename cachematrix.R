## In week 3 of the R Programming Course in the Coursera Data Science Specialization
## my goal is to write a pair of funcitons that cache the inverse of a matrix :)

## The first function is creates a matrix object that is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }


ret <- function() x

set_inv <- function(inverse) inv <<- inverse
ret_inv <- function() inv
list(set = set, get = get, set_inv = set_inv, ret_inv = ret_inv)
}


## This function calculates the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated then cacheSolve will retrieve the inverse
## from the cache - if the matrix was not changed.

cacheSolve <- function(x, ...) {
  inv <- x$ret_inv()
  if(!is.null(inv)) {
    message("retrieving cached data")
    return(inv)
  }
  data <- x$ret()
  inv <- solve(data,...)
  x$set_inv(inv)
  inv
}
