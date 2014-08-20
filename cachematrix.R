## This file contains a pair of functions that can be used to compute
## and cache the inverse of a matrix.


## This function builds a container for the given matrix.
##
## Input arguments:
##    x: a square numeric or complex matrix
##
## Return value: a list object that encapsulates the matrix
##
## Example usage:
##    my_matrix <- makeCacheMatrix(matrix(1:4,2))
##
makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse of the matrix to be NULL (i.e. not yet calculated)
  i <- NULL
  
  # create a function to set/change the value of the matrix
  set <- function(y) {
    # the new value of the matrix is y
    x <<- y
    
    # reset the inverse to NULL (i.e. not yet calculated)
    i <<- NULL
  }
  
  # create a function to get the value of the matrix
  get <- function() x
  
  # create a function to set the value of the inverse of the matrix
  setinv <- function(inv) i <<- inv
  
  # create a function to get the value of the matrix
  getinv <- function() i
  
  # return a list containing the above accessor functions
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the given matrix.
##
## Input arguments:
##    x: a list object of the type returned by makeCacheMatrix
##
## Return value: the inverse of the matrix encapsulated in x
##
## Example usage:
##    my_matrix <- makeCacheMatrix(matrix(1:4,2))
##    my_inverse <- cacheSolve(my_matrix)
##
cacheSolve <- function(x, ...) {
  # get cached value of the inverse
  i <- x$getinv()

  # if the inverse has already been computed...
  if (!is.null(i)) {
    # return the cached value
    message("getting cached data")
    return(i)
  }

  # the inverse has not yet been computed, so we'll solve it
  data <- x$get()
  i <- solve(data, ...)

  # save the value, so that future calls to the function won't need
  # to recompute it
  x$setinv(i)
  i
}
