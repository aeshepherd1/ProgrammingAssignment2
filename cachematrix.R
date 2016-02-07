## The two functions below are used to find and cache the inverse matrix of a square
## matrix, provided the inverse matric can be calculated.  An error is returned if
## the inverse cannot be found.

## makeCacheMatrix includes four "get" and "set" functions.  The "set" functions
## populate values of local and parent environment variables based upon the matrix
## passed into the function.  The "get" functions simply return values that are set.
## The return is a list of the functions that can be applied to the matrix that is
## passed in in the x argument.

makeCacheMatrix <- function(x = matrix()) {
  mat <- numeric(0) 
  
  set <- function(m1){
    x <<- m1
    mat <<- numeric(0)
  }
  
  get <- function() x
  
  setInverse <- function(mat_inverse) mat <<- mat_inverse
  
  getInverse <- function() mat
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve applies the functions from the function list of makeCacheMatrix.
## The purpose is to return the inverse of a matrix either by finding it in a
## parent environment because it has already be calculated, or calculating it
## anew if it has not been previously calculated.  If it is a new calculation
## for the matrix, the inverse will be calculated using the solve function, and
## the result will be stored in the cached environment and will not need to be
## caculated again.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getInverse()
  
  if (length(mat) != 0) {
    message("Cached inverse found - returning cached inverse.")
    return(mat)
  }
  
  # Not found in cache.  Calculate inverse matrix.
  new_matrix <- x$get()
  mat <- solve(new_matrix, ...)
  x$setInverse(mat)
  
  mat
}
