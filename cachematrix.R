## These functions demonstrate R's ability to cache values rather than recomputing them, when appropriate.
## In this example, the inverse of a matrix is computed using the solve() function.  If an unchanged matrix is passed to the function,
## the cached value will be returned rather than re-computing the same value.

## The makeCacheMatrix function creates a list of functions which get and set the value of the matrix and the cached inverse value of the matrix.  

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## The cacheSolve function checks to see if a matrix has already been solved.  If so, it returns the cached value.
## Otherwise, it computes the inverse of the matrix, caches the solution, and returns the value.

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("Retrieving cached data.")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmatrix(m)
  m
}
