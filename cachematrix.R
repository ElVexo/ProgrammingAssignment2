## These functions demonstrate R's ability to cache values rather than recomputing them, when appropriate.
## In this example, the inverse of a matrix is computed using the solve() function.  If an unchanged matrix is passed to the function,
## the cached value will be returned rather than re-computing the same value.

## The makeCacheMatrix function takes a matrix as an argument.  

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


## Write a short comment describing this function

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
