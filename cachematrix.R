## Put comments here that give an overall description of what your
## functions do
## These functions make an inverse matrix of an input matrix.
## A matrix that is run through this function will have its value stored in a cache. 
## If its value has already been dtermined, the function will recall it from the cache, rather than recalculate it.

## Write a short comment describing this function
## Defines the function, sets variables to "go back" to ("<<-") a former environment,
## makes a list of the subfunctions in the main function.

makeCacheMatrix <- function(x = matrix()) { ##stores the "set" and "get" functions
  m <- NULL ## creates initial matrix, default = NULL
  set <- function(y) { # puts new matrix into main function 
          x <<- y ## substitutes new vector?? 
          m <<- NULL  ## makes value of m = NULL (erases cache??)
  }
  get <- function() x ## returns matrix stored in cache
  setmatrix <- function(solve) m <<- solve ## sets a new solved matrix into the cache
  getmatrix <- function() m ## retrieves the solved matrix from the cache
  list (set = set, get = get, ## make list of these functions
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}


## Write a short comment describing this function:
## returns the inverse matrix either from the cache or calculates a new one and stores it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {  ## if !is.null is true (not NULL), the cached matrix is returned 
          message("getting cached data") ## message stating that cached matrix is being recalled
          return(m)  ## cached matrix is reported
        }
        data <- x$get()  ## Else, inverse is calculated from input matrix
        m <- solve(data, ...)  ## new calculation
        x$setmatrix(m)  ## new calculation is cached
        m ## new inverse matrix reported
}
