## Put comments here that give an overall description of what your
## functions do

## caching the inverse of a matrix 
## function creating a matrix-caching object
makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv_x <<- inv
  getinv <- function() inv_x
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## Return a matrix that is the inverse of 'x'
##
## This function computes theinverse of the special"matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
## It is assumed that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
