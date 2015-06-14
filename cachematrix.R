## The two following functions can be used to calculate and cache 
## the inverse of a matrix. The first function is a factory-function 
## that creates a special matrix that is able to cache its inverse matrix
## once calculated (and assigned using setinv())
## The second function (cacheSolve) is used to return the inverse of the matrix by 
## using the factory-instance
##
## Example usage:
## # create a new cacheMatrix
## >matrix <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
##
## # solve the matrix, the inverse matrix is calculated and stored in the cache
## >inv-matrix <- cacheSolve(matrix)
## 
## # set a new matrix, cached inverse is cleared
## > matrix$set(matrix(c(9,9,4,5,6,6,3,2,1), nrow=3,ncol=3))


## This factory-function creates and returns a matrix-caching object
## with 4 functions to set/get the matrix and to set/get the inverse
## of the matrix
##
## functions of the returned object
## set: set the matrix
## get: get the matrix
## setinv: set the inverse of the matrix
## getinv: get the inverse of the matrix
##
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

## Return a matrix that is the inverse of 'x' by
## This function computes the inverse of the "special matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves the inverse 
## from the cache.
## It is assumed that the matrix supplied is always invertible
##
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
