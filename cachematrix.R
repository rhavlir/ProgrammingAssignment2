## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## function assumes that the matrix is a square invertible matrix
  ## used to run cacheSolve function
  myInv <- NULL
  set <- function(y) {
    x <<- y
    myInv <<- NULL
  }
  get <- function() x
  
  ## store in Cache
  setMyInv <- function(inverse) myInv  <<- inverse
  
  ## get from cache
  getMyInv <- function() myInv
  list(set = set, get = get,
       setMyInv = setMyInv,
       getMyInv = getMyInv)
}


## Write a short comment describing this function
## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  myInv <- x$getMyInv()
  
  ## check to see if the inverse has already  been calcualted
  ## and if so get it from the cache instead of recalculating
  if(!is.null(myInv)) {
    message("getting cached data")
    return(myInv)
  }
  
  ## if not already calculated, use solve function to find inverse of a square matrix
  data <- x$get()
  myInv <- solve(data, ...)
  x$setMyInv(myInv)
  myInv
}
