## Put comments here that give an overall description of what your
## functions do
## The following functions calculate the inverse of a given matrix and store 
## it in the cache then retrieve the result from the cache 
## Write a short comment describing this function
## Function MakeCacheMatrix makeCacheMatrix: This function creates 
## a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  inv
}
## Write a short comment describing this function
##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve will retrieve 
##the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- inv(data, ...)
  x$setinv(inv)
  inv
}
