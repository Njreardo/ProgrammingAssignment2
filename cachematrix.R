## Put comments here that give an overall description of what your
## functions do

## This function creates a pecial matrix object that can cache its inverse
makeCacheMatrix <- function(mat = matrix()) {
  cache <- NULL
  set <- function(assign) {
    mat <<- assign
    cache <<- NULL
  }
  get <- function() mat
  setInverseMatrix <- function(inverseMatrix) cache <<- inverseMatrix
  getInverseMatrix <- function() cache
}

## This function computes the inverse of the special matrix returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(mat, ...) {
  cache <- mat$getInverseMatrix()
  if(!is.null(cache)) {
    message("Returning cached data")
    return(cache)
  }
  data <- mat$get()
  cache <- solve(data, ...)
  mat$setInverseMatrix(cache)
  cache
}
