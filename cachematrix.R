## Put comments here that give an overall description of what your
## functions do

## In those functions, first we make a cached matrix that allows a both input matrix
## and inversed matrix to be stored, then during the cacheSolve function event,
## we calculate the situation whether we use the cached inversed matrix
## or calculate a new one.

## Write a short comment describing this function

## Stores a matrix in a list and return the list for use
## List includes functions to
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inversed matrix
##   get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  set <- function(y) {
    x <<- y
    xInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inversedMatrix) xInverse <<- inversedMatrix
  getInverse <- function() xInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## Function that solves for the inverse
## First determines if inverse has already been calculated and cached
## if not calculates inverse using solve()
## if it has already been calculated retrieves cached inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xInverse <- x$getInverse()
  if (!is.null(xInverse)) {
    message("getting cached data")
    return(xInverse)
  }
  data <- x$get()
  xInverse <- solve(data, ...)
  x$setInverse(xInverse)
  return(xInverse)
}
