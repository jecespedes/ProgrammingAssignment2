## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix"
## If the inverse has been calculated then the cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inver = x$getInverse()
  if (!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  data = x$get()
  inver = solve(data, ...)
  x$setInverse(inver)
  
  ## Return a matrix that is the inverse of 'x'
  return(inver)
}
