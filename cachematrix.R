## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inverse_m <- NULL
  set <- function(y) {
    x <<- y
    inverse_m <<- NULL
  }
  get <- function() { x }
  setInverse <- function(inverse) {inverse_m <<- inverse}
  getInverse <- function() {inverse_m}
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse_m <- x$getInverse()
  if (!is.null(inverse_m)) {
    message("getting cached data")
    return(inverse_m)
  }
  mat <- x$get()
  inverse_m <- solve(mat, ...)
  x$setInverse(inverse_m)
  inverse_m
}

