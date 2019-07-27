
## makeCacheMatrix: It creates a special “matrix” object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This second function computes the inverse of the special "matrix" created by the previous function: makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inverse)){
    message("Getting cached data...")
    return(inverse)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inverse)
  inverse      
}
