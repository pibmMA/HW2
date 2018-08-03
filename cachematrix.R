## Paired functions to cache the inverse of a matrix, then used the cached value 
## in the event the matrix hasnt changed 


## function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x<<-y
    invm <<- NULL
  }
  get <- function() x
  setinvm <- function(inverse) invm <<- inverse
  getinvm <- function() invm
  list(set=set, get=get, setinvm=setinvm, getinvm=getinvm)
  ## this list is used as the input to cacheSolve()
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed)
## then retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  invm <- x$getinvm()
  if(!is.null(invm)) {
    message("getting cached inverse matrix")
    return(invm)
  } else {
  matx <- x$get()
  invm <- solve(matx, ...)
  x$setinvm(invm)
  return(invm)
  }
}
