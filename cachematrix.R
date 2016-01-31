## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  x<-NULL
  setMatrix<-function(y){
    x<<-y
    inv_x<<-NULL
  }
  getMatrix <- function() x
  setInverse <- function(inv) inv_x<<-inv
  getInverse <- function() inv_x
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getInverse()
  if(!is.null(inv_x)) {
    message("getting cached inverse")
    return(inv_x)
  }
  data <- x$getMatrix()
  inv_x <- solve(data, ...)
  x$setInverse(inv_x)
  inv_x
}
