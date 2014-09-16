#Coursera programming assignment 2.
#This script contains to functions which allow to set a matrix, calculate inverse matrix, 
#store inverse matrix in cache and modify initial matrix with subsequent recalculation of inverse matrix.

#The first function - makeCacheMatrix allows to create 
makeCacheMatrix <- function(x = matrix()) {
  inverseM <- NULL
  set <- function(y) {
    x <<- y
    inverseM <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverseM <<- solve
  getinverse <- function() inverseM
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
  inverseM <- x$getinverse()
  if(!is.null(inverseM)) {
    message("getting cached data")
    return(inverseM)                     ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  inverseM <- solve(data, ...)
  x$setinverse(inverseM)
  inverseM
}
