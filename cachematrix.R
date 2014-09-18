##Programming assignment 2.
##This script contains to functions which allow to set a matrix, calculate inverse matrix, 
##store inverse matrix in cache and modify initial matrix with subsequent recalculation of inverse matrix.

##The first function - makeCacheMatrix allows to create a matrix, view and modify a matrix.
##The functions also creates a variable to store store cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverseM <- NULL          
  set <- function(y) {      ## This is a function that allows to set new matrix. 
    x <<- y                 ## <<- operator assigns the y to  the variable x from parent environment.  
    inverseM <<- NULL       ## ensurs that the old value of inverse matrix is not used with the new matrix.
  }
  get <- function() x       ## creats a get function which allows to aaccess our matrix stored in x variable
  setinverse <- function(solve) inverseM <<- solve    ## allows to set a value inverse matrix
  getinverse <- function() inverseM                   ## allows to access cached value of inverse matrix
  list(set = set, get = get,       ## allows to call our functions with $ operator
       setinverse = setinverse,    ## like x$setinverse()
       getinverse = getinverse)
}


## This function calculates the inverse matrix to the matrix that is created by createCacheMatrix function. And saves this value for later use (caches it.

cacheSolve <- function(x, ...) {
        
  inverseM <- x$getinverse()            ## gets the value of inverse matrix from the object create by createCacheMatrix
  if(!is.null(inverseM)) {              ## checks if the inverse matrix value is available  
    message("getting cached data")      ## if the value is already available a message is printed and 
    return(inverseM)                    ## a matrix that is the inverse of 'x' is returned
  }
  data <- x$get()                       ## if the value of inverse matrix is not cached, the matrix x is obtained
  inverseM <- solve(data, ...)          ## inverse matrix is calculated for matrix x
  x$setinverse(inverseM)                ## the value of inverse matrix is saved in object x using setinverse function
  inverseM                              ## the value of inverse matrix is returned
}

## The comments are a bit overkill :) Thanks for grading my assignment.