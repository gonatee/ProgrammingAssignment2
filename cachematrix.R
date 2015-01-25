## Put comments here that give an overall description of what your
## 2015-01-26
## functions do
## Write a short comment describing this function

## The first function, makeCacheMatrix creates a list containing a function to
## set the value of the Matrix
## get the value of the Matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## iMatrix = Inverted Matrix
  iMatrix <- NULL
  set <- function(y) {
    x <<- y
    iMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) iMatrix <<- inverse
  getinverse <- function() iMatrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## The following function calculates the inverse of the "matrix" created with the above function. 
## 1. checks to see if the inverted version has already been calculated
##   If so - it gets the value from the cache and skips the computation
##   Else - it calculates the inverted version of the data then sets the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  iMatrix <- x$getinverse()
  if(!is.null(iMatrix)) {
    message("getting cached data")
    return(iMatrix)
  }
  data <- x$get()
  iMatrix <- solve(data, ...)
  x$setinverse(iMatrix)
  iMatrix
}
