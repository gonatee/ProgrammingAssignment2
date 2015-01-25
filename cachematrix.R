## Put comments here that give an overall description of what your
## 2015-01-26
## functions do
## Write a short comment describing this function

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
