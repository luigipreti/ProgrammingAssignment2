## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix functions creates a special matrix by setting the value of 
## a matrix, getting the value of a matrix, setting the value of its inverse and
## getting the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## the cacheSolve function calculates the inverse of a matrix returned from the 
## above function. if the inverse already exists, it returns the inverse from
## the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}