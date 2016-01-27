## Put comments here that give an overall description of what your
## functions do

## Defines a list of functions that operate the inversed matrix cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Tries to get and return the inversed matrix from a cache, if found, 
## otherwise will inverse the matrix and create the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
