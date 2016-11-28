## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## Returns cached matrix and either its cached inverse or NULL.
## Provides functions for manipulating the cache.
## Providing a new matrix will invalidate the cached inverse value with a NULL.

makeCacheMatrix <- function(x = matrix()) {
  om <- x #original
  im <- NULL #inverse
  
  set <- function(m) {
    #optimization to only invalidate inverse when new matrix is diff than old
    if(!identical(om, m)){
      om <<- m
      im <<- NULL
    }
  }
  get <- function() om
  getInverse <- function() im
  setInverse <- function(i) im <<- i
  
  list(set=set, get=get, getInverse=getInverse, setInverse=setInverse)
}


## Write a short comment describing this function
## 
## Calculate the inverse of the supplied matrix. Return cached version if exists.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if(!is.null(x$getInverse())){
    message("returning cached data")
    return(x$getInverse())
  }
  sm <- solve(x$get())
  x$setInverse(sm)
  sm
}
