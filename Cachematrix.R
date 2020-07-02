## Put comments here that give an overall description of what your
# The following functions create a matrix and cache its inverse
# 1) set : sets or stores the matrix
# 2) get : return the matrix
# 3) setinverse : caches the inverse of matrix
# 4) getinverse : returns the inverse(if inverse is cached) 
## functions do

## Write a short comment describing this function
# This function creates a special matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv1) inv <<- inv1
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# This function returns the inverse of the special matrix
# If the inverse is cached it will fetch it using getinverse
# Else it will calculate the inverse and cache it using setinverse
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
