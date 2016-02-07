## Put comments here that give an overall description of what your
## functions do
## My function caches the inverse of the matrix rather than computing it repeatedly. 

## Write a short comment describing this function

##My function creates a special matrix object that can cache its inverse.  

makeCacheMatrix <- function(x = matrix()) {
# set the stored inverse value to NULL and set the value of the Matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## my function calculates the invesrre of the special matrix returned
## by the makeCacheMatrix function above. If the invesre has
## already been computed, it gets the result and skips the calculation
## if it hasn't it computes the inverse and sets teh value in the cache
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
          message("getting cached data.")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
