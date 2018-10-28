## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix takes a matrix x as input (x is assumed to be invertible)
## and returns a list object with four functions:
## 1. set a new value for the matrix x
## 2. get the matrix x
## 3. set the inverse of the matrix x
## 4. get the inverse of the matrix x
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(z) inv <<- z
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)      
}


## cacheSolve is intended to be applied to an object of "type CacheMatrix",
## i.e., the input parameter x has to be created by the makeCacheMatrix function 
## above.
## If the inverse is already stored in x, cacheSolve returns the stored inverse.
## Otherwise, the inverse is computed, stored in x, and returned.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached inverse of the matrix")
            return(inv)
      }
      mtx <- x$get()
      inv <- solve(mtx,...)
      x$setinv(inv)
      inv
}
