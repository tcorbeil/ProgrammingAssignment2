## cachematrix.R - August 2015

## The two functions below enable the programmer to cache the
##   inverse of a matrix and call on that inverse later
##   without having to recompute it.

## First function: creates a list of functions to set the 
##   matrix and its inverse in the global environment and 
##   facilitate their retrieval.


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      get <- function() x
      setinverse <- function(inverted) inv <<- inverted
      getinverse <- function() inv      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Second function: calls on makeCacheMatrix to find out if
##   an inverse is already cached for the given matrix. If so,
##   it returns the cached inverse. If not, it computes
##   the inverse, caches it, and returns it.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv      
}        
