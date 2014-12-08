## Author : Jeff Hohenstein
## Purpose : 
##    Assignment 2 : Caching the Inverse of a Matrix
##    Demonstrate two functions that together compute the inverse of a matrix
##    and cache the result so that future calls to return the inverse use the 
##    cached version.
##
## Discussion: 
##    I followed the given example directly. However, having a long history in OO
##    programming, this is a weird solution to me. It is odd that our
##    pseudo-object allows us to call setInverse(). This is odd because a matrix has
##    at most one inverse and it should not be settable. Instead, in an OO language,
##    the operation to compute the inverse and optionally cache it should be attached
##    to the object and always return the same value.

makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL
    
    # Get the matrix
    get <- function() x
    
    # Set the matrix or default it to an empty matrix
    set <- function(y){
      x <<- y
      
      # Force the recomputation of the inverse 
      inverse <<- NULL
      
      # Return the set matrix
      y
    }
    
    # Get the inverse
    # This method is done in the style of the example
    getInverse <- function() inverse
    
    # Alternative to getInverse that computes the solution if the cached version
    # is not present.
    # 
    # Note : this version is called invert() because it doesn't attempt to solve for coefficients
    # of a series of linear equations but merely inverts the matrix. I think this solution is better
    # because it works whenever you call it whereas getInverse() returns NULL if the cacheSolve() method
    # has not been called.
    invert <- function() {
      
      if(is.null(inverse)){
        message("Computing cacheable inverse of matrix")
        inverse <<- solve(x)  
      }
      
      return(inverse)
      
    }
    
    # Set the inverse
    setInverse <- function(i) inverse <<- i
    
    # Returns the "matrix" as a list of operations
    list(
      set = set, 
      get = get,
      getInverse = getInverse,
      setInverse = setInverse,
      invert = invert
    )
}


## Invert the given matrix
##    If the given matrix does not have a cached inverse, compute a new one
##    otherwise return the cached copy.
##
## Note : This method is done in the style of the given examples. See invert()
##    in the "object" for what I think is a better implementation.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)){
          message("Getting cached matrix inverse")
          return(i)
        }
        else {
          m <- x$get()
          i <- solve(m)
          x$setInverse(i)
          i
        }
}
