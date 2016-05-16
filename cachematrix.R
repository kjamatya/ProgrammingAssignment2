##  R Programming- Programming Assignment 2 

## These functions are used to inverse and cache the matrix.
## The cached matrix can be returned when needed such that
## the recalculation of inverse is not needed.



## The function makeCacheMatrix  created a special "matrix" object,
## that calculate and cache a inverse of ordinary matrix x.
## returns as list to get, set the value of the matrix and inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
    im <- NULL #  Initially set to NULL
set <- function(y) {
        x <<- y    # Set the value
        im <<- NULL # Clear the cache
    }

 # Gets the ordinary matrix
 get <- function() x

 # Set the inverse
 setInverse <- function(inverse) im <<- inverse

 # Get the inverse
 getInverse <- function() im

 # Return a list with the above four functions
  list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function returns the inverse of matrix
## if this called on the special matrix ,
## then the precomputed result is returned from cache
 
cacheSolve <- function(x) {
    # fetches the cached value for the inverse
    im <- x$getInverse() 
    if(!is.null(im)) { 
	# If the cache is not empty, return the cached matrix
        message("Getting cached matrix")
        return(im)
    }
    # Else Get the matrix itself
    data <- x$get() 

    # calculate the inverse 
    im <- solve(data) 

    # Cache the result
    x$setInverse(im) 
     
    # Return this new result
    im                
}