## cachematrix.R
## Author: hemagso
## Version: 0.1
##
## This file contains functions cabable of calculating and caching the inverse of 
## matrices.
##
## - makeCacheMatrix: Factory for cacheMatrix objects.
## - cacheSolve: Cache enabled version of the solve function, written to work with
##   cacheMatrix objects.


makeCacheMatrix <- function(x = matrix()) {
    # This functions wraps a 2-D Array in a list, along with some useful "methods"
    # that allow us to cache the results of a inversion operation done on this
    # object. Description of the added methods may be found in the methods definition
    # below.
    #
    # Named Arguments:
    # - x: The matrix which we wish to wrap.
    #
    # Returns: List containing the the wrapped methods for accessing and manipulating
    # the data defined below.
    
    #Internal states of the object
    inv <- NULL
    
    #Methods for accessing internal states
    set <- function(y) {
        # This method set the internal matrix of the object.
        #
        # Positional Arguments:
        # - y: The matrix we which to wrap in the object.
        # 
        # Returns: NULL
        
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        # This method get the internal matrix of the object.
        # 
        # Returns: The wrapped matrix
        
        x
    }
    setinverse <- function(inverse) {
        # This method set the inverted matrix and is used by the cacheSolve 
        # function. In general, this method must not be called by the user.
        #
        # Positional Arguments:
        # - inverse: The inverse matrix which we want to store.
        #
        # Returns: NULL
        
        inv <<- inverse
    }
    getinverse <- function() {
        # This method get the inverted matrix and is used by the cacheSolve 
        # function. In general, this method must not be called by the user.
        #
        # Returns: The stored inverted matrix
        
        inv
    }
    
    #Returning a list to emulate the object
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    # This function returns the inverted matrix x. The object X must be an
    # instance created by the makeCacheMatrix function, that is, an object
    # with cache capabilities.
    #
    # We use the solve function to do the actual matrix inversion, so any
    # extra arguments besides the object supplied to this function will
    # be forwarded to the solve function.
    #
    # Positional Arguments:
    # - x: The cacheMatrix object which we want to invert.
    # - (...): Aditional arguments will be forwarded to the solve function 
    #
    # Returns: Inverted matrix of x.
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Using cached inverse matrix")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
