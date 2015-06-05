## Put comments here that give an overall description of what your
## functions do
# Caching functionality in r in 
# assignment 2 of Coursera Datascience Course 2: R Programming

## Write a short comment describing this function
# makeCacheMatrix() define functions to handle value in cache
# for a inversible matrix 
makeCacheMatrix <- function(x = matrix()) {
        # x: invertible matrix  
        # returns: list of functions
        #       1.set the matrix - set()
        #       2.get the matrix - get()
        #       3.set the inverse - setsolve()
        #       4.get the inverse - getsolve()
        
        m <- NULL
        
        # Clear inverse matrix and store matrix
        # by assigning value to an object in a different
        # environment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # Return original matrix input       
        get <- function() x
        
        # Store inverse matrix
        # by assigning value to an object in a different
        # environment 
        setsolve <- function(solve) m <<- solve
        
        # Return inverse matrix from cache
        getsolve <- function() m
        
        # Return all functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
# cacheSolve() returns the inverse of the original matrix 
# input to makeCacheMatrix(), if the inverse is already stored
# in the cache, the function will return the inverse from the
# cache instead of calculating the inverse
cacheSolve <- function(x, ...) {
        # x: output of makeCacheMatrix()
        # return: inverse of the makeCacheMatrix() matrix input
        m <- x$getsolve()
        
        # if the inverse is already calculated
        if (!is.null(m)) {
                # return value from cache and stop processing
                message("Cached data available")
                return(m)
        } 
        
        # calculate the inverse matrix
        data <- x$get()
        m <- solve(data, ...)
        
        # store the value in cache
        x$setsolve(m)
        
        # return calculated inverse matrix
        m
}
