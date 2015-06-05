## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        #clear inverse matrix and store matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #return matrix        
        get <- function() x
        
        #store inverse matrix
        setsolve <- function(solve) m <<- solve
        
        #return inverse matrix
        getsolve <- function() m
        
        #list all functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        } else {
                message("calculating inverse matrix")
        }
        
        data <- x$get()
        x$set(data)
        
        m <- solve(data, ...)
        x$setsolve(m)
        
        m
}
