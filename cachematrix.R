#Assignment: Caching the Inverse of a Matrix
# Matrix inversion is usually a costly computation and there may be some benefit to caching 
# the inverse of a matrix rather than computing it repeatedly (there are also alternatives 
# to matrix inversion that we will not discuss here). Your assignment is to write a pair of 
# functions that cache the inverse of a matrix.

#Write the following functions:    
# * makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# * cacheSolve: This function computes the inverse of the special "matrix" returned 
#   by makeCacheMatrix above. If the inverse has already been calculated 
#   (and the matrix has not changed), then cacheSolve should retrieve the inverse 
#    from the cache.


# makeCacheMatrix: return a list of functions to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    # inverseM will store the cached inverse matrix
    inverseM <- NULL
    
    # setter for the matrix
    set <- function(y) {
        x <<- y
        # clear out current inverse
        inverseM <<- NULL
    }
    
    # getter for the matrix
    get <- function() x
    
    # setter for the inverse
    setinverse <- function(inverse) inverseM <<- inverse
    # getter for the inverse

    getinverse <- function() inverseM
    # return the matrix with our defined functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# cacheSolve: compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
    ## return a matrix that is the inverse of 'x'

    inverseM <- x$getinverse()
    # If the inverse is already calculated, return it
    if (!is.null(inverseM)) {
        message("getting cached data")
        return(inverseM)
    }
    # calculate the inverse matrix using solve
    data <- x$get()
    inverseM <- solve(data, ...)
    # and cache it
    x$setinverse(inverseM)
    
    inverseM
}
