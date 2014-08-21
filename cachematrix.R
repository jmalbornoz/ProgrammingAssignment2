##
## makeCacheMatrix: creates a cache of a matrix and its inverse
##
## cacheSolve: if there is not a cached inverse, matrix is retrieved and inverse calculated, 
## otherwise cached inverse is returned
##

#
# makeCacheMatrix
#
# This function creates an object that caches that caches a matrix and its inverse
#
# Jose M Albornoz
# August 21 2014
#
makeCacheMatrix <- function(x = matrix()) {
    
    # a function that verifies if a matrix M is square
    checkMatrix <- function(M) {
        if(dim(M)[1] != dim(M)[2]) {
            stop("Matrix must be square to compute its inverse")
        }
    }
    
    # checks if matrix is square, if not execution is stopped
    checkMatrix(x)
    
    # sets inverse to null
    inv <- NULL
    
    # sets Matrix
    setMatrix <- function(y) {
        checkMatrix(y)             # verifies if matrix is square, if not execution is stopped
        x <<- y
        inv <<- NULL
    }
    
    # defines functions to cache and recover matrix and its inverse
    getMatrix <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


#
# cacheSolve
#
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), then the 
# cachesolve should retrieve the inverse from the cache.
#
# Jose M Albornoz
# August 21 2014
#
cacheSolve <- function(x, ...) {
    
    # retrieves inverse from cached matrix object
    Inv <- x$getInverse()
    
    # the inverse has already been calculated and the matrix has not changed: inverse is retrieved
    # and returned
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    
    # retrieves matrix
    data <- x$getMatrix()
    
    # computes inverse
    Inv <- solve(data)
    
    # caches inverse
    x$setInverse(Inv)
    
    # returns inverse
    Inv
    
}
