## There are two functions makeCacheMatrix and cacheSolve
# makeCacheMatrix calculates the inverse of a matrix and stores the result in cache memory
# cacheSolve returns the inverse of a matrix stored in cache memory.

## The function makeCacheMatrix calculates the inverse of a matrix and stores this inverse in cache. The input is a square matrix.

makeCacheMatrix <- function(x = matrix()) {
inverse_x <- NULL
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse_x <<- solve
    getinverse <- function() inverse_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## the function cacheSolve returns the stored inverse of a matrix; if the inverse is not present in cache memory, the function calculates the inverse. 
# Note: before performing the cacheSolve function the outcome of the makeCacheMatrix needs to be stored; the latter stored outcome can b

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_x <- x$getinverse()
    if(!is.null(inverse_x)) {
        message("getting cached inverse matrix")
        return(inverse_x)
    } else {
    inverse_x <-solve (x$get())
    x$setinverse(inverse_x)
    print(inverse_x)
	}
}
