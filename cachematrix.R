## The functions in this file introduce a special matrix object that can 
## cache its inverse as well as a special function that can calculate the 
## inverse of such a special matrix or if available retrieves the inverse from
## the cache.

## makeCacheMatrix creates a special matrix object that can store a matrix and 
## ist inverse. It includes functions to set, get the matrix and the inverse. 
## Technically, the matrix object returns a list of 4 functions: set(), get(),
## setinverse(), getinverse()

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve returns the inverse of a special matrix object created by 
## makeCacheMatrix; if a inverse has already been stored in the matrix object,
## cachseSolve will simply return this stored matrix, otherwise the inverse will 
## be calculated and stored in the matrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
