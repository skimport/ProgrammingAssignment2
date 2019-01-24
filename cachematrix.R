## The idea is to create a cache of the inverse of a matrix so as to not have
## to compute it over and over when the matrix has not changed.

## Function that creates a "matrix" that contains its own inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { # set the value of the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x # get the value of the matrix
    setinverse <- function(inverse) inv <<- inverse # set the inverse
    getinverse <- function() inv # get the inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Inverts the given matrix, returning the cached inverse if it exists
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


