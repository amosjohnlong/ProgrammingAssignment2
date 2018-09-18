# The makeCacheMatrix function creates a series of object methods used to cache a
# matix and its inverse.  The object methods are stored in a special "cache matrix".

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# The cacheSolve function uses either a cached inverse of the matrix, or finds the 
# matrix's inverse and caches it.  This function relies on the object methods 
# created in makeCacheMatrix(), above.
# For this assignment, we can assume the input matrix, x, is always invertible.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
