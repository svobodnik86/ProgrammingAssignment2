## Put comments here that give an overall description of what your
## functions do

## the function makeCacheMatrix retrieves the input (a matrix) and 
## defines a list of operations

makeCacheMatrix <- function(x = matrix()) {
## it should stay the same as in the example
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        ## it should be possible to cache the inverse now...solve(x) computes inverse matrix
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## the function cacheSolve calculates the inverted matrix
## unless the matrix was already calculated before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


