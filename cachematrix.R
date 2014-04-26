## Two functions that are used to create a special object that stores a numeric 
## and caches its inverse.

## Creates a special "vector", which is really a list 
## containing a function to
##1. set the value of a matrix
##2. get the value of a matrix
##3. set the value of the invere of a matrix
##4. get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function that calculates the inverse of a 'x'. It first checks to see if the 
## inverse has already been calculated. 

cacheSolve <- function(x, ...) {
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
