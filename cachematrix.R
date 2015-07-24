## Below are two functions that are used to create a special object 
## that stores a matrix vector and cache's its inverse.

## The first function, makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to
## set the matrix of the vector
## get the matrix of the vector
## set the matrix of the inverse
## get the matrix of the inverse

makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) ix <<- inverse
        getinverse <- function() ix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function calculates the inverse of the special "vector" 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the matrix of the inverse in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ix <- x$getinverse()
        if(!is.null(ix)) {
                message("getting cached data")
                return(ix)
        }
        data <- x$get()
        ix <- solve(data, ...)
        x$setinverse(ix)
        ix
}
