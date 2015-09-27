## The two functions below are based on the example "Caching the Mean of a vector"
## The first one "makeCacheMatrix" creates an object that stores a matrix and the
## second one (cacheSolve) returns its inverse obtained from either computing it or pulling 
##it from the cache. 

## Write a short comment describing this function

##This funnction takes as an argument a matrix and returns a list of functions used by
## the function below to cache its inverse 

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


## Write a short comment describing this function

##This function returns the inverse of the matrix created by the function above.
## If that calculation wasn't cached before the computing of the inverse is done. If the inverse of the matrix was previously 
##calculated and cached, the calculation is skipped and the value is pulled from the cache


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
