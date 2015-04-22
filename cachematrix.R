## Matrix inversion can be very resource intensive so below are two
## functions created to cache the inverse of a matrix so that the calculation
## isn't performed repeatedly.

## The first function, makeCacheMatrix creates a special "vector", which 
## is really a list containing a function to:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setin <- function(inverse) i <<- inverse
    getin <- function() i
    list(set = set, get = get,
         setin = setin,
         getin = getin)
}

## This function checks the cached data to see if an inverse was already 
## calculated returning either the cached data or computes the inverse
## then stores within the special "vector" produced by makeCacheMatrix.

cacheSolve <- function(x, ...) {    
    i <- x$getin()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setin(i)
    i
}

