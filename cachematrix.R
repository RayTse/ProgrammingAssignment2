## Matrix inversion is usually a costly computation and there will be
## some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly.  Following pair of functions will serve for 
## this purpose.

## This function creates a special "matrix" object that can cache its inverse.
## This function, makeCacheMatrix creates a list vector containing 4 functions
## 1. $set(): to store the value of inputted matrix and clear any cache
## 2. $get(): return the value of inputted matrix
## 3. $setinverse(): to store inputted value as cache value
## 4. $getinverse(): return the value of the cache value
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


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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
