## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly
## 

## This function takes a square matrix and returns
## a special square matrix with its cached inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(cacheSolve) inv <<- cacheSolve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the
## inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
}
