## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                      ## initialize inverse result variable
        
        set <- function(y) {                             ## define set matrix function
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x                              ## define get matrix function
        
        setinverse <- function(inverse) inv <<- inverse  ## define set inverse function
        
        getinverse <- function() inv                     ## define get inverse function
        
        list(set = set, get = get,                       ## create special "matrix" using a list
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by function makeCacheMatrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()                   ## retrieve previously calculated inverse
        
        if (!is.null(inv)) {
                message("getting cached data")  ## stored inverse was found so return value and leave function
                return(inv)
        }
        
        data <- x$get()                         ## retrieve matrix
        
        inv <- solve(data, ...)                 ## calculate inverse of matrix
        
        x$setinverse(inv)                       ## store inverse in special "matrix"
        
        inv                                     ## return inverse to calling routine
}
