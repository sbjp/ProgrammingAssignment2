## Taking the inverse of a large matrix may take a while to compute
## so these functions will cache the inverse and look it up if needed
## again rather than re-compute, which will happen if the contents
## of the matrix change



## create a special object that stores a matrix and caches its inverse

makeCacheMatrix  <- function(x = matrix()) {
        matrixinverse <- NULL
        set <- function(y) {
                x <<- y
                matrixinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) matrixinverse <<- solve
        getinverse <- function() matrixinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## calculates the inverse of the special matrix created with the above function
## unless it has already been calculated, at which point it will retrieve it from
## the cache and skip the computation

cacheSolve <- function(x, ...) {
        matrixinverse <- x$getinverse()
        if(!is.null(matrixinverse)) {
                message("getting cached data")
                return(matrixinverse)
        }
        data <- x$get()
        matrixinverse <- solve(data, ...)
        x$setinverse(matrixinverse)
        matrixinverse
}
