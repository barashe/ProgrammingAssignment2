## The two functions below create a "smart" matrix, which cache its calculated
## inverse, rather than recompute it each time a user retrieves it

## The matrix factory. Creates the "smart" matrix, and allows editting of 
## matrix content (which automatically deletes the inverse) and inverse and retrieval
## of both

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse 
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the matrix inverse. Either from cache, or, if not cached,
## caculates it

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
        i <- solve(x$get(), ...)
        x$setInverse(i)
        i
}
