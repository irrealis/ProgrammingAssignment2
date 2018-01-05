## To paraphrase Roger Peng:
## (https://github.com/irrealis/ProgrammingAssignment2)
##
## Matrix inversion can be an expensive computation in terms of CPU time, so
## caching the result can be beneficial. The following functions do exactly
## that.
##
## - `makeCacheMatrix` creates a special "matrix" containing the actual matrix
##   and a cache to store the inverse.
##
## - `cacheSolve` is designed to used the special "matrix" above. If the
##   inverse has already been calculated and cached, `cacheSolve` returns the
##   cached value, and otherwise computes, caches, and returns the inverse.

## `makeCacheMatrix` creates a special "matrix" containing the actual matrix
## and a cache to store the inverse.
##
## Arguments:
## - `x`: an R `matrix`.
##
## Returns: a `list` containing functions to:
## - Set the value of the matrix.
## - Get the value of the matrix.
## - Set the value of the matrix inverse.
## - Get the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        # Cached value of the matrix inverse.
        x_inverse <- NULL

        # Inner function to set value of the matrix.
        set <- function(y) {
                x <<- y
                x_inverse <<- NULL
        }
        # Inner function to get value of the matrix.
        get <- function() x
        # Inner function to set cached value of the matrix inverse.
        setinverse <- function(y_inverse) x_inverse <<- y_inverse
        # Inner function to get cached value of the matrix inverse.
        getinverse <- function() x_inverse

        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
         )
}


## `cacheSolve` is designed to used the special "matrix" above. If the inverse
## has already been calculated and cached, `cacheSolve` returns the cached
## value, and otherwise computes, caches, and returns the inverse.
##
## Arguments:
## - `x`: a "matrix" created by `makeCacheMatrix`.
## - `...`: additional arguments that will be passed to the `solve` function to
##   compute the matrix inverse.
##
## Returns: the matrix inverse of `x`.
##
## Note: the returned inverse will be an R `matrix` object, not a "matrix"
## returned by `makeCacheMatrix`.

cacheSolve <- function(x, ...) {
        # Get possibly-NULL cached matrix inverse.
        x_inverse <- x$getinverse()
        # If cached matrix inverse isn't NULL, return its value.
        if(!is.null(x_inverse)) {
                message("getting cached date")
                return(x_inverse)
        }
        # Otherwise, get the underlying `matrix` from `x`, then compute, cache,
        # and return its matrix inverse.
        data <- x$get()
        x_inverse <- solve(data, ...)
        x$setinverse(x_inverse)
        x_inverse
}
