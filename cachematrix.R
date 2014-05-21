## makeCacheMatrix and cacheSolve calculate the inverse of a matrix, and cache
## the value so that it can be re-used if the calculation is repeated

## makeCacheMatrix: takes a matrix and returns a list containing a four functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve: takes the output of makeCacheMatrix and returns the inverse
## of the matrix given to makeCacheMatrix. However, it checks if it was previously 
## cached by makeCacheMatrix, in which case it skips the calculation and takes the 
## cached value. Otherwise it calculates the inverse of the matrix and sets the
## value in the cache using the setInverse function of makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
