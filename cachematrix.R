## Using the pair of functions below, the inverse of a matrix can be read from the cache,
## in case it was already calculated since the last change to the matrix.
## in case the inverse was not yet calculated since the last change to the matrix, in inverse 
## can be calculated and the cache will be set.

## makeCacheMatrix returns constructs the functions to set/get the matrix, 
## and to set/get the the inverse of the matrix, and returns a list of these functions

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## When the matrix is changed using set(), the cached inverse (inv) is initialized.
    set <- function(y) {
        x <- y
        inv <- NULL
    }
    ##get() returns the matrix 
    get <- function() x
    ## The setInv() function, when called, sets the inv variable in the parent environment using <<- operator
    setInv <- function(inverse = matrix()) inv <<- inverse
    ## getInv() returns the inverse (inv)
    getInv <- function() inv
    ## makeCacheMatrix() returns the list of functions constructed above
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## cacheSolve() either returns from cache (if already calculated), or calculates the inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
