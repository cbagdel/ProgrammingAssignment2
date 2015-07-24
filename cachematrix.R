## Using the pair of functions below, the inverse of a matrix will either be read from the cache,
## in case it was already calculated; 
## or the inverse will be calculated and cached, in case the inverse was not yet calculated. 

## makeCacheMatrix() constructs the functions to set/get the matrix, 
## and to set/get the the inverse of the matrix, 
## and returns a list of these functions
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

## cacheSolve() either returns the inverse of matrix 'x' from the cache (if already calculated), 
## or calculates the inverse, and caches the inverse using setInv(), and returns the inverse
cacheSolve <- function(x, ...) {
    ## read the cached inverse
    inv <- x$getInv()
    ## if the cached inverse is not null, return it.
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    ## calculate the inverse of matrix
    inv <- solve(data, ...)
    ## cache the calculated inverse
    x$setInv(inv)
    ## return the inverse
    inv
}
