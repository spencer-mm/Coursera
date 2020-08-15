## These functions store and call from a cache mutliple matrix inverses. These
## are written for the Coursera "Introduction to R" course. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    M_in <- NULL
    set <- function(y) {
        x <<- y
        M_in <<- NULL
    } 
    get <- function() x
    setM_in <- function(M_in) M_in <<- (M_in)
    getM_in <- function() M_in
    list(set = set, get = get, setM_in = setM_in, 
         getM_in = getM_in)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    M_in <- x$getM_in()
    if(!is.null(M_in)) {
        message("getting cached data")
        return(M_in)
    }
    data <- x$get()
    M_in <- solve(data, ...)
    x$setM_in(M_in)
    M_in
}
