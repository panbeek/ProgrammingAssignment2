## makeCacheMatrix creates a "special" matrix, that makes it possible to store it's inverse
## At creation the inverse matrix (sol) is initialized.
## Functions:
## - set: Stores the actual matrix
## - get: Returns the actual matrix
## - setsolution: Stores the inverse matrix
## - getsolution: Returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    sol <- matrix(ncol = ncol(x))
    set <- function(y) {
        x <<- y
        sol <<- matrix(ncol = ncol(y))
    }
    get <- function() x
    setsolution <- function(solution) sol <<- solution
    getsolution <- function() sol
    list(set = set, get = get,
         setsolution = setsolution,
         getsolution = setsolution)

}

## cacheSolve returns the inverse of a matrix
## - If the inverse has been calculated before, and stored in the cache it just
##   returns the inverse from cache
## - If the inverse has not been stored yet, in calculates the inverse matrix using
##   the funtion "solve"

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    sol <- x$getsolution()
    if(!is.null(sol)) {
        message("getting cached data")
        return(sol)
    }
    data <- x$get()
    sol <- solve(data, ...)
    x$setsolution(sol)
    sol
}

