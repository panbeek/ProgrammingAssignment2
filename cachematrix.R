## makeCacheMatrix creates a "special" matrix, that makes it possible to store it's inverse in cache
## At creation the inverse matrix (sol) is initialized.
## Functions:
## - set: Stores the actual matrix
## - get: Returns the actual matrix
## - setsolution: Stores the inverse matrix
## - getsolution: Returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    sol <- NULL
    set <- function(y) {
        x <<- y
        sol <<- NULL
    }
    get <- function() x
    setsolution <- function(solution) sol <<- solution
    getsolution <- function() sol
    list(set = set, get = get,
         setsolution = setsolution,
         getsolution = getsolution)

}

## cacheSolve returns the inverse of a matrix
## - If the inverse has been calculated before, and stored in the cache it just
##   returns the inverse from cache
## - If the inverse has not been stored yet, it calculates the inverse matrix using
##   the funtion "solve"

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    sol <- x$getsolution()
    ## If sulution is stored get it from the cache
    if(!is.null(sol)) {
        message("getting cached data")
        return(sol)
    }
    ## Otherwise calculate the inverse matrix and store it in cache
    data <- x$get()
    sol <- solve(data, ...)
    x$setsolution(sol)
    sol
}

