## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

## Write a short comment describing this function

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

