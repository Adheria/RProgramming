## A pair of functions that cache and compute the
## inverse of a matrix

## This function creates a matrix object
## that can cache its inverse
## > matrix2 <- makecachematrix(matrix1)
## > matrix2$get()
## Shows the matrix
## > matrix2$getinv()
## > NULL (it's not there yet)


makecachematrix <- function(mtx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtx <<- x
        inverse <<- NULL
    }
    get <- function() return(mtx)
    setinv <- function(inv) inverse <<- inv
    getinv <- function() return(inverse)
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))

}

## This function computes the inverse of the special
## "matrix" returned by the above function. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cachesolve` should retrieve the inverse from the cache
## > cachesolve(matrix2)
## > shows inverted matrix
## > matrix2$getinv() now will show that inverted matrix

cachesolve <- function(mtx, ...) {
    inverse <- mtx$getinv()
    if(!is.null(inverse)) {
        print("Getting cached data...")
        return(inverse)
    }
    data <- mtx$get()
    inverse <- solve(data, ...)
    mtx$setinv(inverse)
    return(inverse)


}
