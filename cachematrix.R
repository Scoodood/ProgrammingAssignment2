## Put comments here that give an overall description of what your
## functions do

## The makeCachedMatrix function creates a special "matrix" containing a list 
## of the following functions
## 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {
    ivm <- NULL
    set <- function(y) {
        x <<- y
        ivm <<- NULL
    }
    get <- function() x
    set_inv <- function(inv_matrix) ivm <<- inv_matrix
    get_inv <- function() ivm
    list(
        set=set,
        get=get,
        set_inv=set_inv,
        get_inv=get_inv)
}


## The cacheSolve function calculates the inverse of the special "matrix".
## If the inverse not yet exist, it will calculate one and keep a copy in its
## cache. Otherwise, it will simply return inverse from its cache and skip the
## computation.

cacheSolve <- function(x, ...) {
    ivm <- x$get_inv()
    if (!is.null(ivm)) {
        message("getting cached data")
        return (ivm)
    }
    data <- x$get()
    
    # use the solve() function to calculate the inverse
    ivm <- solve(data)
    x$set_inv(ivm)
    ivm
}
