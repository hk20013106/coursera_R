## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## define a list of four elements.

makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL 
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    
    matrix_cache  <- function() x
    ##store the geted matrix
    setmatrix <- function(inverse_getter) inverse_x <<- inverse_getter
    ##get a argument called inverse_getter, assign it to inverse_x
    
    getmatrix <- function() inverse_x
    ## assign the result of setmatrix and assign it to getmatrix
    
    list(set = set, matrix_cache = matrix_cache,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
    ## save the four functions as components of the list
}


## Write a short comment describing this function

cacheSolve <- function(want_m, ...) {
        ## Return a matrix that is the inverse of 'x'
    wanted_m <- want_m$getmatrix()
    if(!is.null(wanted_m)) {
        message("getting cached inverse matrix")
        return(wanted_m)
    }
    cache_m <- want_m$matrix_cache()
    inverse_x <- solve(cache_m, ...)
    want_m$setmatrix(inverse_x)
    inverse_x
}

