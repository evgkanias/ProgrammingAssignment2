## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## 'x' is a numeric or logical squared matrix.
    
    # Initialise the inverse matrix as NULL
    inv_x <- NULL
    
    # The function that sets the matrix and initialises the inverse one
    set <- function(y = matrix()) {
        x <<- y
        inv_x <<- NULL
    }
    
    # The function that returns the matrix
    get <- function() x
    
    # The function that sets the inverse matrix
    setinverse <- function(inv = matrix()) inv_x <<- inv
    
    # The function that returns the inverse matrix
    getinverse <- function() inv_x
    
    # Returns a list with the functions above
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## 'x' is a numeric or logical square and invertible CacheMatrix.
    ## If the matrix is logical, it will be converted automatically
    ## to numeric for the calculation.
    
    # Initialises the inverse matrix with the existed value
    inv_x <- x$getinverse()
    
    # check if the inverse matrix has been set
    if (!is.null(inv_x)) {
        # in this case it returns the existed inverse matrix
        message("getting inverse matrix")
        return(inv_x)
    }
    
    # computes the inverse matrix of x
    inv_x <- solve(x$get(), ...)
    
    # sets the inverse matrix to our CacheMatrix
    x$setinverse(inv_x)
    
    # returns the inverse matrix
    inv_x
}
