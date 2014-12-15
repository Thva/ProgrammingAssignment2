########################################################

# cachingInverseMatrix.R
#
# This file creates a pair of functions that cache the inverse of a matrix.

########################################################

# makeCacheMatrix
#
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())    ## makeCacheMatrix can be called without any argument. If so, one can "initialize" the matrix with the method set()
{
    inverse <- NULL    ## "attribute" of 'makeCacheMatrix'
    set <- function(y)    ## initialization (needed if argument 'matrix' is not given)
    {
        x <<- y    ## "attribute" of 'makeCacheMatrix'
        inverse <<- NULL
    }
    get <- function() x    ## returns the matrix "attribute"
    setInverse <- function(inv) inverse <<- inv    ## method that sets its argument as the inverse matrix "attribute"
    getInverse <- function() inverse    ## method that returns the inverse matrix "attribute"
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  ## Returns a list of functions : one accesses to the functions through list selection
}


########################################################

# cacheSolve
#
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)
{
    inverse <- x$getInverse()    ## gets the inverse matrix
    if(!is.null(inverse))    ## if the inverse matrix is already cached
    {
        message("getting cache data")
        return(inverse)
    }
    matrix <- x$get()    ## if not, get the data from the object x
    inverse <- solve(matrix, ...)    ## compute the inverse (with possible extra argumments '...')
    x$setInverse(inverse)    ## set the value of 'inverse' in the inverse matrix "attribute" of object x
    inverse    ## returns the inverse matrix
}