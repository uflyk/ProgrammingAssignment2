## This is a set of functions for calculating inverse of a invertible matrix.
## And by taking advantage of caching, the functions will also saving time by 
## scoping instead of recamputation.

## By K @ Sep 27, 2015

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## create a special matrix for cache
    inv <- NULL
    ## 
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    ##
    get <- function() x
    setinverse <- function (inverse) inv <<- inverse
    getinverse <- function () inv
    list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.


## Note that all input of this function should be invertible

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)){
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
}

## testing:
## creating matrix to be calculated
## a <- matrix(c(4,7,2,6),colnum = 2, rownum = 2)
## solving process:
## ca <- makeCacheMatrix(a)
## inva <- cacheSolve(ca)
## what we have is the inverse of a stored in inva without any messages
## inva <- cacheSolve(ca)
## what we have is:
## getting cached data.
##       [,1] [,2]
## [1,]  0.6 -0.2
## [2,] -0.7  0.4
