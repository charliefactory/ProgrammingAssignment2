## This file has two functions that compute the inverse of a given
## square matrix. These two assume that the provided input is a 
## invertible matrix
##
## Sample code using these functions to get matrix inverse:
## > R = makeCacheMatrix(matrix(c(0,1,5,2,1,6,3,4,1),3,3))
## > cacheSolve(R)
##

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inverse <<- solve
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix function. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inverse = x$getInverse()
        
        ## Return cached inverse if it already has been computed & cached
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        
        data <- x$get()
        inverse <- solve(data,...)
        x$setInverse(inverse)
        inverse 
}
