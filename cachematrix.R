## functions to cache the inverse of a matrix for r programming assignment 2
## Following function "makeCacheMatrix": This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        setValue <- function(y){
                x <<- y
                inverse <<- NULL
        }
        getValue <- function() x
        setInverse <- function(solveMatrix) inverse <<- solveMatrix
        getInverse <- function() inverse
        list(setValue = setValue, getValue = getValue, setInverse = setInverse, getInverse = getInverse)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$getValue()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse      
}
