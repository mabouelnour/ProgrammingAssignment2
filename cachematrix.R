## This R file is as per Coursera Week3 Assignment
## The purpose of the assignment is to write R function able to cache potentially time-consuming computations
## functions do

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## We are re-using the same code provided by Coursera to Cache the mean of a vectore  
## In this case we apply the changes to cache the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
mat_inv <- NULL
        set <- function(y) {
                x <<- y
                mat_inv <<- NULL
        }
        get <- function() x
        set_matInverse <- function(inverse) mat_inv <<- inverse
        get_matInverse <- function() mat_inv
        list(set = set,
             get = get,
             set_matInverse = set_matInverse,
             get_matInverse = get_matInverse)
}


## The function cacheSolve will computes the inverse of the special matrix that was 
## created by makeCacheMatrix above. As per the requirements, If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        mat_inv <- x$get_matInverse()
        if (!is.null(mat_inv)) {
                message("getting cached data")
                return(mat_inv)
        }
        mat_data <- x$get()
        mat_inv <- solve(mat_data, ...)
        x$set_matInverse(mat_inv)
        mat_inv
        
}


