# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list of functions to set/get the matrix
# and set/get the inverse of matrix.
makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
      
     # set the matrix into local environment and set matrix
     # inverse to NULL   
     set <- function(y) {
         x <<- y
         inv <<- NULL
     }
      
     # get the stored matrix
     get <- function() x
      
     # set the inverse of the matrix
     setInverse <- function(inverse) inv <<- inverse
      
     # get the cached inverse of the matrix
     getInverse <- function() inv
      
     # return the list of functions
     list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


# The following function returns the inverse of the matrix. 
#
# It checks if the inverse is already computed using getInverse
# function.
# 1. If yes, it gets the result and skips the computation. 
# 2. If not, it computes the inverse, sets the value in the cache 
#    using setInverse function.
#
# Assumption: This function assumes that the matrix is square and invertible.
cacheSolve <- function(x, ...) {
     # Get the inverse of matrix x. If inverse is not null then
     # return the cached inverse matrix
     inv <- x$getInverse() 
     if(!is.null(inv)) {
         message("Getting cached data for matrix inverse.")
         return(inv)
    } else { # if null then compute the inverse of the matrix
         message("No cached data, computing matric inverse.");
      
         # Compute the inverse of the matrix 'data' using solve function.
         # Then call setInverse to cache the inverse of the matrix and 
         # return the computed inverse
         data <- x$get()
         inv <- solve(data)
         x$setInverse(inv)
         inv
     }
}


##Sample Run
##> source('~/Desktop/coursera/cacheMatrix.R')
##> x <- rbind(c(1,2), c(3,4))
##> m <- makeCacheMatrix(x)
##> m$get()
##        [,1] [,2]
## [1,]    1    2
## [2,]    3    4
##> cacheSolve(m)
##No cached data, computing matric inverse.
##      [,1]  [,2]
## [1,] -2.0   1.0
## [2,]  1.5  -0.5
##> cacheSolve(m)
##Getting cached data for matrix inverse.
##      [,1]  [,2]
## [1,] -2.0   1.0
## [2,]  1.5  -0.5
##> y <- cacheSolve(m)
##Getting cached data for matrix inverse.
##> x %*% y #showing that it is identity matrix
##        [,1]         [,2]
## [1,]    1        1.110223e-16
## [2,]    0        1.000000e+00
##>
