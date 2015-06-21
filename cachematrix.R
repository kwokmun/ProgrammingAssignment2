## Author: Kwokmun Lee
## Date: June 20, 2015

## This function accepts a square matrix as an argument, 
## and has four separate functions that enables users to:
## 1. Get the matrix that was passed.
## 2. Set a different matrix to overwrite the one that was passed.
## 3. Get the inverse of the matrix (value is NULL if it's not available).
## 4. Set the inverse of the matrix by overwriting the current object.
##    This in effect caches the inverse.
## It will return a list of functions that will be used in 
## the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinv <- function(inv) m <<- inv
     getinv <- function() m
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## This function accepts the list of functions from makeCacheMatrix()
## and outputs the inverse of the cached matrix. It checks to see if a 
## cached version is available. If it is, it returns the cached output.
## If it's not available, then it calculates the inverse from scratch
## and caches it.

## Note: Run makeCacheMatrix() once to cache the matrix. Then you can 
##       run cacheSolve() to calculate the inverse. If you wish to test 
##       this on another square matrix, be sure to run makeCacheMatrix()
##       once before running cacheSolve()

# Sample Output:
#      
# > source("cachematrix.R")
# > cac <- makeCacheMatrix(matrix2)
# > cacheSolve(cac)
# [,1] [,2]
# [1,] -12.5   12
# [2,]  11.5  -11
# > cacheSolve(cac)
# getting cached data
# [,1] [,2]
# [1,] -12.5   12
# [2,]  11.5  -11
# > cac <- makeCacheMatrix(matrix3)
# > cacheSolve(cac)
# [,1] [,2] [,3]
# [1,]   -6  3.6  1.4
# [2,]    5 -3.0 -1.0
# [3,]   -1  0.8  0.2
# > cacheSolve(cac)
# getting cached data
# [,1] [,2] [,3]
# [1,]   -6  3.6  1.4
# [2,]    5 -3.0 -1.0
# [3,]   -1  0.8  0.2

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinv()

     ## Returns cached data if available. If not,
     ## compute the inverse and cache it.
     if(!is.null(m)) {
          message("getting cached data")
     }
     else {
     data <- x$get()
     m <- solve(data, ...)
     
     x$setinv(m)
     }
     m
}
