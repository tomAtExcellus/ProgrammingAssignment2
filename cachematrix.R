## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix is a special function that can cache the inverse of a matrix
## and related operations around maintaining the cached value
##
## input - to the function is a matrix of data
##
## output - returns a list of functions that allow getting and setting of the matrix
## data, and getting and setting the matrix inverse.  If if the matrix data has not changed
## then the cached value is returned
##
makeCacheMatrix <- function(x = matrix()) {
                inverseVal <- NULL
                # if the data is changed via the set function set the cached value 
                # to null
                setMatrixData <- function(y) {
                        matrixData <<- y
                        inverseVal <<- NULL
                }
                # get the matrix data
                getMatrixData <- function() matrixData
                
                # set the matrix inverse
                setMatrixInverse <- function(y) inverseVal <<- y
                # get the Matrix Inverse
                getMatrixInverse <- function() inverseVal
                # return the list of functions that will maintain the matrix data and 
                # maintain the matrix inverse data and cahced value
                list(setMatrixData = setMatrixData, getMatrixData = getMatrixData,
                     setMatrixInverse = setMatrixInverse,
                     getMatrixInverse = getMatrixInverse)
        
}


## This functions will return the inverse of a matrix.  If the matrix data has not changed
## since the last time the matrix inverse was calculated the cached value of the matrix is 
## returned, else the new inverse is calculated and the value cached.
##
## input - a list of functions that will get and set the matrix data and the matrix inverse
##
## output - the matrix inverse.

cacheSolve <- function(x, ...) {
                matrixInverse <- x$getMatrixInverse()
                if(!is.null(matrixInverse)) {
                        message("getting cached data")
                        return(matrixInverse)
                }
                matrixData <- x$getMatrixData()
                matrixInverse <- solve(matrixData, ...)
                x$setMatrixInverse(matrixInverse)
                matrixInverse
}
