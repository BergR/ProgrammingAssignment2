## Put comments here that give an overall description of what your 
## functions do
## The first function creates a vector that contains 6 elements
## It contains 4 functions and 2 data objects x (matrix) and IM (inverse matrix)
## The second function calculates the inverse of the matrix. When it has already
## been calculated, it is taken from the cache


## Write a short comment describing this function
## The input for this function is a matrix. The output is the matrix, indication
## if inverse has been calculated, and 4 functions (set matrix, get matrix, set
## inverse matrix and get inverse matrix)

makeCacheMatrix <- function(x = matrix()) {
        IM <- NULL
        ## When a new matrix is given as input variable
        setMatrix <- function(y) {
                IM <<- NULL
                x <<- y
        }
        getMatrix <- function() x
        setInverseMatrix <- function(InverseMatrix) IM <<- InverseMatrix
        getInverseMatrix <- function() IM
        list(setMatrix=setMatrix, getMatrix=getMatrix,
             setInverseMatrix=setInverseMatrix, getInverseMatrix=getInverseMatrix)
}


## Write a short comment describing this function
## This function calculates the inverse of the matrix. Before calculating it, it first
## checks the cache if it has already been calculated before. When that is the case,
## it takes the information from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        IM <- x$getInverseMatrix()
        if(!is.null(IM)){
                message("getting cached data")
                return(IM)
        }
        data <- x$getMatrix()
        IM <- solve(data, ...)
        x$setInverseMatrix(IM)
        IM
}