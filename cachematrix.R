## Put comments here that give an overall description of what your
## functions do

## Create a special Matrix object and cache the Inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        ## Initialize the cache Matrix (cMatrix) and set to NULL
        ## Define function setcMarix to assign the input matrix
        
        cMatrix <- NULL
        setcMatrix <- function (y) {
                x <<- y
                cMatrix <<- NULL
        }
        
        ## Return the input matrix "x" 
        ## Using solve function get the inverse of the cMatrix and Return the inversed matrix of "x"
        
        
        getcMatrix <- function() x
        setcMatrixInverse <- function(solve) cMatrix <<- solve
        getcMatrixInverse <- function() cMatrix
        
        ## List the assigned matrices with the names 
        
        list(setcMatrix = setcMatrix, getcMatrix = getcMatrix,
             setcMatrixInverse = setcMatrixInverse,
             getcMatrixInverse = getcMatrixInverse)
}


## The below function checks if the given input matrix is already inversed. 
## If not, it then computes the inverse of the input matrix

cacheSolve <- function(x, ...) {
        
        ## Check if the input matrix is already inversed into the cache matrix
        ## If so, it returns value from the cached matrix, else moves past the below "if" statement
        
        cMatrix <- x$getcMatrixInverse()
        if(!is.null(cMatrix)) {
                message("Getting from cached matrix")
                return(cMatrix)
        }
        
        ## Gets the cached Matrix and using "Solve" function it inverses the input matrix
        dataMatrix <- x$getcMatrix()
        cMatrix <- solve(dataMatrix, ...)
        x$setcMatrixInverse(cMatrix)
        cMatrix
        
}
