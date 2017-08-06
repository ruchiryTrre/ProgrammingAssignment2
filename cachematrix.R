## This script creates a matrix that can cache the inerse of given matrix

## function to create and initialise getter and setter for main 
## and inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inversematrix <- NULL
        
        ## set the main matrix in a variable.
        set <- function(x) {
                x <<- y
                ## if the matrix is set, then reset the 
                ## any earlier cache
                inversematrix <<- NULL
        }
        
        ## return the main matrix
        get <- function() x 
        
        ## set the inverse matrix
        setinverse <- function(im) inversematrix <<- im
        
        ## return the cached inverse matrix
        getinverse <- function() inversematrix
        
        ## returns all the function variable.
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## function to return the cached inverse. It will compute the inverse of 
## matrix if not found in cache.

cacheSolve <- function(x, ...) {
        ## get inverse of matrix from cache
        i <- x$getinverse()
        
        ## if the value is NULL, then cache does not have the inveser. 
        ## need to compute the inverse
        if(is.null(i)) {
                print("computing matrix inverse...")
                data <- x$get()
                
                ## the inverse is possible only if matrix is a square matrix.
                ## even if matrix is square the inverse will not be computed
                ## if matrix is singular. 
                if(nrow(data) == ncol(data)) {
                        i <- solve(data)
                        x$setinverse(i)
                }
                else {
                        #matrix inverse not possible, becuase this is not a
                        #square matrix.
                        print("ERROR : inverse is not possible for the given matrix")
                }
        }
        
        ## retutn the inverse
        return (i)
}
