## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        matrixinverse <- NULL                     
        set <- function(y) {                      
                x <<- y
                matrixinverse <<- NULL              
        }
        get <- function() x                           
        setinverse <- function(solve) matrixinverse <<- solve 
        getinverse <- function() matrixinverse        
        list(set = set, get = get,                    
             setinverse = setinverse,
             getinverse = getinverse)
}


# Function for getting the cache of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixinverse <- x$getinverse()
        if(!is.null(matrixinverse)) {                 
                message("getting cached data - Inverse of the matrix")
                return(matrixinverse)
        }
        data <- x$get()                               
        matrixinverse <- solve(data, ...)
        x$setinverse(matrixinverse)
        matrixinverse
}
