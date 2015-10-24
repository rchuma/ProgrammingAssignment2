##creates a function which caches matrix
# This function will do the following:
#1. Sets inverse matrix to null
#2. Creates new matrix or change existing one
#3. Change the value of inverse matrix
#4. Get the value of the inverse
#5. Calculates the inverse of matrix via solve
#6. Get the inverse matrix
#7. passes the value of the function makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) { 
        matrixinverse <- NULL    # 1.                 
        set <- function(y) {    #2.                  
                x <<- y
                matrixinverse <<- NULL   #3.           
        }
        get <- function() x     #4.                      
        setinverse <- function(solve) matrixinverse <<- solve #5.
        getinverse <- function() matrixinverse     #6.   
        list(set = set, get = get,                    
             setinverse = setinverse,
             getinverse = getinverse) #7.
}

# Function for getting the cache of the matrix will do:
#1. Gets the inverse if it exists (not null)
#2. Calculates the inverse if it doesn't exist
cacheSolve<- function(x, ...) {  #1.               
        matrixinverse <- x$getinverse()
        if(!is.null(matrixinverse)) {                 
                message("getting cached data - Inverse of the matrix")
                return(matrixinverse)
        }
        data <- x$get()    #2.                           
        matrixinverse <- solve(data, ...)
        x$setinverse(matrixinverse)
        matrixinverse
}
