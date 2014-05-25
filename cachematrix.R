## This function creates a matrix object to cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix  <- NULL
        set  <- function(y){
          x <<- y
          inverseMatrix <<- NULL 
        }
        
        get  <- function() x
        setinverse  <- function(inverse) inverseMatrix  <<- inverse
        getinverse  <- function() inverseMatrix
        list(set= set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
  
}


## Checks if an inverse is already computed, if not it computes the 
## inverse and returns the same

cacheSolve <- function(x, ...) {
        inverseMatrix  <- x$getinverse()
        if (!is.null(inverseMatrix)){
          message("getting cached data")
          return(inverseMatrix)
        }
        data  <- x$get()
        inverseMatrix  <- solve(data, ...)
        x$setinverse(inverseMatrix)
        inverseMatrix
}
