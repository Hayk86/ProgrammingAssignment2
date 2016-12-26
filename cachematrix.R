## makeCacheMatrix faunction creates a special list
## that contains four faunction to:
## set the value of matrix
## get the value of matrix
## set the inverse matrix
## get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {      
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        get <- function() x      
        setinverse <- function(solve) s <<- solve       
        getinverse <- function() s        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function calculates the inverse of the special matrix
## Unless the inverse is already calculated, in which case 
                ## the cached inverse matrix is returned
                
cacheSolve <- function(x, ...) {
        
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        } 
        data <- x$get() 
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
