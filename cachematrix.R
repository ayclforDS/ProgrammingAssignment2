## This function deals with a cached Matrix.

## This function will store a matrix and cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        
        ## NULL means no matrix inverse has been cached.
        inver <- NULL
        
        ## Caches x in another environment.
        set <- function(y) {
                x <<- y
                inver <<- NULL
                
        }
        
        ## Returns x.
        get <- function() x
        
        ## Sets the inverse of a function to this "inver" object
        ## in different environment.
        setInverse <- function(inverse) inver <<- inverse
        
        ## Returns the inverse of matrix.
        getInverse <- function() inver
        
        ## Returns a list of matrix functions.
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}

## Finds the inverse of a given matrix.
cacheSolve <- function(x, ...) {
        
        ## Returns a matrix that is the inverse of 'x'
        inver <- x$getInverse()
        
        ## If the object inver is not NULL
        ## showing the edited message
        ## otherwise dealing with the matrix directly.
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        
        ## x$get() is stored in object data.
        data <- x$get()
        
        ## Inverse of data is stored in object inver. 
        inver <- solve(data, ...)
        
        ## Caches inverse if it has been stored.
        x$setInverse(inver)
        
        ## Returns inverse of object data.
        inver
}
