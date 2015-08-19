
## This function makes a list with stub functions

makeCacheMatrix <- function(x = matrix()) {
        
        inverse = NULL
        set = function(y) {

                x <<- y
                inverse <<- NULL
        }
        get = function() x
        setinverse = function(inverserse) inverse <<- inverserse 
        getinverse = function() inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the cached data

cacheInverse <- function(x, ...) {

        
        inverse = x$getinv()
        
        if (!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        
        mat.data = x$get()
        inverse = solve(mat.data, ...)
        
        x$setinv(inverse)
        
        return(inverse)
}
