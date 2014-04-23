
## makeCacheMatrix is setting up four functions in a list to calculate an inverse
## matrix in an economical way

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # initializing before function set
        
                # four functions are made: get, set, getinverse and setinverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
                # a list is made with the four functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve calculates the inverse of a matrix, when that hasn't been done earlier, otherwise it will 
## get the inverse from cache
## cachSolve uses the functions made in the list from makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        ## checking through getinverse if the inverse has been calculated before
        ## if it has, it doesn't calculate again
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}
