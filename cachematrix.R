## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This one just create a matrix and catch it

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL            ## First the function saves m, to use it later
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x ##This function just retrieves the value of x
        setInverse <- function(inverse) m <<- inverse ##"inverse" is assigned to a free variable
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        m <- x$getInverse()  ## m is the inverse of a matrix x
        if(!is.null(m)) {
                message("getting cached data")
                return(m)    ## Cache Solve will retrieve the inverse from the 
        }                    ## cache if it has been calculated, and also a message
        mat <- x$get()
        m <- solve(mat, ...)
        x$setInverse(m)
        m                     ## Finally it´ll also returns m
}

