## makeCacheMatrix and cacheSolve functions are used to get the inverse of matrix using cache
## makeCacheMatrix function gives the matrix information
## cacheSolve function gives the result of makeCacheMatrix and also shows the result as cached if it is cached.

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of matrix(solve)
## get the value of the inverse of matrix(solve)


makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve calculated the inverse of the matrix of special "matrix" created from above function.
## The function will chech if the inverse is already calculated or not. If it is already calculated, then return the result from cache
## and avoid the computation.
## Else, it will calculate the inverse of the matrix and sets the value of the inverse in the cache i.e the setsolve function.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        
        ## Return a matrix that is the inverse of 'x'
}
