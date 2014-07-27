## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## based on the example, this function have the same structure, having a list of functions as its return.
## set = initialization
## get = argument matrix
## setinv = function that compute the solve function
## getinv = store the result (inverse matrix)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## Write a short comment describing this function
## this function basically tests if the inverse matrix is already cached and if so it return, otherwise it calculate.
## I don´t think that there is a reason for checking if the matrix had changed cause in each use of makeCacheMatrix object it set 'm' to NULL and that is the 
## trigger to solve again if it is needed.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}