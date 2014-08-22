## These two functions allow for 1. the construction of a matrix 'object' that can cache its inverse matrix
## and 2. a function to retrieve the inverse, using the cached information if available.

## This function will create a matrix 'object' (list) that saves its inverse for fast retrieval.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        i <<- NULL
        x <<- y
    }
    get <- function() {x}
    setinv <- function(j) {i <<- j} 
    getinv <- function() {i}
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function retrieves the cached inverse of a matrix object (list), or calculates and returns 
## the inverse if it is not already cached.

## The problem template allowed for the use of ... arguments to cacheSolve, however passing 
## these arguments forward to solve() will give incorrect results, since the cached inverse 
## will not reflect this. Therefore the ... have been intentionally left out. 

cacheSolve <- function(x) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("Returning cached inverse")
        return(i)
    }
    message("Calculating inverse")
    m <- x$get()
    j <- solve(m) 
    x$setinv(j)
    j
}

# Test code
# m <- makeCacheMatrix(matrix(1:9, 3, 3))
# m$get()
# m$set(matrix(rnorm(9), 3, 3))
# m$get()
# m$getinv()
# cacheSolve(m)
# round(m$get() %*% cacheSolve(m), 2)
