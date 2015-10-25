## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix acts like a matrix Class
## it consumes the ordinary matrix and allows it to be 
## passed to cacheSolve function
## cacheSolve function then can execute get/set on the data
## and getinv/setinv to execute function calls to either retrive the data or
## calculate it if not already stored.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    

}


## cacheSolve consumes the object of class makeCacheMatrix, and then either
## calls for calculation of matrix if solution is not stored on passed matrix
## or retrives stored solution of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    #here is the inverse calculation
    m <- solve(data, ...)
    x$setinv(m)
    m
    
}


