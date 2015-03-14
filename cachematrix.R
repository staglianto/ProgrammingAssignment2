## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCachematrix function creates a list vector, that includes the matrix 
## to be inverted.Inside we have four subfunctions
## The <<- operator can be used to assign a value
## to an object in an environment that is different from the current environment

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
## The cacheSolve function return cached  matrix if has already been inverted
## else invert matrix and cached with setinv()
## Use subfunction defined into makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        ## if m is calculated return matrix cached
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
            }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
