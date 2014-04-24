## Functions to calculate and cache a matrix and its inverse
## 

## makeCacheMatrix cache a matrix and the functions needed for retrieval 

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinvm <- function(z) invm <<- z
    getinvm <- function() invm
    list(set = set, get = get,
         setinvm = setinvm,
         getinvm = getinvm)
}


## cacheSolve retrieves the cached version of the inverse matrix or calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invm <- x$getinvm()
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    mat <- x$get()
    invm <- solve(mat, ...)
    x$setinvm(invm)
    invm
}
