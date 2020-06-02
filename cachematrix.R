## Assignment 2

## makeCacheMatrix creates a special matrix that can store its inverse

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y) {
        x <<- y
        Inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) Inv <<- inverse
    getInverse <- function() Inv 
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve checks whether the inverted matrix already exists, if not computes the inverted matrix
## In any case, returns the inverted matrix

cacheSolve <- function(x, ...) {
    Inv <- x$getInverse()
    
    # this if checkes whether an inverted matrix already exists 
    # and, if this is the case, it prints a message and return the inverted matrix
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    
    # if the previous check doesn't return any value, then the function computes the inverted matrix itself
    data <- x$get()
    Inv <- solve(data, ...)
    x$setInverse(Inv)
    Inv    

}
