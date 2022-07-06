## makeCacheMatrix and cacheSolve return an inverted matrix given as input for the makeCacheMatrix function
## these functions illustrate the mechanism of the caching with the <<- symbols

## makeCacheMatrix returns a list of functions transmited as parameters of the returned "matrix"
## myReturn$set() allows to update the input matrix
## myReturn$get() displays the input matrix
## myReturn$setinv creates the inverted matrix
## myReturn$getinv displays the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    # setinv <- function(inv) m <<- solve(x)
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve calls the makeCacheMatrix function to calculate and displax its inverted matrix
## Calling the cached data allows it to work quickly

cacheSolve <- function(x, ...) {
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
