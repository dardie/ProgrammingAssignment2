## Put comments here that give an overall description of what your
## functions do

## Create a special "matrix",
## actually a list containing functions to
## add values, get values, set & get the inverse
## value of inverse is cached for efficiency

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function (matr) {
        x <<- matr
        i <<- NULL
    }

    get <- function() { x }
    setinv <- function(inv) { i <<- inv }
    getinv <- function() {i}
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## Check if inverse has been set for x
## If NO calculate and store inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if (!is.null(i)) {
        message ("Getting cached data")
        return(x)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}

## Testing:
## matr <- makeCacheMatrix(matrix(1:4,2,2))
## matr$get()
## matr$getinv()
## cacheSolve(matr)
## matr$getinv()
## matr$getinv() %*% matr$get()
## Final line should return 2x2 identity matrix
