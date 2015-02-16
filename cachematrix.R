## Create a matrix with the possibility to store its inverse
makeCacheMatrix <- function (x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        ## reset inverse
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setInv <- function(inverse) {
        inv <<- inverse
    }
    getInv <- function() {
        inv
    }
    list(set = set,
        get = get,
        setInv = setInv,
        getInv = getInv)
}


## Set the inverse of a square matrix if it is NULL
cacheSolve <- function(x, ...) {
    ## get inverse of x
    inv <- x$getInv()
    if(is.null(inv)) {
        ## Inverse has not been calculated
        m <- x$get()
        inv <- solve(m, ...)
        x$setInv(inv)
        return(inv)
    } else {
        ## Inverse has been calculated
        return(inv)
    }
}
