## makeCacheMatrix creates a matrix to be inverted and have the inverse cached

## creates, gets and sets a square invertable matrix and the inverse functions
## includes NULLing out the inverse if it already exists from previous runs

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y) {
                x<<-y
                inv<<-NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## tests for existance of inverse
##if not present, creates and caches it
##if present, uses cached version

cacheSolve <- function(x, ...) {
        inv=x$getinv()
        
        if(!is.null(inv)) {
                message("Using cached inverse")
                return(inv)
        }
        original = x$get()
        inv=solve(original,...)
        x$setinv(inv)
        return(inv)
        ## Return a matrix that is the inverse of 'x'
}
