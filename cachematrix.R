## I have produced two functions. 
## When used together, they take a matrix and return its invers


##makeCacheMatrix takes a matrix and returns a list with four object. Each object
##is a function. The first function resets the objects within the function. The 
## second function returns the matrix. The third sets the object inv and the 
## returns the object inv.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##cacheSolve calculates the inverse of a matrix. It is called using the object 
##created by makeCacheMatrix, which is a list. cacheSolve first looks to see if
##the inverse matrix is stored in the cache. If it is not, it calculates it anew.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
