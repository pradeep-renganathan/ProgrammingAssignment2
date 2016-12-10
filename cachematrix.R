## Put comments here that give an overall description of what your
## functions will set the value and get the value. Instead of calculating value agaoin in other function, we simply retrieve the return the cached value.

## Write a short comment describing this function
# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    # inv will store the cached inverse matrix
    inv <- NULL

    # Setter for the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
}
    # Getter for the matrix
    get <- function() x

    # Setter for the inverse
    setinv <- function(inverse) inv <<- inverse
    # Getter for the inverse
    getinv <- function() inv

    # Return the matrix with our newly defined functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Write a short comment describing this function
# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()

    # If the inverse is already calculated, return it
    if (!is.null(inv)) {
message("getting cached data")
        return(inv)
}

    # The inverse is not yet calculated, so we calculate it
    data <- x$get()
    inv <- solve(data, ...)

    # Cache the inverse
    x$setinv(inv)

    # Return it
    inv
}
