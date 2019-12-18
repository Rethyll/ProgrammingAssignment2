## The functions create a special matrix that can store it's inverse. They
## also calculate the inverse. The intention is to reduce the number of matrix
## inversions calculated as they are a costly operation.

## This function stores a matrix and (optionally) it's inversion. It does not
## calculate the inversion. Instead it provides functions to change and retrieve
## the matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
    # returns [get matrix, set matrix, get inversion, get inversion]
    # inv is the inversion.
    # x in the matrix
    inv <- NULL
    
    get_matrix <- function() x
    set_matrix <- function(x) {
        inv <<- NULL
        x <<- x
    }
    get_inverse <- function() inv
    set_inverse <- function(inverse) inv <<- inverse
    list( get_matrix = get_matrix,
          set_matrix = set_matrix,
          get_inverse = get_inverse,
          set_inverse = set_inverse)
}


## This function accepts a makeCacheMatrix as an argument
## It checks if the matrix holds it's inverse. It returns the stored inverse.
## If it doesn't then it calculates and stores the inverse. It returns
## the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inverse()
        if (!is.null(inv)) {
            print('Returning stored inverse.')
            return(inv)
        }
        matrix <- x$get_matrix()
        inv <- solve(matrix, ...)
        x$set_inverse(inv)
        inv
}
