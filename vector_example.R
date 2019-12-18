## FROM README.md

# Basically the function can return the data or mean
# It doesnt calculate the mean, it only stores the result
# 
# The meancache checks if the object knows its mean, if yes it returns it
# If no it calculates and stores it.


# think of the function as a class, it returns a list of objects that
# remember data using internal namespace
makeVector <- function(x = numeric()) {
    m <- NULL  # set mean to NULL when created
    set <- function(y) {  # unused
        x <<- y
        m <<- NULL
    }
    get <- function() x  # return x from object creation  
    setmean <- function(mean) m <<- mean  # used to remember mean
    getmean <- function() m  # return mean
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)  # allows stored function to act like class
}

cachemean <- function(x, ...) {
    m <- x$getmean()  # Get the mean from provided object
    if(!is.null(m)) {
        message("getting cached data")
        return(m)  # return the mean stored by object
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)  # set the mean of the stored object
    m # return calculated mean
}