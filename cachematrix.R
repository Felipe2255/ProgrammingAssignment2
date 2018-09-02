#We define a makeCacheMatrix proc to initialize the object x (a matrix which
#       inverse we might want), "inverse" (a variable where we would locate that
#       inverse matrix) and define a list of functions set, get, setinv and
#       getinv (to operate with that matrix x and it's inverse).

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL # inverse starts uncalculated
#set allows to define a new value for the matrix x in the parent environment of
        #makeCacheMatrix
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
#get just expreses the value of x
        get <- function() x
#setinv assigns the value of the inverse of the setted x to the parent directory
        setinv <- function(minverse) inverse <<- minverse
#getinv retrieves, when invoked, the value of the inverse of x from the parent
#       directory
        getinv <- function() inverse
#Creates a named list to allow manipulation of it's objects with the "$" notation
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


#The cacheSolve function actually calculates the inverse of the set matrix "x"
#using the "solve" function

cacheSolve <- function(x, ...) {
#       if the inverse variable is not null in the parent directory, then
#       cacheSolve returns it's value
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
#       if "inverse" is null, then cacheSolve calculates a new inverse getting the
#       value of x with x$get, and then applying the solve function
        data <- x$get()
        inverse <- solve(data, ...)
#       then it sets the new value of the inverse matrix to "inverse" variable
#       and prints this result
        x$setinv(inverse)
        inverse
}        