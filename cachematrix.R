##
## Description: This file contains two functions to cache the inverse of a 
##              matrix. These 2 functions assume that the matrix supplied is 
##              always invertible.
##
##              1. makeCacheMatrix(): It builds a special "matrix" 
##                 R object that stores a matrix and its inverse.
##              2. cacheSolve(): It requires an argument that is returned by 
##                 makeCacheMatrix() in order to retrieve the inverse from the 
##                 cached value that is stored in the makeCacheMatrix() object's
##                 environment.
##
## Version: 1.0
##


#Function Name: makeCacheMatrix()
#Description: It builds a special "matrix" R object that can cache its inverse. 
#             In addition, it builds a set of functions and returns the
#             functions within a list to the parent environment. 

makeCacheMatrix <- function(x = matrix()) {  # initialize x object
        inverse_m <<- NULL      # initialize inverse_m object  
        set <- function(y) {    # function to initialize a new matrix (reset it)
            x <<- y             # x stores the matrix in parent environment
            inverse_m <<- NULL
        }
        get <- function() x     # retrieve the matrix from parent environment 
        setinversematrix <- function(inverse) inverse_m <<- inverse
                                # store passed-in parameter in parent 
                                # environemnt
        getinversematrix <- function() inverse_m
                                # retrieve the calculated inverse of the matrix
                                # from parent environment
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
                                # build a set of functions and returns the 
                                # functions within a list to the parent 
                                # environment. 
    }


# Function Name: cacheSolve()
# Description: It is designed to populate and/or retrieve the inverse from an 
#              object of type makeCacheMatrix().

cacheSolve <- function(x, ...) {
        m <- x$getinversematrix()  # call the getinversematrix function on the 
                                   # input object.
        if(!is.null(m)) {          # check if the inverse is not NULL
            message("getting cached data")
            return(m)
        }
        # if the inverse is NULL, then retrieve the matrix, compute the inverse,
        # store it in parent environment & return it to caller.
        data <- x$get()
        m <- solve(data)
        x$setinversematrix(m)
        m
}


    
