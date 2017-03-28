## This script contains the functions for the Programming Assignment 2
## The first function creates a special "matrix" object which is a list 
## that contains four functions. More details below.
## The second function takes a "matrix" created with the first function
## and returns the inverse of this matrix. More details below. 


## makeCacheMatrix() takes a matrix and returns a list with four functions: 
## set(), get(), setInverse(), getInverse() used to 
## set the value of the matrix, get the value of the vector,
## sets the value of the inverse, gets the value of the mean, respectively.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # inv is the inverse of the matrix
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}


## The following function calculates the inverse of the "matrix" created by
## makeCacheMatrix() defined above. If the "matrix" already contains the 
## inverse it simply returns that value.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(a = mat, b = diag(nrow(mat)), ...) 
        # set b to identity matrix. See ?solve for details
        x$setInverse(inv)
        inv
}
