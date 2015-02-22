## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This makeCacheMatrix function takes a matrix and creates an object of type 'list'. This list contains four functions.

## The 'set' function assigns a new matrix to the function. 
## The 'get' function returns the value of the original matrix.
## The 'setinv' function is called by cacheSolve() when run for the first time and stores the inverted matrix.
## The 'getinv' function returns the cached inverted matrix.

makeCacheMatrix <- function(x = matrix()) { # input x will be a matrix

        inv <- NULL # inv will be the inverted matrix and it is reset to NULL every time makeCacheMatrix is called.
        
        # the 'set' function is not accessed by 'cacheSolve()'. It lets you assign a new matrix to the object.
        
        set <- function(newMatrix) { # takes an input matrix, 'newMatrix' is an arbitrarely anonymous name.
                
                x <<- newMatrix # saves the input matrix
                inv <<- NULL # resets the inverted matrix to NULL, when a new object is created.
                
        }
        
        # these next three functions are defined but not run when makeCacheMatrix is called.
        # instead, they will be used by cacheSolve() to get values for x or for
        # inv (solve(x)) and for setting the inverse. They are called object 'methods'.
        
        get <- function() x # this function returns the value of the original matrix.
        
        setinv <- function(solve) inv <<- solve # this is called by cacheSolve() during the first cacheSolve() access
                                                # and it will store the inverted matrix using superassignment
        
        getinv <- function() inv # this will return the cached inverted matrix to cacheSolve() on subsequent accesses.
        
        # the list below is accessed each time a new object is made by means of calling makeCacheMatrix().
        # this is a list of the internal functions ('methods') so a calling function knows how to access those methods.
        
        list(set = set, get = get, setinv = setinv, getinv = getinv) # an object of type 'list' is created.

}


## Write a short comment describing this function

## cacheSolve returns an inverse matrix. It takes as input a list created by makeCacheMatrix.
## This function first verifies whether an inverse matrix was already calculated before. If so, it will get and return
## the inverted matrix, rather than calculating it again. As such this avoids unneccessary computation effort.
## When this function is called the first time and no inverse matrix was calculated before, it will get the origingal matrix,
## calculate the inverse and store the inverted matrix back in x.

cacheSolve <- function(x, ...) {# the input x is an object of type 'list' created by makeCacheMatrix.
        ## Return a matrix that is the inverse of 'x'
        
        # x is just a token, a placeholder for the object name and will be replaced by the name of the object
        # when cacheSolve() is called. You may have chosen any arbitrary name for this.
        
        inv <- x$getinv() # accesses the object 'x' and gets the inverted matrix
        
        if(!is.null(inv)) { # check if the inverted matrix has been calculated and cached earlier saving computing time
                message("getting cached inverted matrix") # send this message to the console.
                return(inv) # return the inverted matrix and end the function cacheSolve().
        }
        
        data <- x$get() # fetching the original matrix, only if there was no inverse calculated before, i.e. x$getinv is NULL
        inv <- solve(data, ...) # calculate the inverse if it wasn't calculated before
        x$setinv(inv) # store this inverted matrix back in x
        inv # return inv to the code that called this function
}
