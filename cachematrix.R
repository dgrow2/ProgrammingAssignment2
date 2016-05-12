# This function will cache the inverse of a matrix, so other functions can use it without
##having to recalculate
makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
          #caches the variables X and i, so they can be used by other functions
     }
     get <- function() x
     setinverse <- function(solve) i <<- solve
     getinverse <- function() i
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#This function checks to see if there is a value for i, if not, then it calculates the inverse of x. if there is a value, it will return that value
cacheSolve <- function(x, ...){
        i <- x$getinverse()
        #calls the function getinverse which is the previous function is defiened as the inverse of x, which is defined as i in this environment
        if(!is.null(i)) {
             #checks to see if i has already been calculated, or is null
                message("getting cached data")
                return(i)
                    #if i has already been calculated, it will call it from the environment
        }
        matrix.data <- x$get()
        #if i has has not been calculated it will call the function get, which pulls the matrix, x and assigns it a name "matrix.data"
        i <- solve(matrix.data, ...)
        #defines i has the inverse of matrix.data
        x$setinverse(i)
        #calls the function setinverse, whcih will cache i into the environment for future iterations
        return(i)
        #prints the newly calculated inverse matrix, i
}