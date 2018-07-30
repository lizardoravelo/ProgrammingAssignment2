## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function create a inverse matrix in the parent environment and check if everything is correct

makeCacheMatrix <- function(x = matrix()) {
          inverse <- NULL
          
          set <- function(y){
             x <<- y
             inverse <<- NULL
          }
          
          get <- function() x
          setInverse <- function(matrix) inverse <<- matrix
          getInverse <- function() inverse
          list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## this function check if the inverse exist in cache if not then its calculated and then stored.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inverse <- x$getInverse()
       if(!is.null(inverse)){
              message("getting cached data")
              return(inverse)
       }
       data <- x$get()
       inverse <- solve(data,...)
       x$setInverse(inverse)
       inverse
}