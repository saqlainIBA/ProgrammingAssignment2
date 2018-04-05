
## First it initiallized the x variable with a matrix 
## Then i is set to Null for the use in future 
## The set fucntion puts the value of y into x and set i again to Null.
## Then there is a setter for inverse which saves value of inverse to i
## Then getter returns the function
## At the end its just a list that will give the values in the form of the list.

makeCacheMatrix <- function(x = matrix()) {
      
      i <- NULL
      set <- function(y){
            x <<- y
            i <<- NULL
      }
      
      get <- function()x
      
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function()i 
      
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## This function simply check if there is already a matrix inverse in cache 
## if the value is not null then it returns the value else it take the 
## inverse of the matrix by the solve function

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
             return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
