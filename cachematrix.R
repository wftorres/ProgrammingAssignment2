## Programming assignment - R Programming Oct 2014
## by Fernando Torres

## The MakeCacheMatrix creates a special "matrix" object that can cache its inverse
## that is, calculates the inverse of a matrix sent as a parameter and stores it in memory
## for later use by other functions.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            ## The '$set()' element changes the variable to be calculated and clears its inverse
            ## It allows to change the value of the matrix to be inverted without having to call
            ## The 'makeCacheMatrix' function again
            x <<- y            
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
      
}


## This function uses the special matrix from the former function
## If the inverse has already been calculated and cached, it gets the cached data
## Otherwise, it calculates the inverse and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      ## x is the special "matrix" returned by the former function
       m <- x$getinverse()
       if(!is.null(m)) {
             ## x$getinverse() will have a null value the first time the function is executed
             ## therefore this condition will never be true the first time
             ## From the second time on, though, it's faster to retrieve data from memory
             ## than to calculate the inverse
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
      
}
