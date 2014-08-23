## Make Matrix Function
#  inv = the inverse of the matrix, can be cached
# makeCacheMatrix creates a list functions to: set value of matrix, get value of matrix, set value of inverse, get value of inverse

makeCacheMatrix <- function(x = numeric()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() {x}                           #get the matrix
      setinv <- function(inverse) inv <<- inv         #set the inverse
      getinv <- function() inv                        #get the inverse
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## CacheSolve Function
# this does the actual work using the functions above

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {                       #check routine
            message("getting cached data")
            return(inv)
      }
      data <- x$get()                           #calculing the inverse
      inv <- solve(data, ...)                   #the actual solve command
      x$setinv(inv)                             #Cache the answer
      inv                                       #return inv (the answer)
}


