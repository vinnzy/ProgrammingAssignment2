## makeCacheMatrix implements caching of matrix inversion
## cacheSolve solves matrix cache.

## This functions does the following:
## Implements matrix inversion caching and provides a setter
## and getter functions.

makeCacheMatrix <- function(x = matrix()) {
  
  cachedInverse <- NULL
  
  set <- function (y) {
    x <<- y
    cachedInverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) {
    cachedInverse <<- inverse
  }
  getinverse <- function() {
    cachedInverse
  }
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## This function solves matrix inversion.
## It will initially check if cached data is available.
## If yes, then retrurns cached data and if no then solves and tehn returns the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  cachedInverse <- x$getinverse()
  if (!is.null(cachedInverse)){
    message("getting cached data")
    return (cachedInverse)
  }
  
  x.data <- x$get()
  cachedInverse <- solve(x.data,...)
  x$setinverse(cachedInverse)
  cachedInverse
}

test = function(mat){
  ## @mat: an invertible matrix
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}