## makeCacheMatrix craeates a special matrix that can cache its inverse

## makeCacheMatrix set the value of the matrix, get the value of the matrix,
## set the value of inverse and get the value of inverse

makeCacheMatrix <- function(x = matrix()) { 
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list( set = set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)

}


## cacheSolve function computes the inverse of the special matrix
##  if the inverse has already been calculated it  retrives the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
        ## Return a matrix that is the inverse of 'x'
}
