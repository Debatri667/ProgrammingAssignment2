## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix creates an object of class matrix that can also cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i<- NULL
        set <- function(z){
  x <<- z
  i <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)

}


## The function cacheSolve calculates the inverse of the matrix which has been returned by makeCacheMatrix  function above. 
## If the inverse of that matrix has already been calculated and there has been no change in the matrix, 
## then the cacheSolve can retrieve the inverse from the cache instead of calculating it again

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
  if(!is.null(i)){
  message("getting cached data")
  return(i)
  }
  matrix <- x$get()
  i <- solve(matrix,...)
  x$setInverse(i)
  i

}
