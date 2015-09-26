## Two funcitons are defined here. These funcitons help in defining a square matrix 
##     and  calculating inverse of it. Both input matrix and inversed matrix are cached 
##     so that there is no need to compute invese everytime when needed.
## 

## makeCacheMatrix funciton helps in creating a specialised matrix which does following-
##    i. Set the value of matrix , matrix is cached
##    ii. Get value of the matrix
##    iii. Set inverse of the matrix, inverse matrix is cached
##    iv. Get inverse value of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(y) i <<- y
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve function uses the specialised matrix which is created using makeCacheMatrix.
## It performs following tasks-
##    i. Return inverse matrix from cache if available 
##    ii. Calculate inverse of matrix if it does not exist in cache. It also sets the inverse matrix in specialised matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
