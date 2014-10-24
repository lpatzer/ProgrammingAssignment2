#######  BEGIN ANSWER

## Below are a series of functions that can inverse a matrix and cache the findings

## Creating a function that inverses a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- c()   ## The inverse starts out as empty so it overwrites any previously done calculations
  set <- function(y) {
    x <<- y
    inv <<- c()
  }
  get <- function() x    ## Will return the defined matrix
  setInverse <- function(inverse) inv <<- inverse   ## Finding the inverse of the matrix
  getInverse <- function() inv
  list(set = set, get = get,      ## putting all obtained objects into a list
       setInverse = setInverse,
       getInverse = getInverse)
}

## Identify if the matrix has changed.  If it has, return a new inverse matrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()    ## Obtaining the inverse of the matrix used previously
  if(!is.null(inv)) {      ## Identifying if the matrix has changed, if not informing user
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)  ## Calculating the new inverse
  x$setInverse(inv)
  inv
}





###### TESTING

amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
amatrix$get()
cacheSolve(amatrix)
amatrix$getInverse()
cacheSolve(amatrix)
amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) 
cacheSolve(amatrix)
amatrix$get()
amatrix$getInverse()




