## makeCacheMatrix stores a matrix object and its inverse (calculated by cacheSolve function). Set functions store 
## matrix and inverse,whike get functions give appropriate values back
 

## makeCacheMatrix: Stores matrix and its inverse, if already calculated. Get-s do direct read of values

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL ##initialize inverse matrix
  set <- function(y) {     ##explicite set for matrix value
    x <<- y      ##value passed to parent environment variable        
    m <<- NULL ##when explicite set used, inverseMatrix reinitialized, also passed to parent env.
  }
  get <- function() x  ##get for getting value of matix
  setInverse <- function(solvedMatrix) inverseMatrix <<- solvedMatrix ##put cacheSolve output to inverseMatrix variable
  getInverse <- function() inverseMatrix ##get for getting value of cached matrix
  list(set = set, get = get,   ##named list for methods calling 
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: Checks whether inverse has been already stored or not. If not, it calculates and writes back through 
## makeCacheMatrix$setInverse method

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()  ##direct read of inverseMatrix of object
  if(!is.null(inverseMatrix)) {  ##check if we got back non-NULL
    message("Getting stored inverse matrix...")
    return(inverseMatrix)  ##if non-NULL, give back stored value
  }
  data <- x$get() ##direct read original matrix, if inverse was not found
  inverseMatrix <- solve(data, ...) ##solve original matrix
  x$setInverse(inverseMatrix) ##putting back to inverseMatrix via setInverse method
  inverseMatrix ##return
}
