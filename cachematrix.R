## This file includes two packages. The first package takes as input a square matrix  
## then it cretes two setter functions and two getter functions to be used for calculating
## the inverse of that square matrix. The second function caches the inverse of the matrix
## so that if it is needed fo firther computation, it can be used from the cache saving
## some expensive computation. This will occur as long as the matrix has changed.

## This function takes as input a matrix. such matrix is expected to be square in order 
## to calcualte the inverse. The function sets the variable m to null. For the other functions
## to access that variable, they need the superassignemnt operator <<-. The Function defines
## four internal functions namely set() which can be used to assign the matrix to variable
## x and it initiates m to null again. the function get() retrieves the matrix. The function
## setinverse() set the inverse for the square matrix and the function getinverse() retrieves 
## that inverse by simply retrieving m. Notice that the inverse is calculated in the second 
## function cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse<- function(solve) m <<- solve
  getinverse<-function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes a matrix which is expected to be square and it returns a matrix that
## is the inverse of x. If the inverse found to have been calculated (not null), then
## it is retrieved from the cache; other wise it is calculated. Before inverse is calculated, 
## the matrix is checked to be square to avoid throwing an error. More checks can be implemented
## especially to check if the matrix is singular. 

cacheSolve <- function(x, ...) {
        
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) ## Return the calculated inverse of 'x'
  }
  data <- x$get()
  if (ncol(data) != nrow(data)){      ## check if the matrix is square
    message("data must be a square matrix")
    break
  }
  m <- solve(data, ...)
  x$set(m)
  m   ## Return the calculated inverse of 'x'
  
}
