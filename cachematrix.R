## Put comments here that give an overall description of what your
## functions do

## Function:makeCacheMatrix - it creates an object of type list with the following functions
## 1. makeCacheMatrix$set which set the matrix with input data as matrix
## 2. makeCacheMatrix$set which gets the value of matrix inside
## 3. makeCacheMatrix$setinv which sets inverted matrix
## 4. makeCacheMatrix$getinv which gets inverted matrix inside 

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinv <- function(solved) invmat <<- solved
  getinv <- function() invmat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function:cacheSolve - This function makes takes an object of type list which is output 
## of above function. If its a new matrix then the inverted matrix is calculated newly. If
## its the existing matrix data and inverted matrix is already calculate, it returns the 
## cached inverted matrix. It saves time for repeated work of calculating inverted matrix
## for same matrix data.

cacheSolve <- function(x, ...) {
  invmat <- x$getinv()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinv(invmat)
  invmat
}
