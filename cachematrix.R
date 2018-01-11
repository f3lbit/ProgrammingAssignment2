solve## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## My comments are written below, within the function
makeCacheMatrix <- function(x = matrix()) {

  s <- NULL                    ##set pointer S to NULL, meaning that new vector was created or re-created
  set <- function(y) {         ##This funcion re-set the pointer of x to new pointer y
    x <<- y
    s <<- NULL
  }
  get <- function() x          ##This function returns the pointerto x, given when the makeCacheMatrix was last called
  setsolve <- function(y) s <<- y     ##Set y to pointer s, so s is not NULL anymore
  getsolve <- function() s
  list(set = set, get = get,   ##This returns a list with pointers to the each internal function.
       setsolve = setsolve,
       getsolve = getsolve)
  
  
}


## Write a short comment describing this function

## My comments are written below, within the function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s1 <- x$getsolve()     ##It takes the pointer from the list returned by makeCacheMatrix to the function getSolve(), which returns the pointer which is the variable s within the environment of function makeCacheMatrix and stores it in variable s1
  if(!is.null(s1)) {     ##In the first call to cacheSolve, the s1 variable is NULL, because s was set NULL everytime the function makeCacheMatrix(x) is called
    message("getting cached data")
    return(s1)           ##In the case s1 is not NULL, it returns s1, which is the inverted matrix
  }
  data <- x$get()       ##In the case it was NULL, the data is the pointer to the matrix x, which was given by the previous call to makeCacheMatrix(x)
  s1 <- solve(data, ...)    ##Here we call the function solve(x matrix) to get the inverted matrix saved into the pointer of s
  x$setsolve(s1)         ##Writes s1 solved Matrix into the pointer s within makeCacheMatrix environment
  s1                     ##return the calculated inverted matrix
}
