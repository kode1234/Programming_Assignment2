
##There are two main functions in this script. 
##MakeCacheMatrix and CacheSolve

##MakeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## Here we want to firstly, set the value of the input matrix.Secondly, get the values
## of the input matri. Thirdly, Set the value of the inverse of the input matrix and finally
## return the value of the input matrix inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Define function to set the value of the special matrix. It also clears the old
  # inverse from the cache
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The CacheSolve does return an inverse of the matrix after the makeCacheMatrix above is run for square matrices.
## Once the inverse is calculated and and matrix elements have not change then the cacheSolve should obtain the 
## the inverse from the cache. if there is no inverse is not cache then NULL, and the inverse of the special matrix
## is calculated using the solve function, then and the inverse is set in the cache using the x$setinverse() function.




cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    invessage("obtaining cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


###TEST SCRIPT
A <- diag(7,5)
A
cachematricA <- makeCacheMatrix(A)
cacheSolve(cachematricA)
