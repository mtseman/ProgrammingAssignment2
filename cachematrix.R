## THese two functions create a "special" matrix that allows the use of cache
## so that the inverse can be pulled from memory and not have to be calulated again
## Below the two functions is an example of how the code works in practive
## This would be helpful to save computation time if the matrix was very large
## or being done multiple times, here the example is just 2x2 matrix

## This is the first function, it actually makes a list of functions that allow
## for caching of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ## initially set the matrix to Null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x  ## Returns the value of the original vector
  
  setinverse <- function(solve) m <<- solve  
  ## called by cacheSolve, during the 
  ##first cacheSOlve. access and it will store the value using super assignment
 
  getinverse <- function() m
  ## This will return the value of cacheSolve
  
  ##Creat a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks to see if the inverse of the matrix has been calculated
## if it has, it recalls the value from the cache, if it hasn't it calculates it

cacheSolve <- function(x, ...) {
  m <- x$getinverse()  ## access the value of x and get its inverse
  if(!is.null(m)) {  ## check to see if the inverse has been previously calculated
    message("getting cached data")
    return(m)
  }
  
  ## we get here only if the value of the inverse was not calculated
  ## here compute the inverse
  data <- x$get()  ##  get the matrix and then store the inverse in m
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#This code below shows how these functions work in action

#step 1 - define a matrix
mat<-matrix(data=c(4,3,7,6),nrow=2,ncol=2)

#step 2 - make a special matrix with the function makecachMatrix
#this function makes the "special" matric which is a list of functions
mat2<-makeCacheMatrix(mat)

#step 3 - run cachesolve the first time and it will compute using solve
cacheSolve(mat2)
#but run it again and it wil pull the cache'd matrix
cacheSolve(mat2)