## In my R-script I wrote two functions in order to set and use cache for the calculation
## of the inverse of a Matrix, because it's a very costly computation. 
## 


## This first function generates a vector of four elements in order to create a cache for the matrix
## and for the matrix inverse. In fact we have 4 elements made by functions: "set","get" refer to the matrix and
## "setinverse", "getinverse" refer to the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<- function() inv
  list(set=set ,
       get=get ,
       setinverse=setinverse ,
       getinverse=getinverse)
}


## The main purpose of this function is to calculate the inverse of a matrix without doing the calculation
## if not necessary. The variable passed is not a matrix but the list created with the makeCacheMatrix function
## This function calculates the inverse only if it doesn't exist already, in fact if it is exists it takes the value
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
