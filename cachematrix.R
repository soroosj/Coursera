## Creates a cached version of matrix inversion and returns a matrix with inverse of x if not previously created

## Creates a cached version of a matrix inversion.

makeCacheMatrix <- function(x = matrix()) {
   
   inv<- NULL
   set <- function (y) {
      x<<- y
      inv<<-NULL
   }
   get<-function()x
   setinv<-function(inverse) inv<<-inverse
   getinv<-function()inv
   list(set=set,get=get, setinv=setinv,getinv=getinv)
}


## Returns a matrix that is the inverse of "".
## If inverse has been previously calculated, retrieves cached version
## If inverese has not been previously calculated, calculates the inverse.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   
   inv<-x$getinv()
   if(!is.null(inv)){
      message("getting cached data")
      return(inv)
   }
   data<-x$get()
   inv<-solve(data,...)
   x$setinv(inv)
   inv
}