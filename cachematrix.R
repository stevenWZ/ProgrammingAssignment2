## 

## this function can help to creat a special "matrix" 
##cache that containing set and get matrix together with
##set and get inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
 i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)i<<-inverse
  getinverse<-function()i
  list(set=set,get=get,setinverse=setinverse,
       getinverse=getinverse)
}


## The following function calculates the inverse 
##of matrix created by above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  invmat<-x$get()
  i<-solve(invmat,...)
  x$setinverse(i)
  return(i) 
}

