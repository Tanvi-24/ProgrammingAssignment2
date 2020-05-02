

##makecacheMatrix function creates function to cache  the inverse of the given matrix
#cacheSolve function returns the inverse matrix which is cached


makeCacheMatrix <- function(x = matrix()) {
  
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(x) inv<<-x
  getinverse<-function()  inv
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  makeCacheMatrix(x)
  inv<-x$getinverse()
  if (!is.null(inv))
  {
    inv
  }
  else
  {
    data<-x$get()
    inv<-solve(x)
    x$setinverse(inv)
    inv
  }
  
}


