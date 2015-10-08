## The purpose of the below routine is to create a special matrix which is a list so that the inverse calculated is saved in cache and if same 
## matrix is used again to get inverse we get the value from the cache rather than computing again.

## Below function takes a matrix as an input and returns a list of four basic function to implement the cache functionality 

makeCacheMatrix <- function(x = matrix()) {

  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){
    x
    } 
  setInverse<-function(inverse) inv<<-inverse
  getInverse<-function(){
  inv
  }
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  
}


## Below function takes the special matrix (list) and return the inverse of a invertible matrix,if inverse is already calculated then
## then it will not compute again rather extract the result from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setInverse(inv)
  inv
}
