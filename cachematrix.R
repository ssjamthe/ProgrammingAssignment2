## This function provides the functionalty to cache the inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }

  get<-function() x
  
  setInverse<-function(inverse){
    inv<<-inverse
  }
  
  getInverse<-function() inv
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  
}

 
## This function checks if the inverse is cached. 
## If it is cached it returns the cached value. Other wise calculates it and caches it.
cacheSolve <- function(x, ...) {
    inv<-x$getInverse()
    if(!is.null(inv))
    {
      message("getting cached data")
      return(inv)
    }
    
    mat<-x$get()
    inv<-solve(mat)
    x$setInverse(inv)
    inv
    
}
