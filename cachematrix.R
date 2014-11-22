## Jian Liang completed on 22nd Nov 2014 for R Programming Assignment 2

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialized inverse matrix
  Inverse <- NULL
  
  ## This function is to initialized a matrix, if passed
  ## in matrix is NULL or not Matrix class object, a 1 value
  ## matrix will be store , and a warning message will
  ## be given. other
  setm <- function(mat){
    if(class(mat)=="matrix" & mat!=NULL){
      x<<-mat
      Inverse <<-NULL
    }
    else{
      warning("Please ensure you pass a matrix class valuable as parameter.")
    }
  }
  
  getm <- function(){
    x
  }
  
  ## This function is to store inverse matrix when origional
  ## matrix and passed in matrix are both not empty. It doesn't check
  ## if a matrix is inversible. When assign successfully, it
  ## will return TRUE, otherwise FALSE
  setinvm <- function(inv){
    if(is.na(x)){
      warning("There is no matrix initialized yet, please provid a matrix
              before given a inverse matrix")
      return FALSE
    }
    else if (is.na(inv)){
      warning("Empty matrix is not acceptable as inverse matrix")
      return FALSE
    }
    Inverse<<-inv
    return TRUE
  } 
  
  ## If there is no initialized matrix, a NULL value will
  ## be returned.
  getinvm <- Function(){
    if(!is.na(x)){
      Inverse <<- solve(x)
      return Inverse
    }
    else{
      warning("Please use setm() to assign a non-NA matrix")
      return NULL
    }
  }
  
  list(setm = setm, getm = getm,
        setinvm = setinvm,
        getinvm = getinvm)
    
    
  }
  
  
  

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
}
