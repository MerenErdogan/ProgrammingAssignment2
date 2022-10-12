
makeCacheMatrix <- function(x = matrix()) { ##This function will allow the code to retrieve matrixes.
  invMatrix <- NULL
                                         ##Setting invMatrix to NULL will lead the code to get data from getmatrix below. 
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  getMatrix <- function() x                             
  setInverse <- function(inverse) invMatrix <<- inverse  ##This part will retrieve the "x" matrix and take inverse of it.
  getInverse <- function() invMatrix                     
  list(setMatrix = setMatrix, getMatrix = getMatrix,      ##This part is necessary to use $sign and link both steps of the process.
       setInverse = setInverse, getInverse = getInverse)
  
}

cacheSolve <- function(x, ...) {             ##This part will paste the same inversed matrix if x is NULL.
  
  invMatrix <- x$getInverse()                ##If not, it will take inverse of the given matrix.
  if(!is.null(invMatrix)) {                       
    message("Getting Cached Invertible Matrix")    
    return(invMatrix)                             
  }
  
  MatrixData <- x$getMatrix()                     
  invMatrix <- solve(MatrixData, ...)            
  x$setInverse(invMatrix)                        
  return(invMatrix)                         
}

TestMatrix <- matrix(1:4,2,2)

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)
