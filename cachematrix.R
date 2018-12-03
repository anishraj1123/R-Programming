## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## it supports setting matrix, getting matrix, setting inverse and getting
## inverse

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
    inv <- NULL                             ##initialize inv as NULL; will hold value of matrix inverse
    set <- function(y) {                    ## set function to assign new value of matrix 
                                            ##in parent environment
        x <<- y
        inv <<- NULL                        ## if we initialize a new matrix, reset inv to NULL to 
                                            ##  store the new inverse
        }
        get <- function() x                 ##get fucntion - returns value of the matrix argument
        
        setinverse <- function(invers) inv <<- invers  ##assigns value of inv in parent environment
        
        getinverse <- function() inv   ##gets the value of inv where called. First time when function runs it will return NULL
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## creating lise in order to refer
                                                                                      ##to the functions with the $ operator 
    }
    


## Write a short comment describing this function

## The function cacheSolve takes the output of the fucntion makeCacheMatrix(matrix) as an input 
#  and checks if the inverse of input matrix has any value in it or not using getinverse() function

# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
# and set the invertible  matrix by using the solve function.

# In case inverse matrix from makeCacheMatrix((matrix) has some value in it, 
#it returns a message  "Getting Cached Invertible Matrix" and the cached object

# please note than first time when cacheSolve() is run, the inverse will be NULL
# but after the first iteration the inverse will have value and we display the same 
# along with the message "getting cached data" as an indication


cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()                      #returns the value of inv 
    if(!is.null(inv)) {                        #if inverse matrix is not NULL- this hppens we have already called cacheSolve function
        message("getting cached data")         #Type message: Getting Cached Invertible Matrix 
        return(inv)                            #return the invertible matrix
    }
    else {                                     # the value of inv is not NULL-This happens when 
                                               # we run the cacheSolve() function for the first time
      
      message("The function is running for first time")
      data <- x$get()                         #get the original Matrix Data 
      inv <- solve(data)                      #use solve function to inverse the matrix
      x$setinverse(inv)                       #set the invertible matrix  
      inv                                     #return the invertible matrix
    }
}


## TEST CASE 1
#m3 <- matrix(3:6,2,2)
#c3 <- makeCacheMatrix(m3)
#cacheSolve(c3)