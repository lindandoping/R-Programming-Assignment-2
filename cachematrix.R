## Put comments here that give an overall description of what your
## functions do

## Set-Assigns input argument to x in the parent environment. It also clear
##     resets the value of the inverse to NULL
## Get-Retrieves the value of the matrix
## SetSolve-Sets the value of the inverse and assigns it parent environment 
## GetSolve-Retrieves the value of the matrix inverse from the makeCacheMatrix 
##          parent environment


## Write a short comment describing this function
##Function below creates a special matrix that sets and caches the 
##inverse of a matrix 
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL #initializes the value of s to null
    set <- function(y){  #Initializes the sets matrix value to null
      x <<- y
      s <<- NULL
    }
    get <- function() x #
    setsolve <- function(solve) s<<- solve # 
    getsolve <- function() s #
    list(set = set, get = get,
     setsolve = setsolve,
     getsolve = getsolve)

}
  
## Write a short comment describing this function
## Obtains cache solve 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s<- solve(data, ...)
  x$setsolve(s)  #Return the inverse matrix 
  s 
  
}

RNGversion("3.5.1")  
set.seed(100)
r<-10
c<-10
a<-round(matrix(runif((r*c),min=0, max=100),r,c))
b<-round(matrix(runif((r*c),min=0, max=100),r,c))
mymatrix<-makeCacheMatrix(a)  ## Initializes function with assigned matrix input
mymatrix$getsolve() ##Retrieves inverse value of matrix which should be NULL in this case
u<-cacheSolve(mymatrix)
mymatrix$getsolve() ##Retrieves inverse value of matrix 


mymatrix$set(b)   #Replaces mymatrix with new matrix values
v<-cacheSolve(mymatrix) #Retrieves mean for new matrix data

w<-cacheSolve(mymatrix) #Retrieves most recent stored inverse data for my matrix


