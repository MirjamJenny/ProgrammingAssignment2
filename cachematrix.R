## Put comments here that give an overall description of what your
## functions do

## This function makeCacheMatrix creates a special "matrix" object. It also creates functions to set the inverse and to get the inverse.
makeCacheMatrix <- function(x = matrix()) {     #set the value of the matrix
        inv <- NULL
        set <- function(y) {                     
                x <<- y
                inv <<- NULL
        }
        get <- function() x                      #get the value of the matrix
        setinv <- function(solve) inv <<- solve  #set the value of the inverse
        getinv <- function() inv                 #get the value of the mean
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function computes the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it computes the inverse of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        inv <- x$getinv() #this is where it checks whether the inverse already exists
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

####

#here comes an example of how to use this function
#set a seed before you begin to make the computations reproducible
#set.seed(101)

#this first step creates the special matrix. the resulting matrix is 10x10 and has random numbers from a standard normal distribution
# mymatrix <- makeCacheMatrix(x=matrix(rnorm(100),nrow=10,ncol=10))

#this scond step computes the inverse of the special matrix created above and saves it
# myInvmatrix <- cacheSolve(mat)

