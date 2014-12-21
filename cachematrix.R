
## Define pair of functions that compute the inverse of the matrix and returns the results from cache if already cached.
## create a special"matrix" object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) { 

         ## set inverse as null 
         inv <- NULL 
         ## Define a function that assigns matrix x and resets the inverse as NULL 
        set <- function(y) { 
               x <<- y 
                inv <<- NULL 
                        } 
# define a funtion to return x 
        get <- function() x 
                ## define a function using function solve to get the inverse of matrix 'x' and store into 'inv' 
       setinverse <- function(solve) inv <<- solve 
         ## define a funtion to return the inverse of matrix 'x' 
        getinverse <- function() inv 
          ## return a list of functions defined 
       list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse) 
} 

##eturn inverse of the special "matrix" if its inverse is found from the above result. 
## Otherwise, computes the inverse.   
cacheSolve <- function(x, ...) { 
         ## Return a matrix that is the inverse of 'x' 
         ## call the function to get the inverse of matrix 'x'
         ## test whether the result exits 
         inv <- x$getinverse()
         if(!is.null(inv)) { 
                 ## 
                 message("getting cached data") 
                 return(inv) 
         } 
         ## compute the inverse of the original matrix  
         data <- x$get() 
         inv <- solve(data, ...) 
       	x$setinverse(inv) 
         inv 
} 



