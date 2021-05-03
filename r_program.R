makeCacheMatrix <- function(a = matrix())
{
  inverse <- NULL
  setting <- function(b)
  {
    a <<- b
    inverse << - NULL
  }
  Calling <- function() a
  SettingInverse <- function(processmatrix)
    inverse <<- processmatrix
  CallingInverse <- function() inverse
  list(setting = setting, Calling = Calling, SettingInverse = SettingInverse, Calling = CallingInverse)
}
cacheSolve <- function(a, ...)
{
 inverse <- a$getInverse()
 if(!is.null(inverse))
 {
   print("there is a cached data")
   return inverse
 }
 value <- a$get()
 inverse <- solve(value)
 a$setInverse(inverse)
 inverse
}