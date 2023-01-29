makeCacheMatrix <- function(){
  cached_inverted_matrix <- NULL 
  cached_source_matrix <- NULL
  cached_orig_matrix <- NULL
  setmat <- function(select_matrix){
    cached_orig_matrix <<- select_matrix
  }
  invertmat <- function(select_matrix){
    cached_source_matrix <<- select_matrix
    cached_inverted_matrix <<- solve(cached_source_matrix)
  }
  getmat <- function(){
    cached_orig_matrix
  }
  getsmat <- function(){
    cached_source_matrix
  }
  getimat <- function(){
    cached_inverted_matrix
  }
  list(setmat = setmat, invertmat = invertmat,
       getmat = getmat, getimat = getimat, getsmat = getsmat)
}


# How do I check the identity of the new matrix object against the old one that I have cached? 
# I might yet need to nest cacheSolve within makeCacheMatrix. How else would it check some new matrix
# against what's in the cache? Peng's example just checks whether the new object's mean is the default of NULL.
# I have to check if the new object is the same as some arbitrary old one, which could be anything?

cacheSolve <- function(obj_matrix){
  if (!is.null(obj_matrix$getimat())){ #NULL is the default value for the inverse matrix. If not NULL, go on.
    if (mean(obj_matrix$getmat() == obj_matrix$getsmat()) == 1){
      message("Matrix identical to previous input; result already cached:")
      return(obj_matrix$getimat())
    }
    else{
      obj_matrix$invertmat(obj_matrix$getmat())
      message("New matrix. Result has been stored in cache and will print below:")
      return(obj_matrix$getimat())
    }
  }
  obj_matrix$invertmat(obj_matrix$getmat()) #If NULL, calculate the inverse and store it.
  message("No inverse yet calculated for any matrix. Result cached and will print below:")
  return(obj_matrix$getimat()) #Return the inverse matrix.
}