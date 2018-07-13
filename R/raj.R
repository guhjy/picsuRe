#' @author Gregoire Versmee, Mikael Dusenne, Laura Versmee
#' @export raj


raj <- function(env, token, var = "ALL", verbose = FALSE)  {
  
  if (var == "ALL") {
    
    # Get the 1rst node of the environment, until reaching the 1st arg of the variable path
    path1 <- paste0(env, "/rest/v1/resourceService/path")
    
    # Look for the i2b2 node
    path <- content.get(path1, token)[[1]][[1]]
    
    return(as.vector(flatten.tree(env, path, token, verbose)))
    
  } else {
    var <- gsub("//", "/", paste0("/", trimws(var)))
    
    path <- content.get(paste0(env, "/rest/v1/resourceService/find?term=", gsub("\\*", "%", basename(var))), token)
    path <- as.character(sapply(path, "[", "pui"))
    if (dirname(var) != ".")  path <- path[grepl(URLencode(dirname(var), reserved = TRUE), sapply(path, URLencode, reserved = TRUE))]
    if (all(is.na(path))) {
      message(paste0("\nNo variables associated with: ", var,
                     "\nPlease check the spelling, or see if there is any forbidden character such as forward slashes, trailing spaces. If so, ask the developpers to remove them."))
      } else {
      if (verbose)  message(paste0("\nRetrieving all variables associated with: ", var))
      return(as.vector(flatten.tree(env, path, token, verbose)))
    }
  }
}
