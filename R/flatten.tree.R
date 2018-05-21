#' @author Gregoire Versmee, Mikael Dusenne, Laura Versmee

flatten.tree <- function(env, path, token, verbose = FALSE)  {

  f <- function(l)  {
    unlist(sapply(l, function(e)  {
      if (substr(basename(e), 1, 1) == "?") {
        message(paste0('\n   !!!There is an issue in the database with the path: "', e,
                       "\n    -> pathways can't start with a question mark. Discarding path, Please contact the developpers regarding this issue.\n"))
        return(NULL)
      }
      node <- content.get(paste0(env, "/rest/v1/resourceService/path", gsub("\\?", "%3F", e)), token)
      if(length(node) == 0)  {
        if (verbose)  message(e)
        return(e)
      } else {
        if (!is.null(node$errorType) | !is.null(node$status)) {
          message(paste0('\n   !!!There is an issue in the database with the path: "', e,
                         '"\n   -> discarding path. Please contact the developpers regarding this issue!!!\n'))
        } else  return(f(sapply(node, function(n) n$pui)))
      }
    }, USE.NAMES = FALSE))
  }
  return(as.vector(f(path)))
}
