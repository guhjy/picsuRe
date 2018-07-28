#' @author Gregoire Versmee, Laura Versmee, Mikael Dusenne
#' @export nicer.result


nicer.result <- function(result, allpaths, verbose = FALSE)  {

  if (verbose)  message("  combining the categorical variables")

  groups <- dirname(colnames(result)[-1])
  names(groups) <- names(allpaths)
  groups2 <- unique(groups)
  names(groups2) <- names(allpaths)[!duplicated(groups)]

  final <- result[1]
  cnames <- c("Patient_id")

  for (e in groups2)  {
    subdf <- result[-1][groups == e]
    if (length(unique(subdf[,1])) <= 2  & any(is.na(unique(subdf[,1]))))  {
      subdf[is.na(subdf)] <- ""
      final <- cbind(final, as.factor(apply(subdf, 1, paste0, collapse = "")))
      if (!all(is.null(names(groups2)))) {
          if (names(e) != "") cnames <- c(cnames, names(e)) else  cnames <- c(cnames, basename(dirname(colnames(subdf)[1])))
      } else  cnames <- c(cnames, basename(dirname(colnames(subdf)[1])))

    } else {
      final <- cbind(final, subdf)
      if (!is.null(names(groups[groups == e])))  {
        cnames <- c(cnames, names(groups[groups == e]))
      } else  cnames <- c(cnames, basename(colnames(subdf)))
    }
  }

  colnames(final) <- cnames

  return(final)
}
