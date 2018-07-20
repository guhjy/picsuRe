#' @author Gregoire Versmee, Laura Versmee
#' @import httr
#' @export agr

agr <- function(doid, output = "table", includechildren = FALSE) {
  
  agr <- content(GET(paste0("https://alliancegenome.org/api/disease/DOID:", doid)))

  if (output == "table") {
    disease <- agr[["name"]]
    symbol <- as.character(sapply(sapply(agr$annotations, `[`, "geneDocument"), `[`, "symbol"))
    species <- as.character(sapply(sapply(agr$annotations, `[`, "geneDocument"), `[`, "species"))
    geneticEntity <- as.character(sapply(sapply(agr$annotations, `[`, "geneDocument"), `[`, "geneticEntityExternalUrl"))
    associationtype <- as.character(sapply(agr$annotations, `[`, "associationType"))
    evidencecode <- as.character(sapply(sapply(sapply(sapply(agr$annotations, `[`, "publications"), `[`, 1), `[`, "evidenceCodes"), `[`, 1))
    references <- as.character(sapply(sapply(sapply(agr$annotations, `[`, "publications"), `[`, 1), `[`, "pubMedId"))
    refurl <- as.character(sapply(sapply(sapply(agr$annotations, `[`, "publications"), `[`, 1), `[`, "pubMedUrl"))
   
    return(data.frame(cbind(symbol, species, associationtype, disease, evidencecode, references, refurl)))
  }

  if (output == "disease") return(agr$name)
  if (output == "definition") return(agr$definition)
  if (output == "synonyms") return(as.character(agr$synonyms))
  if (output == "crossreferences") return(agr$crossReferences[[1]])

}
