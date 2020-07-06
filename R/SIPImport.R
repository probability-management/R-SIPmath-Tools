#'Import SLURP
#'
#'Imports a SIP library that is in a CSV format into a dataframe or list of
#'dataframes if metadata is included.
#'
#'@param filename The name of the file which the SIP library is to be read from. Uses the same format as read.csv to allow reading from the current working directory or a specified one.
#'@param meta Logical, use TRUE if you want to include the metadata of the library.
#'@return If meta = TRUE, returns a list of two dataframes, one with the metadata and the other with the SIPs. If FALSE(default), returns a dataframe of the imported SIPs.
#'@export
#'@examples
#'SLURPImportCSV(system.file("extdata", "TestLibrary.csv", package = "SIPmathTools"), TRUE)
SLURPImportCSV <- function(filename, meta = FALSE) {
  library <- utils::read.csv(filename, header = FALSE)
  var <- as.numeric(library[15,4])
  trial <- as.numeric(library[9,4])
  lengthdf <- length(library[[4]])
  df <- data.frame(matrix(NA, nrow=trial, ncol=var))
  names <- rep(0, var)
  for (i in 1:var){
    names[i] <- library[19, i+3]
  }
  trunclib <- as.data.frame(library[(lengthdf-trial+1):lengthdf,4:(var+3)])
  colnames(trunclib) <- names
  rownames(trunclib) <- 1:trial
  if (meta == TRUE) {
    truncmeta <- as.data.frame(library[21:(lengthdf-trial-2),4:(var+3)])
    colnames(truncmeta) <- names
    rownames(truncmeta) <- library[21:(lengthdf-trial-2),3]
    liblist <- list(truncmeta, trunclib)
  } else {
    liblist <- trunclib
  }
  return(liblist)
}
