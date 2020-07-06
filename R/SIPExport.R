#'Create Metadata
#'
#'Returns data frame containing metadata about the specified SIP(s). Useful for
#'constructing metadata from a structured dataset to be included with a SIP library.
#'
#'@param df A dataframe containing data you wish to turn into metadata.
#'@param IDcol Name of column with ID's
#'@param IDvect A vector containing the ID's associated with the SIP(s).
#'@param metanamesvect A vector containing the column names of the metadata.
#'@return A dataframe with metadata to be used in association with the
#'"ExportSIPLib" SIP export function.
#'@export
#'@examples
#'data <- mtcars
#'data$ID <- rownames(mtcars)
#'meta <- SLURPMetaDF(data, "ID", rownames(mtcars)[1:4], colnames(mtcars)[1:4])
SLURPMetaDF <- function(df, IDcol, IDvect, metanamesvect) {
  xdf <- data.frame(matrix(nrow = length(metanamesvect), ncol = length(IDvect)+1))
  for(j in 1:length(IDvect)) {
    for(i in 1:length(metanamesvect)) {
      xdf[i,(j+1)] <- as.character(df[match(IDvect[j], df[[IDcol]]), match(metanamesvect[i], colnames(df))])
    }
  }
  xdf[1] <- metanamesvect
  colnames(xdf) <- c("Meta",IDvect)
  return(xdf)
}
#'Export Data as .XML SLURP
#'
#'Exports a dataframe and optional metadata as a SIP Library in .xml format to
#'the current wd, allowing a user to export SLURPs generated in R.
#'
#'@param dataframe The dataframe containing the SIP(s).
#'@param filename Name for the exported .xml.
#'@param index Logical, optionally increase index by 1
#'@param provenance Optional string to indicate origin
#'@param csvr The number digits to the right of the decimal to include
#'@param average Logical, set to TRUE to include the averages as metadata
#'@param median Logical, set to TRUE to include the medians as metadata
#'@param meta Optional dataframe created from SLURPMetaDF function. A user may also
#'specify dataframe of metadata where the column name matches the name of the SIP,
#'and the final vector contains the names of the metadata. See the example for
#'SLURPMetaDF function.
#'@export
#'@examples
#'SIPdf <- data.frame(matrix(ncol = 5, nrow = 2000))
#'colnames(SIPdf) <- c("simulate1","simulate2","simulate3","simulate4","simulate5")
#'for(i in 1:5) {
#'  SIPdf[,i] <- runif(2000)
#'  SIPdf[,i] <- ifelse(SIPdf[,i] > .5, 0, 50000)
#'}
#'
#'SLURPExportXML(SIPdf, testslurp.xml, average = TRUE, median = TRUE)
SLURPExportXML <- function(dataframe,filename,index=FALSE,provenance="",csvr=4,average=FALSE,median=FALSE,meta=NULL) {
  SIPS <- NULL
  metas <- NULL
  res <- ""
  res.meta <- ""
  if (index==TRUE) start<- 2 else start<- 1
  metadata.function <- function(i) {
    for (j in 1:nrow(meta[i])) {
      metas <- c(metas,
                 paste(" ",meta[j,1],'="',meta[j,(i+1)],'"',collapse = "",sep = ""))
    }
    for (metadata in metas) {
      res.meta <- paste(res.meta,metadata,sep = "")
    }
    return(res.meta)}
  for (i in start:ncol(dataframe)) {
    SIPS <- c(SIPS,
              paste( "<SIP name=",'"',colnames(dataframe[i]),'"',
                     " count=",'"',length(dataframe[,i]),'"',
                     " type=",'"',"CSV",'"',
                     if (provenance!="") paste(" provenance=",'"',provenance,'"',sep = "")
                     else "",
                     if (is.null(meta)) ""
                     else metadata.function(i),
                     if (average==TRUE) paste(" average=",'"',mean(dataframe[,i]),'"',sep = "")
                     else "",
                     if (median==TRUE) paste(" median=",'"',median(dataframe[,i]),'"',sep = "")
                     else "",
                     "> ",
                     paste(
                       if (is.numeric(csvr)==TRUE)
                         round(
                           dataframe[,i],
                           digits = as.numeric(csvr))
                       ,collapse = ",", sep = ", "),
                     " </SIP>",
                     "\n",
                     sep = "",
                     collapse = "") )
  }
  for (items in SIPS) {
    res <- paste(res,items, sep = "")
  }
  write(
    paste( "<SLURP name=",'"',deparse(substitute(dataframe)),'"',
           " provenance=",'"',provenance,'"',
           if (index==TRUE) paste(" count=",'"',ncol(dataframe)-1,'"')
           else paste(" count=",'"',ncol(dataframe),'"'),
           "> ",
           "\n",
           res,
           "</SLURP>",
           "\n",
           sep = "",
           collapse = ""),
    deparse(substitute(filename)),sep = "\n") }
#'Export Data as .CSV SLURP
#'
#'Exports a dataframe and optional metadata as a SIP Library in .csv format to
#'the current wd, allowing a user to export SLURPs generated in R.
#'
#'@param dataframe The dataframe containing the SIPs to export as a SLURP.
#'@param filename A string containing the desired file name. Add ".csv" to the
#'string, otherwise the file will write in a basic file format used by the write()
#'function.
#'@param slurpname An optional string containing the desired name of the SLURP
#'@param origin An optional string containing the an identifier for who made the
#'SLURP.
#'@param meta An optional dataframe containing metadata to include in export. See
#'the SLURPMetaDF function example for the correct format.
#'@return A message indicating a success.
#'@export
#'@examples
#'SIPdf <- data.frame(matrix(ncol = 5, nrow = 2000))
#'colnames(SIPdf) <- c("simulate1","simulate2","simulate3","simulate4","simulate5")
#'for(i in 1:5) {
#'  SIPdf[,i] <- runif(2000)
#'  SIPdf[,i] <- ifelse(SIPdf[,i] > .5, 0, 50000)
#'}
#'
#'SLURPExportXML(SIPdf, testslurp.xml, average = TRUE, median = TRUE)
SLURPExportCSV <- function(dataframe, filename = "", slurpname = "", origin = "", meta = NULL) {
  if (ncol(dataframe) != (ncol(meta)-1) && !is.null(meta)) {
    print("Error, meta dataframe does not match the dimensions required")
  } else {
  csvstring1 <- paste("CSV,,,,,,Always have something in A1",paste(rep(",",(max((3+ncol(dataframe)-7),0))), collapse = ""), collapse = "", sep = "")
  csvstring2 <- paste(rep(",",max(7,(2+ncol(dataframe)))), collapse = "")
  csvstring3 <- paste("Control,,SheetName,Sheet1,,,",paste(rep(",",(max((3+ncol(dataframe)-7),0))), collapse = ""), collapse = "", sep = "")
  csvstring4 <- paste(",,FilePath,",filename,paste(rep(",",max(3,(3+ncol(dataframe)-4))), collapse = ""), collapse = "", sep = "")
  csvstring5 <- paste(",,SlurpAttrs,C14:C16,,,",paste(rep(",",max(0,(3+ncol(dataframe)-7))), collapse = ""), sep = "")
  Metaloc <- ifelse((is.null(meta) || nrow(meta) == 1),
                   paste("C19:C",21, sep = ""),
                   paste("C19:C",20+nrow(meta), sep = ""))
  csvstring6 <- paste(",,SipAttrs,",Metaloc,paste(rep(",",max(3,(3+ncol(dataframe)-7))), collapse = ""), sep = "")
  SIPloc <- ifelse((is.null(meta) || nrow(meta) == 1),
                  paste("D",24, sep = ""),
                  paste("D",23+nrow(meta), sep = ""))
  csvstring7 <- paste(",,SipTlc,",SIPloc,paste(rep(",",max(3,(3+ncol(dataframe)-4))), collapse = ""), collapse = "", sep = "")
  csvstring8 <- paste(",,ClearFirst,TRUE,,,",paste(rep(",",(max((3+ncol(dataframe)-7),0))), collapse = ""), collapse = "", sep = "")
  csvstring9 <- paste(",,numSamples,",nrow(dataframe),paste(rep(",",max(3,(3+ncol(dataframe)-4))), collapse = ""), collapse = "", sep = "")
  csvstring14 <- paste("SLURP,,name,",slurpname,paste(rep(",",max(3,(3+ncol(dataframe)-4))), collapse = ""), collapse = "", sep = "")
  csvstring15 <- paste(",,count,",ncol(dataframe),paste(rep(",",max(3,(3+ncol(dataframe)-4))), collapse = ""), collapse = "", sep = "")
  csvstring16 <- paste(",,origin,",origin,paste(rep(",",max(3,(3+ncol(dataframe)-4))), collapse = ""), collapse = "", sep = "")
  csvstring19 <- paste("SIP,,name,",ifelse(ncol(dataframe) < 5,
                                          paste(paste(colnames(dataframe), collapse = ","),paste(rep(",",4-ncol(dataframe)), collapse = ""),sep = "", collapse = ""),
                                          paste(colnames(dataframe),collapse = ",")),sep = "", collapse = "")
  csvstring20 <- paste(",,count,",(ifelse(ncol(dataframe) < 4,
                                         paste(paste(rep(nrow(dataframe),ncol(dataframe)), collapse = ","),paste(rep(",",4-ncol(dataframe)), collapse = ""),sep = "", collapse = ""),
                                         paste(rep(nrow(dataframe),ncol(dataframe)), collapse = ","))),
                       sep = "", collapse = "")
  if (!is.null(meta)) {
  metavect <- rep(NA, nrow(meta))
  if (ncol(dataframe) < 5) {
                for (i in 1:(nrow(meta))) {
                  metavect[i] <- paste(",,",paste(as.character(meta[i,1:(ncol(dataframe)+1)]), collapse = ","),paste(rep(",",5-ncol(dataframe)), collapse = ""),sep = "", collapse = ",")
                }} else {
                for (i in 1:(nrow(meta))) {
                  metavect[i] <- paste(",,",paste(as.character(meta[i,1:(ncol(dataframe)+1)]), collapse = ","),sep = "", collapse = ",")
                }}
  }
  postmetapad <- rep(csvstring2,2)
  SIPsvect <- rep(NA,nrow(dataframe))
  SIPsvect[1] <- ifelse((ncol(dataframe) < 5),
                     paste("SIPs,,,",paste(as.character(dataframe[1,]),collapse = ","),paste(rep(",",4-ncol(dataframe)), collapse = ""),sep = "", collapse = ","),
                     paste("SIPs,,,",paste(as.character(dataframe[1,]),collapse = ","),sep = "", collapse = ","))
  if (ncol(dataframe) < 5) {
  for (i in 2:nrow(dataframe)) {
    SIPsvect[i] <- paste(",,,",paste(as.character(dataframe[i,]),collapse = ","),paste(rep(",",4-ncol(dataframe)), collapse = ""),sep = "", collapse = ",")
  }} else {
  for (i in 2:nrow(dataframe)) {
    SIPsvect[i] <- paste(",,,",paste(as.character(dataframe[i,]),collapse = ","),sep = "", collapse = ",")}
  }
  fullstring <- paste(csvstring1, csvstring2, csvstring3, csvstring4, csvstring5, csvstring6, csvstring7, csvstring8, csvstring9, csvstring2, csvstring2, csvstring2, csvstring2, csvstring14, csvstring15, csvstring16, csvstring2, csvstring2, csvstring19, csvstring20,
                      ifelse(is.null(meta),paste(rep(",",ncol(dataframe)), collapse = ""),paste(metavect, collapse = "\n")),
                      paste(postmetapad, collapse = "\n"),
                      paste(SIPsvect, collapse = "\n"),
                      sep = "\n")
  write(fullstring, filename)
  ret <- c("The SLURP has saved successfully to your working directory.")
  return(ret)
  }
}
