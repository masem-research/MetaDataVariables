#' PrintDisplay numbered list of variables in a dataset
#'
#' @description Often it is really useful to have a numbered list of variables
#' in a dataset, especially if the dataset is new. The function will provide such
#' a list
#'
#' @param df data.frame. Input object: data.frame.
#'
#' @return data.frame. data.frame with row numbers corresponding to the position of the
#' variables in the dataset and variable names.
#' @export
#'
#' @examples
#' # iris
#' numberedVariablesList(df = iris)
#' # mtcars
#' numberedVariablesList(df = mtcars)
numberedVariablesList <- function(df) {
  if (!is.data.frame(df)) {
    #warning("input object is not a data.frame!")
    #return(NA)
    stop("input object is not a data.frame!")
  }
  return(data.frame(variableNames = names(df)))
}





#' Displays Metadata of variables in a data.frame
#'
#' @param DataFrame data.frame. data to be analyzed. data.frame required.
#'
#' @details
#' Returns four columns in a data.frame:
#' 1: VarNo:         Running Number
#' 2: VarNames:      Variable Name
#' 3: StorageType:   Atomic class
#' 4: UniqueValues:  Number of unique categories
#'
#' @return data.frame. data.frame with metadata.
#' @export
#'
#' @examples
#' ## famous iris data set
#' VariableMetaData(DataFrame = iris)
#' ## mtcars data set
#' VariableMetaData(DataFrame = mtcars)
VariableMetaData <- function(DataFrame) {
  ## Returns four columns in a data.frame:
  #   1: VarNo:         Running Number
  #   2: VarNames:      Variable Name
  #   3: StorageType:   Atomic class
  #   4: UniqueValues:  Number of unique categories
  ## Generate data.frame
  VariableMetaData <- data.frame(VarNo = 1: length(names(DataFrame)),
                                 VarNames = names(DataFrame),
                                 StorageType = sapply(DataFrame, class),
                                 UniqueValues = sapply(DataFrame, function(x) length(unique(x))))
  ## Delete row,names
  rownames(VariableMetaData) <- NULL
  ## Return
  return(VariableMetaData)
}
