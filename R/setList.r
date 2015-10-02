#' Turn a data.table or data.frame into a list by Reference
#'
#' This is analogous to setDT and setDF from data.table, although far less
#' elaborate.
#'
#' @param .data A data.table or data.frame
#'
#' @return This will change the type by reference. Nevertheless it is also
#' returned invisibly for ease of use with magrittr and to be consistent with
#' setDT and setDF.
#'
#' @export
#' @examples
#' DF <- data.frame(x=1:10)
#' setList(DF)
#' stopifnot(is.data.frame(DF) == FALSE)
setList <- function(.data)
{
  setattr(
    .data,
    "class",
    setdiff(class(.data),
            c("data.table", "data.frame"))
  )

  invisible(.data)
}
