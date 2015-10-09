#' Save and Restore the Current State of a Data Structure
#'
#' This is much less grandiose than the title. It just saves whether you have
#' a data.table, data.frame or list and if it's a data.table also saves the
#' key columns.
#'
#' @param .data The data structure
#' @param .state The return value of an earlier get_dt_state call
#'
#' @import data.table
#' @examples
#' library(data.table)
#' DT <- data.table(x=1:10, key="x")
#'
#' state <- BigStuff:::get_dt_state(DT)
#' BigStuff:::set_dt_state(DT, state)
get_dt_state <- function(.data)
{
  if(!is.list(.data))
    stop("Input should be a data.table, data.frame or list")

  cl <- class(.data)

  out.key <- NULL

  if("data.table" %in% cl)
  {
    out.class <- "data.table"
    out.key <- key(.data)
  } else if("data.frame" %in% cl)
    out.class <- "data.frame"
  else
    out.class <- "list"

  list(class = out.class,
       key = out.key)
}

#' @rdname get_dt_state
set_dt_state <- function(.data, .state)
{
  if(.state$class == "list")
  {
    setDF(.data)
    setattr(.data,
            "class",
            setdiff(class(.data),
                    c("data.table", "data.frame")))
  }

  if(.state$class == "data.frame")
  {
    setDF(.data)
  }

  if(.state$class == "data.table")
  {
    setDT(.data)
    setkeyv(.data, .state$key)
  }

  invisible(.data)
}
