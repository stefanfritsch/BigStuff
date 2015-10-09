#' Efficiently Remove Rows from a data.table, data.frame or list
#'
#' This function allows you to delete rows from a data.table, data.frame or list
#' without copying the whole table. Instead you need space in RAM for the
#' table + 1 column. This function might change the row order of the table if it
#' is not keyed.
#'
#' @param .data   A data.table, data.frame or list
#' @param .n      The first .n rows of the table will be deleted.
#' @param .keep   The same as i in a data.table, .data[i,j]. I.e. a selector
#'                that specifies which rows should be kept. This must not be a join.
#'
#' @export
drop_in_place <- function(.data, .n = NULL, .keep = NULL)
{
  env <- parent.frame()
  state <- get_dt_state(.data)

  if(is.null(.n))
  {
    .n <- eval(
      envir = env,
      substitute({
        if(exists(".drop_in_place"))
          stop("The variable '.drop_in_place' must not exist in the environment.")
        .data[,.drop_in_place := .keep]
        setkey(.data, .drop_in_place)

        .data[,min(which(.drop_in_place == TRUE)) - 1]
      })
    )

    eval(
      envir = env,
      substitute({
        .data[,.drop_in_place := NULL]
      })
    )

    if(.n < 1)
      stop("Selection with .keep failed. Please specify .n instead. This error
           message is not really helpful, I know, I'm sorry.")
  }


  setList(.data)

  eval(
    envir = parent.frame(),
    substitute({
      for(col in seq_along(.data))
      {
        .data[[col]] <- .data[[col]][-seq_len(.n)]
      }

      BigStuff:::set_dt_state(.data, state)
    }))

  invisible(.data)
}
