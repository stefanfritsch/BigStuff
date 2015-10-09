#' Efficiently Combine Two data.tables, data.frames and lists
#'
#' This function appends one data.table to another by "semi-reference." This
#' means that you need enough memory for both inputs + 1 column instead of twice
#' the memory of both inputs combine like you do for regular rbind (data.table or
#' not).
#'
#' @param .X      A data.table, data.frame or list. .Y will be appended to this
#' @param .Y      A data.table, data.frame or list. This will be appended to .X.
#'                Unless you set .keep.Y to TRUE .Y will be deleted in the process.
#' @param .keep.Y Logical. Should .Y be deleted while combining the tables?
#' @param .use.names Logical. Like with rbind if you choose FALSE (default)
#'                columns will be merged by position. Otherwise by column names.
#' @param .fill   If set to TRUE columns of .X not present in .Y will be filled
#'                with NA. Otherwise you will get an error.
#'                Note that this function does not add additional columns to .X.
#'
#' @return The operation is done by reference so you do not need the return value.
#' Nevertheless .X (with the appended .Y) is returned for ease of use with
#' magrittr.
#'
#' @export
#' @examples
#' # Bind by position
#' DF.1 <- data.frame(x=1:5, y=10:6)
#' DF.2 <- data.frame(y=5:1, x=6:10)
#'
#' rbind_in_place(DF.1, DF.2)
#'
#' # Bind by name
#' DF.1 <- data.frame(x=1:5, y=10:6)
#' DF.2 <- data.frame(y=5:1, x=6:10)
#'
#' rbind_in_place(DF.1, DF.2, .use.names = TRUE)
#'
#' # Fill missing columns
#' DF.1 <- data.frame(x=1:5, y=10:6, z=1:5)
#' DF.2 <- data.frame(y=5:1, x=6:10)
#'
#' rbind_in_place(DF.1, DF.2, .use.names = TRUE, .fill = TRUE)
rbind_in_place <- function(.X,
                           .Y,
                           .keep.Y = FALSE,
                           .use.names = FALSE,
                           .fill = FALSE)
{
  #*****************************************************************************
  # Preliminaries ##############################################################
  #*****************************************************************************

  if(isTRUE(.keep.Y))
    stop("Currently defunct. Sorry.")

  # Where should we join .X and .Y
  env <- parent.frame()


  # Save current state
  state.X <- get_dt_state(.X)
  state.Y <- get_dt_state(.Y)

  # Compare columns present in .X and .Y
  if(isTRUE(.use.names))
  {
    cols.X <- names(.X)
    missing.cols <- setdiff(cols.X, names(.Y))
    cols.X <- setdiff(cols.X, missing.cols)
    cols.Y <- cols.X
  } else {
    cols.X <- seq_along(.X)
    old.names.Y <- names(.Y)
    setnames(.Y, paste0("V", seq_along(.Y)))
    cols.Y <- names(.Y)

    missing.cols <- setdiff(cols.X, seq_along(.Y))
    cols.X <- setdiff(cols.X, missing.cols)
  }




  # Throw error if .Y is missing columns and fill = FALSE
  if(!isTRUE(.fill) && length(missing.cols) > 0 )
    stop("Not all columns of .X are present in .Y and fill = FALSE.")


  #*****************************************************************************
  # Combine .X and .Y ##########################################################
  #*****************************************************************************

  setList(.X)
  setList(.Y)

  #----------------------------------------------------------------------------#
  # Simply combine columns both in .X and .Y
  #----------------------------------------------------------------------------#
  for(col in seq_along(cols.X))
  {
    eval(
      envir = env,
      substitute({
        .X[[cols.X[col]]] <- BigStuff:::combine_cols(.X[[cols.X[col]]],
                                                     .Y[[cols.Y[col]]])
      })
    )

    if(!isTRUE(.keep.Y))
    {
      eval(
        envir = env,
        substitute({
          .Y[[cols.Y[col]]] <- NULL
        })
      )
    }
  }

  #----------------------------------------------------------------------------#
  # Handle missing columns
  #----------------------------------------------------------------------------#
  if(length(.Y) > 0)
    NROW.Y <- length(.Y[[1]])
  else
    NROW.Y <- 0

  for(col in missing.cols)
  {
    eval(
      envir = env,
      substitute({
        if(!is.factor(.X[[col]])) {
        .X[[col]] <- c(.X[[col]],
                       rep(NA_integer_, NROW.Y))
        } else {
          .X[[col]] <- factor(c(as.character(.X[[col]]),
                         rep(NA_character_, NROW.Y)))
        }
      })
    )
  }


  #*****************************************************************************
  # Cleanup ####################################################################
  #*****************************************************************************

  # restore state of .X
  eval(
    envir = env,
    substitute({
      BigStuff:::set_dt_state(.X, state.X)
    })
  )

  # If .Y should be deleted do so, otherwise restore its state
  if(!isTRUE(.keep.Y))
  {
    eval(
      envir = env,
      substitute({
        rm(.Y)
      })
    )
  } else {
    eval(
      envir = env,
      substitute({
        setnames(.Y, old.names.Y)
        BigStuff:::set_dt_state(.Y, state.Y)
      })
    )
  }

  invisible(.X)
}


# rbind_in_place <- function(..., .use.names = FALSE)
# {
#   name.list <- eval(substitute(alist(...)))
#   data.list <- eval(substitute(list(...)))
#
#   state <- lapply(data.list, get_dt_state)
#
#   lapply(data.list, setList)
#
#   if(isTRUE(.use.names))
#   {
#     cols <- colnames(data.list[[1]])
#
#     lapply(data.list, setcolorder)
#   }
#
#   eval(
#     envir = parent.frame(),
#     substitute({
#       for(col in seq_along(.data))
#       {
#         .data[[col]] <- c(.data[[col]][-seq_len(.n)]
#       }
#     }))
#
#   set_dt_state(.data, state)
#
#   invisible(.data)
# }
