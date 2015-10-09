#' @import magrittr
exp_c <- function(...)
{
  exps <- eval(substitute(alist(...)))

  # Downside: Needs to do string manipulation
  # Upside: This is done on strictly controlled names which should eliminate
  #           the risk
  names(exps) <- paste0("exp",
                        seq_along(exps))

  paste0(".(",names(exps),")", collapse=";") %>%
    paste0("{",.,"}") %>%
    c(~exps) %>%
    pryr::make_call(quote(bquote), .) %>%
    .$expr %>%
    eval
}
