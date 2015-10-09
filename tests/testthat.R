library(testthat)
#
# if(!require(BigStuff, lib.loc = "C:/Users/fritsch.STATUP/Documents/R/win-library/3.2"))
#   install.packages("BigStuff",
#                    repos="file:Z:/Rpackages",
#                    lib = "C:/Users/fritsch.STATUP/Documents/R/win-library/3.2")

library(BigStuff)

test_check("BigStuff")
