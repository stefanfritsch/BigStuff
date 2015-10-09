library(testthat)
library(data.table)
library(magrittr)

context("rbind_in_place")

#' Test .use.names = FALSE
#' =======================
DT <- as.data.table(iris)
DT[,ID:=1:.N]
setkey(DT, ID)

A <- DT[Species == "virginica"]
B <- DT[Species != "virginica"]

A.old <- copy(A)
setcolorder(B, rev(colnames(B)))

# debugonce(rbind_in_place)
rbind_in_place(A, B, .use.names = FALSE)

expect_equal(dim(A), c(150,6))
expect_equal(exists("B"), FALSE)

expect_equal(lapply(A[101:150], as.character),
             lapply(A.old, as.character))



#' Test .use.names = TRUE
#' ======================
DT <- as.data.table(iris)
DT[,ID:=1:.N]
setkey(DT, ID)

A <- DT[Species == "virginica"]
B <- DT[Species != "virginica"]

setcolorder(B, rev(colnames(B)))

# debugonce(rbind_in_place)
rbind_in_place(A, B, .use.names = TRUE)

setkey(A, ID)
setattr(DT, "index", NULL)

expect_equal(A, DT)
expect_equal(exists("B"), FALSE)
expect_equal(colnames(A),
             c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species", "ID"))
expect_equal(
  sapply(A, . %>% is.na(.) %>% sum),
  structure(c(0L, 0L, 0L, 0L, 0L, 0L),
            .Names = c("Sepal.Length","Sepal.Width", "Petal.Length", "Petal.Width", "Species", "ID"))
)



#' Test .fill = TRUE
#' =================
DT <- as.data.table(iris)
A <- DT[Species == "virginica"]
B <- DT[Species != "virginica"]
B[,Sepal.Length := NULL]

rbind_in_place(A, B, .use.names = TRUE, .fill = TRUE)

expect_equal(is.data.table(A), TRUE)
expect_equal(dim(A), c(150, 5))
expect_equal(exists("B"), FALSE)

expect_equal(colnames(A),
             c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species"))
expect_equal(
  sapply(A, . %>% is.na(.) %>% sum),
  structure(c(100L, 0L, 0L, 0L, 0L),
            .Names = c("Sepal.Length","Sepal.Width", "Petal.Length", "Petal.Width", "Species"))
)


