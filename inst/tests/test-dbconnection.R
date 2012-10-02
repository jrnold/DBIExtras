context("dbConnection methods")

library("RSQLite")
tmpdb <- tempfile()
drv <- dbDriver("SQLite")
con <- dbConnect(drv, tmpdb)

## These tests make persistent changes
## and thus are dependent on the state based on the previous test
test_that("[[ create table read works", {
    ## Create table
    con[["iris"]] <- iris
    expect_true("iris" %in% dbListTables(con))
})
test_that("[[ read table read works", {
    ## Create table
    expect_equivalent(con[["iris"]],
                      transform(iris, Species=as.character(Species)))
})
test_that("[[ delete table read works", {
    ## Create table
    con[["iris"]] <- NULL
    expect_true(! "iris" %in% dbListTables(con))
})

## These tests make persistent changes
## and thus are dependent on the state based on the previous test
test_that("$ create table read works", {
    ## Create table
    con$iris <- iris
    expect_true("iris" %in% dbListTables(con))
})
test_that("$ read table read works", {
    ## Create table
    expect_equivalent(con[["iris"]],
                      transform(iris, Species=as.character(Species)))
})
test_that("$ delete table read works", {
    ## Create table
    con$iris <- NULL
    expect_true(! "iris" %in% dbListTables(con))
})

## Length of Tables
test_that("length,DBIConncetion works", {
    expect_equal(length(con), 0)
    con$iris <- iris
    expect_equal(length(con), 1)
    con$iris <- NULL
    expect_equal(length(con), 0)
})

## Names of Tables
test_that("names,DBIConncetion works", {
    expect_equal(names(con), character(0))
    con$iris <- iris
    expect_equal(names(con), "iris")
    con$iris <- NULL
    expect_equal(names(con), character(0))
})

