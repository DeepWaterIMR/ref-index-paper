## ---------------------------
##
## Script name: Run first
##
## Purpose of script: To contain all definitions, packages and functions
## required by a project. Only place definitions that you want to be
## executed in every script throughout the project here. Save the script
## as "0 run first.R" in your project root folder. It will be sourced in
## all scripts that follow.
##
## Author: Mikko Vihtakari // Institute of Marine Research, Norway
## Email: mikko.vihtakari@hi.no
##
## Date Created: 2021-04-18
##
## ---------------------------

### Clear workspace (comment to )
if (exists("con_db")) {
  DBI::dbDisconnect(con_db)
}

rm(list = ls())

###################
#### Libraries ----

# Package names
packages <- c(
  "tidyverse",
  "openxlsx",
  "data.table",
  "ggOceanMaps",
  "DBI",
  "duckdb",
  "jsonlite",
  "knitr",
  "kableExtra"
)

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

##########################
## Function shortcuts ----

h <- head

########################
## Custom functions ----

## Read clipboard for Mac

read.clipboard <- function() read.table(pipe("pbpaste"), sep = "\t", header = T)

## Standard error of mean

#' @title Standard error of mean
#' @param x numeric vector

se <- function(x) {
  sd(x, na.rm = T) / sqrt(sum(!is.na(x)))
}

## Column numbers of a data frame

#' @title Column numbers of a data frame
#' @param x data.frame
#' @return retuns a named vector with column names as names and order of columns as elements
#' @author Mikko Vihtakari

coln <- function(x) {
  y <- rbind(seq(1, ncol(x)))
  colnames(y) <- colnames(x)
  rownames(y) <- "col.number"
  return(y)
}

## Check colors

#' @title Plot color vector to inspect the colors visually
#' @param cols a character vector containing accepted R \link[grDevices]{colors}
#' @return returns a base R plot with the colors given in \code{cols} argument
#' @author Mikko Vihtakari

check_cols <- function(cols) {
  if (is.null(names(cols))) {
    labs <- seq_along(cols)
  } else {
    labs <- paste0(names(cols), "\n[", seq_along(cols), "]")
  }

  mp <- barplot(
    rep(1, length(cols)),
    yaxt = "n",
    col = cols,
    border = NA,
    names.arg = labs,
    xlab = "Color sequence",
    ylim = c(0, 1.2)
  )
  points(mp, rep(1.1, length(cols)), col = cols, pch = 16, cex = 4)
}

## Select element from a list

#' @title Select an element of each vector from a list
#' @description Selects y'th element of each vector from a list
#' @param x list
#' @param y number of element. Must be integer

select.element <- function(x, y) sapply(x, "[", y)

## round_any

#' @title Round to multiple of any number
#' @param x numeric vector to round
#' @param accuracy number to round to; for POSIXct objects, a number of seconds
#' @param f rounding function: \code{\link{floor}}, \code{\link{ceiling}} or
#'  \code{\link{round}}
#' @keywords internal

round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

## Font and line size conversions for ggplot2

#' @title Convert font sizes measured as points to ggplot font sizes
#' @description Converts font sizes measured as points (as given by most programs such as MS Word etc.) to ggplot font sizes
#' @param x numeric vector giving the font sizes in points
#' @return Returns a numeric vector of lenght \code{x} of ggplot font sizes
#' @author Mikko Vihtakari

FS <- function(x) x / 2.845276

#' @title Convert line sizes measured as points to ggplot line sizes
#' @description Converts line sizes measured as points (as given by most programs such as Adobe Illustrator etc.) to ggplot font sizes
#' @param x numeric vector giving the lines sizes in points
#' @return Returns a numeric vector of lenght \code{x} of ggplot line sizes
#' @author Mikko Vihtakari

LS <- function(x) x / 2.13

###################
## Definitions ----

## Sizes and definitions for figures in Frontiers in Marine Science

colwidth <- 85 # mm
pagewidth <- 180 # mm
unit <- "mm"

colwidth_in <- colwidth * 0.0393701
pagewidth_in <- pagewidth * 0.0393701

## ggplot theme

theme_cust <- theme_classic(base_size = 8) %+replace%
  theme(
    strip.background = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank()
  )

theme_set(theme_cust) # Set default theme globally for the entire project

####################
## Color themes ----

## Functions to lighten and darken colors, source: https://gist.github.com/Jfortin1/72ef064469d1703c6b30

darken <- function(color, factor = 1.2) {
  col <- col2rgb(color)
  col <- col / factor
  col <- rgb(t(col), maxColorValue = 255)
  col
}


lighten <- function(color, factor = 1.2) {
  col <- col2rgb(color)
  col <- col * factor
  col[col > 255] <- 255
  col <- rgb(t(col), maxColorValue = 255)
  col
}

## Vector of standard colors

cols <- ColorPalette <- c(
  "#D696C8",
  "#449BCF",
  "#82C893",
  "#FF5F68",
  "#FF9252",
  "#FFC95B",
  "#056A89"
)

# check_cols(cols)

size_colors <- cols[1:3]
size_hues <- unlist(lapply(size_colors, function(k) {
  c(lighten(k), k, darken(k))
}))

# Connect to the database

## Make the connection

default_db_path <- Sys.getenv("BES_DATABASE_PATH", "paste your BES database path here")
bait_config_path <- path.expand("~/.bait/config.json")
config_db_path <- NULL

if (file.exists(bait_config_path)) {
  config_db_path <- tryCatch(
    jsonlite::read_json(bait_config_path, simplifyVector = TRUE)$bes_db_path,
    error = function(e) NULL
  )
}

db_path <- if (!is.null(config_db_path) && nzchar(config_db_path)) {
  config_db_path
} else {
  default_db_path
}

con_db <- db_path %>%
  path.expand() %>%
  normalizePath() %>%
  duckdb::duckdb(read_only = TRUE) %>%
  DBI::dbConnect()

## Create the data objects

stnall <- dplyr::tbl(con_db, "stnall")
indall <- dplyr::tbl(con_db, "indall")
ageall <- dplyr::tbl(con_db, "ageall")
mission <- dplyr::tbl(con_db, "mission")
meta <- dplyr::tbl(con_db, "metadata") %>%
  collect() %>%
  mutate(across(any_of(c("timestart", "timeend")), ~ as.POSIXct(.x, tz = "UTC")))
csindex <- dplyr::tbl(con_db, "csindex")
gearlist <- dplyr::tbl(con_db, "gearindex") %>% collect()

## END -----------------------
