#!/usr/bin/env Rscript

library(rdrop2)
library(cyphr)
library(sodium)
library(base64enc)
#
#
# token <- decrypt(readRDS("token.rds.encrypted"), key = key_sodium(base64decode(Sys.getenv("SODIUM_KEY"))))
# to_upload <- list.files("outputs", pattern = "(html|xlsx)$", full.names = TRUE)
# for (f in to_upload) {
#   drop_upload(f, path  = "shared/eidith-cleaning-reports-outputs/", dtoken = token, autorename = FALSE)
# }