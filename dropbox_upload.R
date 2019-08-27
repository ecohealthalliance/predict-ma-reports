#!/usr/bin/env Rscript

library(rdrop2)
library(cyphr)
library(sodium)
library(base64enc)

# Run once:
# # Generate dropbox token
# db_token <- rdrop2::drop_auth()
# saveRDS(db_token, file = "token.rds")
#
# # Generate key
# key <-sodium::keygen()
# key64 <- base64enc::base64encode(key)
#
# # Encrypt token
# cyphr::encrypt_file("token.rds", cyphr::key_sodium(base64decode(key64)), "token.rds.encrypted")

token <- decrypt(readRDS("token.rds.encrypted"), key = key_sodium(base64decode(Sys.getenv("SODIUM_KEY"))))
to_upload <- list.files("outputs", pattern = ".html", full.names = TRUE)
to_upload <- grep("Malaysia", to_upload, value = TRUE, invert = TRUE)
for (f in to_upload) {

  drop_upload(f, path  = "predict-ma-reports", dtoken = token, autorename = FALSE)
}
