load("twitterFunctions.Rdata")
args <- commandArgs(trailingOnly = TRUE)
suppressMessages(process.files(as.character(args[1]), as.character(args[2]), as.logical(args[3])))
