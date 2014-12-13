# for f in (**/*.csv) {echo $f}
library(ndtv)
library(music.as.network)
options(echo=TRUE)
args <- commandArgs(trailingOnly = TRUE)
csv.file.name <- args[1]
pdf.music(csv.file.name)

