# for f in (**/*.csv) {echo $f}
library(ntdv)
library(music.as.networ)
options(echo=TRUE)
args <- commandArgs(trailingOnly = TRUE)
csv.file.name <- args[1]
pdf.music(csv.file.name)

