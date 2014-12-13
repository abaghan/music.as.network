#' makes a pdf file for the given piece of music
#' @export
pdf.music <- function(csv.file.name, pdf.file.name="", plot.title=""
           , displayisolates=F, displaylabels=T){

  # reads the csv data
  csv <- read.csv(csv.file.name, header=F, stringsAsFactors=F, sep=",", strip.white=T)

  # renames the columns
  names(csv) <- c("track", "time", "type", "channel", "note", "velocity")

  tracks <- subset(csv, type=="Program_c")$track

  if(pdf.file.name==""){
    dir.name = dirname(csv.file.name)
    dir.name = gsub("data", "report", dir.name)
    dir.create(dir.name, recursive=T )

    pdf.file.name = basename(csv.file.name)
    pdf.file.name = gsub(".csv", ".pdf", pdf.file.name)
    pdf.file.name = paste(dir.name, pdf.file.name, sep="/")
  }

  pdf(pdf.file.name)

  net.tracks <- network.initialize(128)

  for(cnt.t in tracks){
    net <- plot.music(csv.file.name, cnt.t
                      , displayisolates=displayisolates, displaylabels=displaylabels)
  }

  dev.off()

}
