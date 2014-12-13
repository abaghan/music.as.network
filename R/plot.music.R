plot.music <- function(csv.file.name, trackno, plot.title=""
           , displayisolates=F, displaylabels=T){

  # reads the csv data
  csv <- read.csv(csv.file.name, header=F, stringsAsFactors=F, sep=",", strip.white=T)

  # renames the columns
  names(csv) <- c("track", "time", "type", "channel", "note", "velocity")

  # filters the track
  csv <- subset(csv, track %in% trackno)

  # Plot title
  if(plot.title == ""){
    plot.title <- subset(csv, type=="Title_t")$channel
    plot.title <- paste(gsub(".csv","", csv.file.name), plot.title)
  }

  net <- construct(csv.file.name, trackno)

  plot(net, displaylabels=displaylabels, displayisolates=displayisolates
       , vertex.col=note.color(), main=plot.title)

  return(net)

  # render.animation(net)
  # saveVideo(ani.replay(),video.name=gsub("csv", ".mp4", csv.file.name)
  #           , ani.width=800)

}
