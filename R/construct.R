#' Constructs a network out of a music in csv format converted from a midi file
#'
#' @param filename An address to csv file containing midi info
#' @param trackno A digit specifying the track
#' @examples \donotrun{
#' construct("../data/fur_elise.csv", 2)
#' }
#' @export
construct <- function(filename, trackno){

  # reads the csv data
  csv <- read.csv(filename, header=F)

  # renames the columns
  names(csv) <- c("track", "time", "type", "channel", "note", "velocity")

  # filters the track
  csv <- subset(csv, track == trackno)

  # filters metadata records (indicated by null velocity)
  csv <- subset(csv, is.na(velocity)==F)

  for(cnt.note in 0:128){
    print(cnt.note)
    csv.note <- subset(csv, note %in% cnt.note)
    print(head(csv.note))
  }

  head(csv)
}
