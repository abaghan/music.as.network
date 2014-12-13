#' Constructs a network out of a music in csv format converted from a midi file
#'
#' @param csv.file.name An address to csv file containing midi info
#' @param trackno A digit specifying the track
#' @export

construct <- function(csv.file.name, trackno){

  set.seed(5041)

  # initiates a network for 128 notes
  # man <- network.initialize(128)

  # reads the csv data
  csv <- read.csv(csv.file.name, header=F, stringsAsFactors=F, sep=",", strip.white=T)

  # renames the columns
  names(csv) <- c("track", "time", "type", "channel", "note", "velocity")

  # filters the track
  csv <- subset(csv, track %in% trackno)

  # filters metadata records (indicated by null velocity)
  csv <- subset(csv, is.na(velocity)==F)

  # filters control_c
  csv <- subset(csv, type!="Control_c")

  csv.on <- subset(csv, velocity>0)
  csv.off <- subset(csv, velocity==0)

  # makes a vector of change times
  change.times <- unique(csv$time)

  matv <- matrix(0, length(change.times), 128)
  matv <- data.frame(matv)
  colnames(matv) <- 1:128
  rownames(matv) <- change.times
  matx <- matv
  matn <- matv
  matn[, ] <- ""

  for(i in 1:nrow(csv.on)) {
    note = NULL
    time.start <- NULL
    time.end <- NULL
    velocity <- NULL
    off <- NULL

    note <-  csv.on[i, ]$note
    time.start <- csv.on[i, ]$time
    velocity <- csv.on[i, ]$velocity

    off <- csv.off[csv.off$note == note & csv.off$time > time.start, ]
    time.end = off[1, ]$time

    matn[rownames(matn)==time.start, note] <- "+"
    matn[rownames(matn)==time.end, note] <- "*"
    for(cnt.t in change.times[ change.times>time.start & change.times<time.end]){
      matn[rownames(matn)==cnt.t, note] <- "|"
    }

    for(cnt.t in change.times[ change.times>=time.start & change.times<=time.end]){
      matv[rownames(matv)==cnt.t, note] <- velocity
    }
    for(cnt.t in change.times[ change.times>=time.start & change.times<=time.end]){
      matx[rownames(matx)==cnt.t, note] <- as.numeric(note)
    }

  }

  adj.lst <- list()
  adj <- matrix(0, 128, 128)
  for(cnt.i in 1:nrow(matx)){
    x <- matx[cnt.i, ]
    adj.tmp <- adj
    x <- x[x!=""]
    for(cnt.x in x){
      x <- x[x!=cnt.x]
      for(cnt.y in x){
        adj.tmp[cnt.x, cnt.y] <- 1
        adj.tmp[cnt.y, cnt.x] <- 1
      }
    }
    adj.lst[[cnt.i]] <- as.network.matrix(adj.tmp
                                          , matrix.type="adjacency"
                                          , directed=F)
  }

  net <- networkDynamic(network.list=adj.lst, directed=F)

  return(net)

}
