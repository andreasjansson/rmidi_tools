library("rmidi")

extract.meter <- function(mat, min.subdiv = .25)
{
  # quantise to min.subdiv (default == semiquavers)
  notes <- midi.to.notes(mat)
  notes <- quantise(notes, min.subdiv)

  # build ioi sequence
  ioi.seq <- integer(max(notes[, "start"]) /
                     (midi.get.ppq() * min.subdiv) + 1)
  ioi.seq[notes[, "start"] / (midi.get.ppq() * min.subdiv) + 1] <-
    notes[, "duration"]

  # get the autocorrelation
  ioi.acf <- acf(ioi.seq, length(ioi.seq), plot = FALSE)$acf

  # find peaks by going backwards (acf is decreasing)
  peaks <- unique(cummax(rev(ioi.acf)))
  peaks.indices <- sort(unlist(lapply(peaks, function(peak) {
    which(ioi.acf == peak)
  })))

  # find the most common distance between peaks
  distances <- diff(peaks.indices)
  distances.mode <- as.numeric(names(which.max(table(distances))))

  # meter is the most common peak distance mutliplied by our
  # minimum subdivision
  meter <- distances.mode * min.subdiv
  return(meter)
}
