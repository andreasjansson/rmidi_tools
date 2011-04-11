##
## RMidi Tools - Meter
## Copyright (C) 2011 Andreas Jansson
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
##


library("rmidi")

## Meter detection by autocorrelation of inter-onset intervals,
## inspired by J. C. "Determination of the meter of musical scores
## by autocorrelation", 1993.
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
