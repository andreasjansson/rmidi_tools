##
## RMidi Tools - Metre
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

## Metre detection by autocorrelation of inter-onset intervals,
## inspired by J. C. Brown "Detremination of the metre of musical scores
## by autocorrelation", 1993.
## This function requires the quantise.R file to be sourced beforehand.
extract.metre <- function(mat, min.subdiv = .25)
{
  # quantise to min.subdiv (default == semiquavers)
  notes <- midi.to.notes(mat)
  notes <- quantise.notes(notes, min.subdiv, FALSE)

  # build ioi sequence
  ioi.seq <- integer(max(notes[, "start"]) /
                     (midi.get.ppq() * min.subdiv) + 1)
  ioi.seq[notes[, "start"] / (midi.get.ppq() * min.subdiv) + 1] <-
    notes[, "duration"] / (midi.get.ppq() * min.subdiv)

  # get the autocorrelation 
  ioi.acf <- as.numeric(acf(ioi.seq, length(ioi.seq), plot = FALSE)$acf)

  # if span is so big that less than two peaks are detected,
  # gradually decrement the window.
  # this rarely happens, but we want to cater for those
  # corner cases as well.
  peaks.span <- 3 / min.subdiv
  repeat {

    # find peaks using R port of S+ peaks function
    peaks <- s.peaks(ioi.acf, peaks.span)
    peaks.indices <- which(peaks)

    # find peak distances
    distances <- diff(peaks.indices)

    # if distances were found (i.e. more than 1 peak), exit loop
    if(length(distances) > 0)
      break;

    # decrement span, possibly giving up
    peaks.span <- floor(peaks.span / 2)
    if(peaks.span < 2)
      return(0)
  }

  # find most common distance
  distances.mode <- as.numeric(names(which.max(table(distances))))

  # metre is the most common peak distance mutliplied by our
  # minimum subdivision
  metre <- distances.mode * min.subdiv
  return(metre)
}

# R port of S+ peaks function,
# http://finzi.psych.upenn.edu/R/Rhelp02a/archive/33097.html
s.peaks<-function(series,span=3) 
{ 
  z <- embed(series, span) 
  s <- span%/%2 
  v<- max.col(z) == 1 + s 
  result <- c(rep(FALSE,s),v) 
  result <- result[1:(length(result)-s)] 
  result 
} 
