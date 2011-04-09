##
## RMidi Tools - Chroma functions
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

## Extract chroma from melody
## A chroma is here defined as relative frequencies of pitches,
## independant on octave. The chroma is transformed by the fourth root,
## in order to increase the distance between non-occurring pitches and
## infrequently occurring pitches.
## Note that the melody is simply taken modulo 12, and since midi note
## 0 is an E flat, index 0 in the returned chroma corresponds to E flat,
## index 1 to E, etc.
extract.chroma.melody <- function(melody)
{
  pitches <- melody %% 12
  chroma <- integer(12)
  for(i in 0:11) {
    chroma[i + 1] <- sum(pitches == i)
  }
  # make high values closer
  chroma <- (chroma / max(chroma)) ^ (1/4)
  return(chroma)
}

## Convenient way to extract chroma from midi file
extract.chroma.file <- function(filename)
{
  m <- midi.read.file(filename)
  melody <- m[,midi.cols.byte1]
  chroma <- extract.chroma.melody(melody)
  return(chroma)
}

## Convenient way to extract chromas from multiple files.
## The result is returned as a matrix where each row represents
## an extracted chroma.
extract.files.chromas <- function(filenames)
{
  result <- matrix(ncol = 12, nrow = length(filenames))
  colnames(result) <- c("ds", "e", "f", "fs", "g", "gs", "a", "as",
                        "b", "c", "cs", "d")
                       
  for(i in 1:length(filenames)) {
    result[i,] <- extract.chroma.file(filenames[i])
  }
  rownames(result) <- sub("\\.mid$", "", basename(filenames))

  return(result)
}

## Get the distance matrix from a matrix of chromas (for example,
## extracted by extract.files.chromas).
## Returns a matrix in the same format as that returned by dist(),
## and can therefore be used in functions like agnes().
get.distance.matrix <- function(chromas)
{
  result <- array(dim=c(nrow(chromas), nrow(chromas)))
  for(y in 1:(nrow(chromas) - 1)) {
    for(x in (y + 1):nrow(chromas)) {
      result[x, y] <- find.closest.distance(chromas[x, ], chromas[y, ])
    }
  }

  return(result)
}

## Convenient way to get closest distance between two chromas
find.closest.distance <- function(chroma1, chroma2)
{
  return(find.best.transpose.dist(chroma1, chroma2)$dist)
}

## Convenient way to get most likely transposition between two chromas
find.best.transpose <- function(chroma.fixed, chroma.moving)
{
  return(find.best.transpose.dist(chroma.fixed, chroma.moving)$transpose)
}

## Convenient way to get the key of a chroma, assuming normal,
## western major scale, relative to C. A result of 0 means that the
## chroma is in C, 1 means C#, etc.
find.major.scale <- function(chroma)
{
  major <- c(0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1)
  return(find.best.transpose(major, chroma))
}

## Find the best transposition and distance between two chromas.
## Best refers to the transposition that results in the shortest
## Manhattan distance between the two chromas.
find.best.transpose.dist <- function(chroma.fixed, chroma.moving)
{
  best.dist <- +Inf
  best.transpose <- NA
  chroma.fixed <- as.numeric(chroma.fixed)
  chroma.moving <- as.numeric(chroma.moving)
  for(i in 0:11) {
    # optimised manhattan dist()
    new.dist <- sum(abs(chroma.fixed - shift(chroma.moving, -i)))
    if(new.dist < best.dist) {
      best.dist <- new.dist
      best.transpose <- i
    }
  }

  return(list(dist = best.dist, transpose = best.transpose))
}

## Utility function to apply circular shift to vector.
shift <- function(v, s)
{
  if(s == 0)
    return(v)
  else if(s > 0)
    return(v[c((length(v) - s + 1):length(v), 1:(length(v) - s))])
  else
    return(v[c((-s + 1):length(v), 1:-s)])
}
