##
## RMidi Tools - Quantise
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

## Convenience function, which also illustrates common usage.
midi.quantise <- function(mat, subdiv, offset = 0, preserve.duration = TRUE)
{
  notes <- midi.to.notes(mat)
  quantised.notes <- quantise.notes(notes, subdiv, offset, preserve.duration)
  quantised.matrix <- notes.to.midi(quantised.notes)

  return(quantised.matrix)
}

## Transform an RMidi matrix into a matrix of note start times,
## durations, pitches and velocities (discarding any none-note information).
midi.to.notes <- function(mat)
{
  mat <- mat[order(mat[, midi.cols.tick]), ]
  notes <- matrix(nrow = 0, ncol = 4)
  colnames(notes) <- c("start", "duration", "pitch", "velocity")

  for(i in 1:nrow(mat)) {
    if(mat[i, midi.cols.command] == midi.cmd.noteon) {
      start <- mat[i, midi.cols.tick]
      pitch <- mat[i, midi.cols.byte1]
      velocity <- mat[i, midi.cols.byte2]
      duration <- mat[mat[, midi.cols.tick] > start &
                      mat[, midi.cols.command] == midi.cmd.noteoff &
                      mat[, midi.cols.byte1] == pitch, ][1] - start
      notes <- rbind(notes, c(start, duration, pitch, velocity))
    }
  }

  return(notes)
}

## Transform a matrix of note start times, durations, pitches and velocities
## into an RMidi matrix.
notes.to.midi <- function(notes)
{
  return(midi.note(notes[, "start"],
                   notes[, "duration"],
                   notes[, "pitch"],
                   notes[, "velocity"]))
}

## Quantise a monophonic melody to a set minimum subdivision (in crotchets).
## This is achieved by first rounding start times, and then resolving
## collisions.
##
## If two or more notes in the rounded matrix end up with
## equal start times, the note that was closest to the quantised time
## in the original matrix will be chosen as the note to keep in the
## quantised matrix. If two notes are equally close (i.e. one up and one
## down), then lower one will be chosen.
##
## Offset provides a way to hangle upbeats, by offsetting quantisation.
## This paramater is also given in crotchets.
## 
## If preserve.duration is FALSE, notes will be stretched to the next
## note. The last note duration will be stretched so that the quantised
## melody is the same length as the input melody.
quantise.notes <- function(notes, subdiv, offset = 0,
                           preserve.duration = TRUE)
{
  if(subdiv == 0)
    return(notes)

  # first, do "naive" quantising, by just rounding start times
  subdiv.ticks <- subdiv * midi.get.ppq()
  offset.ticks <- offset * midi.get.ppq()
  notes.quantised <- notes
  notes.quantised[, "start"] <-
    round((notes.quantised[, "start"] - offset.ticks) /
          subdiv.ticks) * subdiv.ticks + offset.ticks

  # find any collisions
  tbl <- sort(table(notes.quantised[, "start"]))
  collisions <- as.numeric(names(tbl[tbl > 1]))

  # if there are collisions, remove all notes that are not the note
  # closest to the quantised value
  if(length(collisions) > 0) {
    remove.indices <- integer(0)
    for(collision in collisions) {
      colliding.indices <- which(notes.quantised[, "start"] == collision)
      distances <- abs(notes[, "start"] - collision)

      # if two colliding notes are equally close, use the lower one
      closest.index <- which(distances == min(distances))[1]

      remove.indices <- c(remove.indices,
                          colliding.indices[colliding.indices !=
                                            closest.index])
    }

    # remove colliding notes
    notes.quantised <- notes.quantised[-remove.indices, ]
  }

  # if we don't preserve the original durations, we tie the notes
  if(!preserve.duration) {

    # order notes by start time
    notes.quantised <- notes.quantised[order(notes.quantised[, "start"]), ]

    # tie all notes except last note
    for(i in 1:(nrow(notes.quantised) - 1)) {
      notes.quantised[i, "duration"] <- notes.quantised[i + 1, "start"] -
        notes.quantised[i, "start"]
    }

    # set the duration of the last note such that the duration of
    # the quantised melody is the same as the duration of the
    # original melody
    notes.quantised[nrow(notes.quantised), "duration"] <-
      notes[nrow(notes), "duration"] + notes[nrow(notes), "start"] -
        notes.quantised[nrow(notes.quantised), "start"]
  }

  return(notes.quantised)
}
