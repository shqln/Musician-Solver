# Musician-Solver
Author: Shiqi (Daniel) Lin <sllin2@student.unimelb.edu.au>

Purpose: To provide a set of tools for simulating and solving a game 
of "Musician"

Description: This module provides a set of functions to simulate and play 
the game of "Musician", and also contains an algorithm for solving the game.
The algorithm has two parts, initialGuess and nextGuess; to use them, call
initial guess at the first guess, and then call nextGuess repeatedly until
the answer is found. The algorithm uses a GameState type to store the
remaining possible targets and is passed from guess to guess.

Musician is a logical guessing game where one player, the composer, comes up
with a Chord as the target, and another player, the performer, tries to 
guess the Chord. The performer receives feedback after each guess, which 
includes the number of correct Pitches, the number of Pitches with the
correct Notes but the wrong Octave, and the number of Pitches with the 
correct Octave but the wrong Note.

# The Game of Musician
Author: Peter Schachte <schachte@unimelb.edu.au>

Musician is a two-player logical guessing game.
For a Musician game, one player is the composer and the other is the 
performer. The composer begins by selecting a three-pitch musical chord, 
where each pitch comprises a musical note, one of A, B, C, D, E, F, or G, 
and an octave, one of 1, 2, or 3. This chord will be the target for the 
game. The order of pitches in the target is irrelevant, and no pitch may 
appear more than once. This game does not include sharps or flats, and no 
more or less than three notes may be included in the target.

Once the composer has selected the target chord, the performer repeatedly 
chooses a similarly defined chord as a guess and tells it to the composer, 
who responds by giving the performer the following feedback:

how many pitches in the guess are included in the target (correct pitches)
how many pitches have the right note but the wrong octave (correct notes)
how many pitches have the right octave but the wrong note (correct octaves)
In counting correct notes and octaves, multiple occurrences in the guess 
are only counted as correct if they also appear repeatedly in the target. 
Correct pitches are not also counted as correct notes and octaves. For 
example, with a target of A1, B2, A3, a guess of A1, A2, B1 would be counted
as 1 correct pitch (A1), two correct notes (A2, B1) and one correct octave 
(A2). B1 would not be counted as a correct octave, even though it has the 
same octave as the target A1, because the target A1 was already used to 
count the guess A1 as a correct pitch. A few more examples:

Target      Guess       Answer
A1,B2,A3    A1,A2,B1    1,2,1
A1,B2,C3    A1,A2,A3    1,0,2
A1,B1,C1    A2,D1,E1    0,1,2
A3,B2,C1    C3,A2,B1    0,3,3
The game finishes once the performer guesses the correct chord (all three 
pitches in the guess are in the target). The object of the game for the 
performer is to find the target with the fewest possible guesses.
