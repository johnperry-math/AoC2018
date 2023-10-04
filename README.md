# Advent of Code 2018 in Ada

[<img src="ada_logo.svg" width="200">](ada_logo.svg)

Because 4 years of pain and suffering aren't enough. :grin:
By the time I got 'round to doing this, I knew Ada's library well enough
that I no longer mention it in the **Tools** subsections
unless it was something truly unusual.

## Days I completed without doing the example first

### Day 1: Chronal Calibration

Your mobile time-travel watch wasn't calibrated before you left,
and you need to calibrate it in medias res.

1. Determine the total frequency drift.
1. Determine the first frequency you reach twice.

#### Tools
* The new `'Reduce` attribute with `"+"`.

#### Experience
Fun and easy.

### Day 2: Inventory Management System

Boxes have an ID with five symbols.

1. Determine which Ids repeat two symbols and which have repeat three symbols.
   Some boxes may have fall in both camps; count them twice.
1. Find the two boxes whose symbols differ in only one position.
   Report the common symbols.

#### Tools
Ada 2022's new bracket-initialization syntax for arrays.

#### Experience
Fun and easy.

### Day 5: Alchemical Reduction

You're given a chain of 50,000 polymers with certain units and polarity.
When two adjoining polymers have the same unit but different polarity,
they reduce.

1. Reduce the chain.
1. Identify the unit which, when both its polarities
   are removed from the chain, leads to a shortest reduced chain.


#### Tools
* Discriminated `Polymer` type for the diagnostic.

#### Experience
Fun and easy.

### Day 7: The Sum of Its Parts

You need to assemble Santa's sleigh.

1. Determine the order in which the steps must be done
   when one person is working alone.
1. Determine how long it will take to complete the instructions
   when five people are working together.

Part 2 is made a little trickier
thanks to a time penalty associated with each step:
* Step A: 61 seconds
* Step B: 62 seconds
* ...

#### Tools

I relied heavily on arrays indexed by the subrange of `Character`
corresponding to the steps. It's fairly easy to represent this graph that way.

#### Experience

Largely fun and easy. I made several mistakes,
but the problem was simple enough to "self-debug".

Although I downloaded the example and ran my final implementation on it
to verify that it would produce the correct answer,
I was by that point already fairly confident that my answer was correct,
but competition rules required me to wait five minutes before I tried again.
Having nothing else to do, I tried the example, but I don't feel it belongs
with the usual days where I needed the example to solve the problem./

### Day 9: Marble Madness

The elves play a game with marbles numbered
from 0 to some obscenely high number.
The rules of play depend on whether the marble's number is a multiple of 23:
when it is, the elf keeps the marble
and takes the marble 7 spaces counter-clockwise of the current marble.
The new current marble is the one clockwise of the one just taken.

1. Find the highest score with a certain number of elves and marbles.
1. Repeat when there are 100 times more marbles.

#### Tools
* Again with `'Reduce`, only this time I needed `Long_Long_Integer'Max`.
* I originally solved Part 2 using `Big_Integers` but then realized that
  `Long_Long_Integer` would do the trick fine.

#### Experience
Surprisingly fun and easy, considering it's a Day 9.
For both Day 8 and Day 9 I've had an initial panic when I first saw the problem
and remembered similar problems that had bedeviled me in other years' puzzles,
but for Day 9 I managed the correct answer for both parts on the first try --
setting aside the stack overflow and integer overflow errors
that I first encountered.

### Day 10: The Stars Align

Having trouble getting directions to the North Pole.
There's a system of points arranged in the sky, always in motion.
If you can determine the message it spells out
once the points arrives at the correct position, you're home free, so to speak.

1. Find the message.
1. Find how long the elves would have had to wait before they saw the message.

#### Tools

Nothing special. Probably could have solved it with Ada 83.

#### Experience

Fun and easy. Kind of surprised: two days in a row!
In fact, I had fewer troubles with this day than with just about any other.

### Day 11: Chronal Charge

Time-traveling again, your device warns that it's low on power.
It has a 300x300 grid of cells, and you need to find the block of cells
with the highest power level, where the "power level" is a curious function
of the cell's position.

1. Do this for a 3x3 block.
1. Do this for an arbitrary-sized block.

#### Tools

The "alternate" solution to Part 2 (which is now done first)
builds up the various levels via a kind of recursion.
It's similar to the
[Bareiss algorithm](https://en.wikipedia.org/wiki/Bareiss_algorithm)
for computing a determinant.

#### Experience

Fun and easy. The only difficulty I had with my first implementation of Part 2
was that I needed some patience (it's slow) and then I accidentally typed the
position in wrong (`236,238,11` instead of `236,268,11`).

I then tried to implement Part 2 more efficiently, but kept wrecking that.
Having the first, slow solution working made that more tolerable.

## Days I tried to solve without the example, but had to give in

### Day 3: No Matter How You Slice It

The elves want to claim rectangles from a very large fabric.

1. Determine the number of square inches with overlapping claims.
1. Identify the one claim that doesn't overlap.

#### Tools
* Custom `Read_Nat` to read natural numbers, thanks to a well-known,
  long-standing Ada ~~bug~~ feature.

#### Experience
I had issues with Ada's unwieldy `Hashed_Maps` API.
I also realized an immense performance improvement by changing
from a badly-chosen hash function to a more reasonable one.

### Day 4: Repose Record

You need to sneak into a storage room.
Every day, one of several guards comes in before midnight,
and during the midnight hour he takes a nap.

1. During what minute does the guard who sleeps most, sleep most?
1. During which minute does some guard sleep most?

#### Tools

* The new `'Reduce` attribute with `"+"`.
* I had some issues with generating a discriminated record
  in a `case` statement without branching on every enumeration variant.
  This would be annoying when the enumeration has many variants,
  and multiple variants share the same instructions.
  Some help from the folks at
  [ada-lang.io](https://forum.ada-lang.io/t/initializing-a-discriminated-record/485)
  helped me get over that,
  _and_ introduced me to the idea of an extended return statement.

#### Experience
This took a long time in part because I overthought it at first.
Only check-ins occur on the day before, and they only do that _sometimes_.
Eventually I realized that putting all the "actions" into a vector,
then sorting the vector by its timestamp,
would automatically take care of what I wanted.

The only reason I needed the example
was my misinterpretation of what Part 1 wanted.

### Day 6: Chronal Coordinates

You're traveling through time and approaching different coordinates.
You want to make sure they're safe.

1. If they're dangerous, you want to find the location whose area
   gives the largest distance from other points. Report that area's size.
1. If they're safe, find the size of the area whose points
   all have a sum of distances from the coordinates less than 10_000.

#### Tools
* Already needed a breadth-first search queue for Part 2.
* The new `'Reduce` attribute with `Natural'Max`.

#### Experience
For some reason, my first implementation of part 1 mis-classifies
one coordinate as still claiming positions, even though it's clearly finite,
while another coordinate has the wrong number of points.
I haven't yet figured out why not, since printing the grid shows
the mis-classified coordinate to be clearly finite.

A silver lining is that the second implementation,
which _does_ give the correct answer,
seems simpler and more efficient than the first.

### Day 8: Memory Maneuver

Your device won't given directions because it can't read the license file.
You need to decode it.

1. Report the sum of all nodes' meta values.
1. Report the value of the root node, where the value of a node is:
   * the sum of its meta values when it has no children, and
   * the sum of its legitimate children's values otherwise.

#### Tools
* The new `'Reduce` attribute with `"+"`, but I had already done that
  on days 1 and 4.

#### Experience
This was fun and easy. I got Part 1 right on the first try!
I'm not sure what I was doing wrong in my first attempt with Part 2,
but after I downloaded the example and tinkered with it a little,
I had the right answer.

### Day 12: Subterranean Sustainability

You're in a cavern. Someone's growing potted plants with geothermal heat.
(Curious plants, which don't require sunlight.)
You notice that someone has scribbled the rules of their growth patterns.

1. Report the sum of the indexes of the pots that have plants
   after 20 generations.
1. Report the sum of the indexes of the pots that have plants
   after 50 billion generations.

#### Tools

* I didn't _need_ the `Big_Integers` type to solve the problem,
  but once I had the solution I went ahead and used it
  to generate the solution automatically.
* The plants' growth rate turns out to stabilize eventually;
  in my case, it stabilizes at 21 well before 1,000 generations.
  So it's a simple matter of calculating an arithmetic sum for the number
  of generations remaining after 1,000.

#### Experience

Fun and mostly easy. I made some silly mistakes when I first initialized
both `Rules` and `Initial_State`, and rather than analyze it
with unknown data I downloaded the example and work with that.
The bugs were relatively easy to fix, so perhaps I should have kept at it
without working the example.

### Day 13: Mine Cart Madness

Elves push carts around on tracks, heedless of their eventually crashing.

1. Report first crash's location.
1. Report the last surviving cart's location.

#### Tools

* `Ada.Containers.Generic_Sorter`

Nothing else I'm not used to by know.

#### Experience
* Part 1 was easy, but implementing it was a bit tedious.
  I don't _think_ I over-engineered it, and
  I had the right answer on the first try, which would seem to support that.
  (I did neglect to convert the original output,
  which was in positive coordinates, to the puzzle's desired format,
  which is in natural coordinates. The final program does that automatically.)

*  Part 2 was also easy, but I overlooked a detail in the instructions.
   An important caveat is that the carts move in a certain order:
   topmost cart, breaking ties leftmost.
   My struggles with this made me try the example, but of course
   it's trivial enough that this constraing doesn't matter.
   It was only after looking at the Reddit page discussion
   that I saw this specified in the directions.

* Doing the example didn't help me solve the problem,
  but technically I _did_ the example, so this puzzle lies in this section.

#### Visualziation

I'd like to do this eventually, but I haven't done it yet.

## Days I completed only after doing the example first

None yet!