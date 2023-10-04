--  Advent of Code 2018
--
--  John Perry
--
--  Day 9: Marble Mania
--
--  part 1: determine the winner of a game of marbles
--
--  part 2: same, but with a lot more marbles

with Ada.Text_IO;

procedure Day9 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  Global types and variables

   Part_To_Solve : constant Natural := 2;
   --  modify to 1 or 2 for the part to solve

   --  SUBSECTION
   --  the basics

   Num_Players : constant Positive := 476;
   Largest_Marble : constant Long_Long_Integer
      := (if Part_To_Solve = 1 then 71_431 else 7_143_100);

   --  SUBSECTION
   --  the circle

   --  implemented as an array, one position for each marble,
   --  recording the marbles clockwise and counterclockwise of this one
   --
   --  Unix systems need to modify stack size via ulimit

   type Neighbors is record
      Counterwise, Clockwise : Long_Long_Integer;
   end record;

   Circle : array (0 .. Largest_Marble) of Neighbors
   --  set up the first two marbles played
      := [
         0 => Neighbors'(Counterwise => 1, Clockwise => 1),
         1 => Neighbors'(Counterwise => 0, Clockwise => 0),
         others => Neighbors'(Counterwise => 0, Clockwise => 0)
      ];

   --  SUBSECTION
   --  Players and scores
   --
   --  implemented as a modular type,
   --  so that I don't have to worry about resetting

   type Player is mod Num_Players;

   Scores : array (Player) of Long_Long_Integer := [others => 0];

   --  SECTION
   --  Parts 1 and 2
   --
   --  the part you play is determined by the constant `Part_To_Solve`, above

   function Part return Long_Long_Integer is

      Next_Marble : Long_Long_Integer;          --  next val of Current_Marble
      Last_Placed : Long_Long_Integer := 1;     --  last marble that was placed
      Current_Marble : Long_Long_Integer := 1;  --  where next play occurs
      Current_Player : Player := Player'First + 1;

   begin

      while Current_Marble <= Largest_Marble loop

         Last_Placed := @ + 1;

         if Last_Placed rem 23 = 0 then
            --  the special case:

            --  adjust the current marble 7 places counterclockwise,
            --  and remove this marble, starting next round ad the marble
            --  that was clockwise from it
            for Each in 1 .. 7 loop
               Current_Marble := Circle (Current_Marble).Counterwise;
            end loop;
            Next_Marble := Circle (Current_Marble).Clockwise;

            --  score the marble at this new position
            --  as well as the one you would otherwise have played,
            Scores (Current_Player) := @ + Last_Placed + Current_Marble;

            --  remove the marble
            Circle (Next_Marble).Counterwise
               := Circle (Current_Marble).Counterwise;
            Circle (Circle (Current_Marble).Counterwise).Clockwise
               := Next_Marble;
            Current_Marble := Next_Marble;

         else
            --  the ordinary case

            --  move clockwise
            Current_Marble := Circle (Current_Marble).Clockwise;

            --  insert marble
            Circle (Last_Placed).Clockwise
               := Circle (Current_Marble).Clockwise;
            Circle (Last_Placed).Counterwise := Current_Marble;
            Circle (Circle (Last_Placed).Clockwise).Counterwise := Last_Placed;
            Circle (Circle (Last_Placed).Counterwise).Clockwise := Last_Placed;

            --  prepare for next round
            Current_Marble := Last_Placed;
         end if;

         Current_Player := @ + 1;

      end loop;

      return Scores'Reduce (Long_Long_Integer'Max, 0);

   end Part;

begin
   IO.Put_Line ("Highest score is " & Part'Image);
end Day9;
