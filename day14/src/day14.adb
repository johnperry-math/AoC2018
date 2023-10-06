--  Advent of Code 2018
--
--  John Perry
--
--  Day 14: Chocolate Charts
--
--  part 1: what are the ten recipe scores
--          after number of recipes in puzzle input?
--
--  part 2: how many recipes before puzzle input appears in sequence?

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day14 is

   Doing_Example : constant Boolean := False;

   package IO renames Ada.Text_IO;

   --  SECTION
   --  Global types and values

   subtype Digit is Natural range 0 .. 9;
   package Int_IO is new IO.Integer_IO (Num => Digit);

   First_Recipes : constant array (Positive range <>) of Digit := [3, 7];

   Puzzle_Input : constant Natural
      := (if Doing_Example then 2018 else 846_021);

   package Digit_Vectors is new Ada.Containers.Vectors
      (Index_Type => Positive, Element_Type => Digit);

   Recipes : Digit_Vectors.Vector;

   procedure Init_Digits is
   begin

      for D of First_Recipes loop
         Recipes.Append (D);
      end loop;

   end Init_Digits;

   Elves : array (1 .. 2) of Positive := [1, 2];

   --  SECTION
   --  Parts 1 and 2

   --  SUBSECTION
   --  Common

   procedure Add_New_Recipes is
      Sum : constant Natural := Natural (Recipes.Element (Elves (1)))
         + Natural (Recipes.Element (Elves (2)));
   begin

      if Sum >= 10 then
         Recipes.Append (Sum / 10);
      end if;

      Recipes.Append (Sum rem 10);

   end Add_New_Recipes;

   procedure Take_Turn (Elf : Positive) is
   begin

      Elves (Elf) := (Elves (Elf) + Recipes (Elves (Elf)))
         rem Integer (Recipes.Length) + 1;

   end Take_Turn;

   --  SUBSECTION
   --  Part 1

   type Ten_Recipes is array (1 .. 10) of Digit;

   function Part_1 return Ten_Recipes is
      Result : Ten_Recipes;
   begin

      for Each in 1 .. (if Doing_Example then 2018 + 10 else Puzzle_Input + 10)
      loop
         Add_New_Recipes;
         for Elf in 1 .. 2 loop
            Take_Turn (Elf);
         end loop;
      end loop;

      for Each in Puzzle_Input + 1 .. Puzzle_Input + 10 loop
         Result (Each - Puzzle_Input) := Recipes (Each);
      end loop;

      return Result;

   end Part_1;

   --  SUBSECTION
   --  Part 2

   Sequence : Digit_Vectors.Vector;

   function Num_Matching (I : Positive) return Natural is
      Result : Natural := 0;
   begin

      for J in 1 .. Positive (Sequence.Length) loop

         if Sequence (J) = Recipes (I + J - 1) then
            Result := @ + 1;
         else
            exit;
         end if;

      end loop;

      return Result;

   end Num_Matching;

   function Part_2 return Natural is

      Idx : Positive := 1;
      Matches : Natural := 0;
      Value : Natural := (if Doing_Example then 59414 else Puzzle_Input);

   begin

      while Value > 0 loop
         Sequence.Append (Value rem 10);
         Value := @ / 10;
      end loop;
      Sequence.Reverse_Elements;

      while Idx < Natural (Recipes.Length) - Natural (Sequence.Length)
         and then Matches < Positive (Sequence.Length)
      loop

         Matches := Num_Matching (Idx);
         Idx := Idx + 1;

         if Idx + Natural (Sequence.Length) >= Natural (Recipes.Length) then
            Add_New_Recipes;
            for Elf in 1 .. 2 loop
               Take_Turn (Elf);
            end loop;
         end if;

      end loop;

      return (Idx - 1) - 1;   -- 1 to recover addition, 1 to count num to left

   end Part_2;

begin

   Init_Digits;

   declare
      First_Ten : constant Ten_Recipes := Part_1;
   begin

      IO.Put ("The 10 scores after the first" & Puzzle_Input'Image & " are ");
      for Each of First_Ten loop
         Int_IO.Put (Each, 0);
      end loop;
      IO.New_Line;

   end;

   IO.Put_Line ("Part 2 will take a few seconds, please wait...");
   declare
      Location : constant Positive := Part_2;
   begin

      IO.Put ("Found sequence ");
      for I in Location + 1 .. Location + Positive (Sequence.Length) loop
         Int_IO.Put (Recipes (I), 0);
      end loop;
      IO.Put_Line (" at" & Location'Image);

   end;
end Day14;
