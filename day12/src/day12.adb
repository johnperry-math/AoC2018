--  Advent of Code 2018
--
--  John Perry
--
--  Day 12: Subterranean Sustainability
--
--  part 1: sum the values of pots with plants after 20 generations
--
--  part 2: repeat 50 billion generations

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers;

procedure Day12 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  Global types and variables

   --  SUBSECTION
   --  the pots, and what's in them

   type Pot is (Empty, Plant);

   subtype Pots_Range is Integer range -1650 .. 1750;

   type Pot_Array is array (Pots_Range) of Pot;

   Initial_State : Pot_Array := [others => Empty];

   --  SUBSECTION
   --  the rules

   subtype Rule_Range is Natural range 0 .. 31;

   Rules : array (Rule_Range) of Pot := [others => Empty];

   function Rule (S : String) return Rule_Range is
   --  computes a rule from a string of '#' and '.'

      Result : Natural := 0;
   begin

      for Idx in 1 .. 5 loop
         if S (Idx) = '#' then
            Result := @ + 2 ** (Idx - 1);
         end if;
      end loop;

      return Result;

   end Rule;

   type Pot_Rule_Range is array (1 .. 5) of Pot;

   function Rule (P : Pot_Rule_Range) return Rule_Range is
   --  computes a rule from a sequence of 5 pots

      Result : Natural := 0;
   begin

      for Idx in 1 .. 5 loop
         if P (Idx) = Plant then
            Result := @ + 2 ** (Idx - 1);
         end if;
      end loop;

      return Result;

   end Rule;

   --  SECTION
   --  I/O

   procedure Put_Pots (P : Pot_Array; First, Last : Pots_Range) is
   --  displays contents of P from First to Last

   begin

      for Idx in First .. Last loop
         IO.Put (if P (Idx) = Plant then '#' else '.');
      end loop;

   end Put_Pots;

   procedure Read_Input is
      F : IO.File_Type;
   begin

      IO.Open (F, IO.In_File, "input.txt");

      --  read initial state
      declare
         S : constant String := IO.Get_Line (F);
      begin
         for Idx in 16 .. S'Last loop
            Initial_State (Idx - 16)
               := (if S (Idx) = '.' then Empty else Plant);
         end loop;
      end;

      IO.Skip_Line (F);

      --  read rules
      while not IO.End_Of_File (F) loop
         declare
            S : constant String := IO.Get_Line (F);
         begin
            Rules (Rule (S)) := (if S (10) = '#' then Plant else Empty);
         end;
      end loop;

      IO.Close (F);

   end Read_Input;

   --  SECTION
   --  Parts 1 and 2

   function Score (P : Pot_Array; First, Last : Pots_Range) return Integer is
   --  sums indices of pots containing plants, within range First .. Last
      Result : Integer := 0;
   begin

      for Idx in First .. Last loop
         if P (Idx) = Plant then
            Result := Result + Idx;
         end if;
      end loop;

      return Result;

   end Score;

   procedure Iterate (
      Current : in out Pot_Array;
      Leftmost, Rightmost : in out Pots_Range
   )
   is
   --  apply the rules to Current, in range Leftmost - 2 .. Rightmost + 2,
   --  adjusting Leftmost and Rightmost if range of plants expands

      Next : Pot_Array;

   begin

      --  iteration
      Next := [others => Empty];
      for P in Leftmost - 2 .. Rightmost + 2 loop
         Next (P) := Rules (Rule (Pot_Rule_Range (Current (P - 2 .. P + 2))));
      end loop;
      Current := Next;

      --  adjust bounds
      if Current (Leftmost - 2) = Plant then
         Leftmost := @ - 2;
      elsif Current (Leftmost - 1) = Plant then
         Leftmost := @ - 1;
      end if;
      if Current (Rightmost + 2) = Plant then
         Rightmost := @ + 2;
      elsif Current (Rightmost + 1) = Plant then
         Rightmost := @ + 1;
      end if;

   end Iterate;

   --  SUBSECTION
   --  Part 1

   function Part_1 return Integer is
      Leftmost, Rightmost : Pots_Range;
      P : Pots_Range := Pots_Range'First;
      Current : Pot_Array := Initial_State;
   begin

      --  find leftmost, rightmost plants

      while Initial_State (P) = Empty loop
         P := @ + 1;
      end loop;
      Leftmost := P;

      P := Pots_Range'Last;
      while Initial_State (P) = Empty loop
         P := @ - 1;
      end loop;
      Rightmost := P;

      IO.Put_Line (
         "Leftmost" & Leftmost'Image & " Rightmost" & Rightmost'Image
      );

      --  now iterate
      for Each in 1 .. 20 loop
         Iterate (Current, Leftmost, Rightmost);
      end loop;

      return Score (Current, Leftmost, Rightmost);

   end Part_1;

   --  SUBSECTION
   --  Part 2

   subtype Big_Integer is Ada.Numerics.Big_Numbers.Big_Integers.Big_Integer;
   use all type Big_Integer;

   type Trend is record
      Last_Value : Integer;
      Difference : Integer;
      Guess : Big_Integer;
   end record;

   function Part_2 return Trend is

      Current : Pot_Array := Initial_State;
      Leftmost, Rightmost : Pots_Range;
      P : Pots_Range := Pots_Range'First;

      Old_Score : Integer := 0;
      New_Score, Difference : Integer;

   begin

      --  find leftmost, rightmost plants
      while Initial_State (P) = Empty loop
         P := @ + 1;
      end loop;
      Leftmost := P;

      P := Pots_Range'Last;
      while Initial_State (P) = Empty loop
         P := @ - 1;
      end loop;
      Rightmost := P;

      --  main loop
      for Each in 1 .. 1_000 loop

         Iterate (Current, Leftmost, Rightmost);
         New_Score := Score (Current, Leftmost, Rightmost);

         Difference := New_Score - Old_Score;
         Old_Score := New_Score;

         --  sanity check: print the last few scores so user can verify
         --  stable Difference
         if Each > 995 then
            IO.Put_Line (
               Each'Image & New_Score'Image & " (" & Difference'Image & ")"
            );
         end if;

      end loop;

      return Trend'(
         Last_Value => New_Score,
         Difference => Difference,
         Guess => From_String ("49_999_999_000") * To_Big_Integer (Difference)
            + To_Big_Integer (New_Score)
      );

   end Part_2;

begin

   Read_Input;

   IO.Put_Line ("After 20 generations, the value is" & Part_1'Image);

   declare
      Eventually : Trend := Part_2;
   begin

      IO.Put_Line (
         "After 50 billion generations, the value is"
         & To_String (Eventually.Guess)
      );
      IO.Put_Line ("For the correct answer to part 2,");
      IO.Put_Line (" 1) the value in parentheses should not be changing");
      IO.Put_Line (" 2) Multiply that value by 49_999_999_000");
      IO.Put_Line (" 3) Add to the number left of the value in parentheses");
      IO.Put_Line ("The result should match the guess.");
      IO.Put ("To wit: 49_999_999_000 x" & Eventually.Difference'Image);
      IO.Put (
         " + " & Eventually.Last_Value'Image
         & " =" & To_String (Eventually.Guess)
      );

   end;

end Day12;
