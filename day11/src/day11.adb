--  Advent of Code 2018
--
--  John Perry
--
--  Day 11: Chronal Charge
--
--  part 1: determine the 3x3 fuel cell block with the largest power level,
--          where "power level" is a slightly complicated formula
--
--  part 2: repeat for any nxn block, where n in 1 .. 300
--
--  there are two solutions to Part 2: the "alternate" solution
--  is much quicker, but still slower than I'd like

pragma Ada_2022;

with Ada.Text_IO;

procedure Day11 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  Global types and variables

   --  SUBSECTION
   --  The grid itself

   subtype Unit is Positive range 1 .. 300;     --  grid size

   type Grid is array (Unit, Unit) of Integer;

   Single_Cell_Levels : Grid;

   --  SUBSECTION
   --  Functions related to determining the power level

   Serial_Number : constant Integer := 4455;    --  given by problem

   function Rack_ID (X, Y : Unit) return Integer is
      (Integer (X) + 10);

   function Hundreds_Digit (Value : Integer) return Integer is
      (Value / 100 - ((Value / 1000) * 10));

   function Power_Level (X, Y : Unit) return Integer is (
      Hundreds_Digit (
         (Rack_ID (X, Y) * Integer (Y) + Serial_Number) * Rack_ID (X, Y)
      ) - 5
   );

   procedure Determine_Power_Levels is
   --  initializes Grid so that we don't re-compute the values over and over
   begin

      for X in Unit loop
         for Y in Unit loop
            Single_Cell_Levels (X, Y) := Power_Level (X, Y);
         end loop;
      end loop;

   end Determine_Power_Levels;

   --  SECTION
   --  Part 1

   type Position_And_Level is record
      X, Y : Unit;
      Level : Integer;
   end record;

   function Part_1 return Position_And_Level is
      Result : Position_And_Level := (X => 1, Y => 1, Level => Integer'First);
   begin

      for X in Unit'First .. Unit'Last - 3 loop
         for Y in Unit'First .. Unit'Last - 3 loop

            declare
               Level : constant Integer
                  := Single_Cell_Levels (X, Y) + Single_Cell_Levels (X + 1, Y)
                        + Single_Cell_Levels (X + 2, Y)
                  + Single_Cell_Levels (X, Y + 1)
                        + Single_Cell_Levels (X + 1, Y + 1)
                        + Single_Cell_Levels (X + 2, Y + 1)
                  + Single_Cell_Levels (X, Y + 2)
                        + Single_Cell_Levels (X + 1, Y + 2)
                        + Single_Cell_Levels (X + 2, Y + 2);
            begin
               if Level > Result.Level then
                  Result := Position_And_Level'(X, Y, Level);
               end if;
            end;

         end loop;
      end loop;

      return Result;

   end Part_1;

   --  SECTION
   --  Part 2

   type Position_Size_And_Power is record
      X, Y : Unit;
      Size : Positive;
      Level : Integer;
   end record;

   --  SUBSECTION
   --  First attempt: naively compute levels for each grid point for which
   --  the size is valid
   --
   --  This is pretty slow, especially when the grid size is around 150

   function Block_Level (X, Y : Unit; Size : Positive) return Integer is
      Result : Integer := 0;
   begin
      for I in X .. X + Size - 1 loop
         for J in Y .. Y + Size - 1 loop
            Result := Result + Single_Cell_Levels (I, J);
         end loop;
      end loop;
      return Result;
   end Block_Level;

   function Part_2 return Position_Size_And_Power is
      Result : Position_Size_And_Power
         := (X => 1, Y => 1, Size => 301, Level => 0);
   begin
      for Size in reverse Unit loop
         IO.Put_Line ("Size" & Size'Image);
         for X in Unit'First .. Unit'Last - Size + 1 loop
            for Y in Unit'First .. Unit'Last - Size + 1 loop
               declare
                  Level : constant Integer := Block_Level (X, Y, Size);
               begin
                  if Level > Result.Level then
                     Result := (X, Y, Size, Level);
                  end if;
               end;
            end loop;
         end loop;
      end loop;
      return Result;
   end Part_2;

   --  SUBSECTION
   --  Second attempt: Build values up recursively
   --
   --  Basically, the value of any nxn block can be determined from
   --  the value of its upper-left (n-1)x(n-1) sub-block, then adding:
   --  - the 1x(n-1) column of values immediately to that sub-block's right,
   --  - the (n-1)x1 row of values immediately below that sub-block, and
   --  - the 1x1 corner value below or to the right of that row & column.
   --
   --  This saves us the hassle of recomputing every 2x2 block whenever we want
   --  to build a 150x150 block: we work with its upper-left 149x149 block,
   --  then tack on some extras.
   --
   --  If you're willing to retain more data, this can be sped up further
   --  by using other blocks.

   function Part_2_Alternate return Position_Size_And_Power is

      Larger_Levels : Grid := Single_Cell_Levels;
      Result : Position_Size_And_Power
         := (X => 1, Y => 1, Size => 301, Level => 0);

   begin

      --  initialize for 1x1 blocks
      for X in Unit loop
         for Y in Unit loop
            if Single_Cell_Levels (X, Y) > Result.Level then
               Result := (X, Y, 1, Single_Cell_Levels (X, Y));
            end if;
         end loop;
      end loop;

      for Size in 2 .. Unit'Last loop

         IO.Put_Line ("size" & Size'Image);  -- to inform user

         --  step 1: determine and store higher levels' values
         for X in 1 .. Unit'Last - Size + 1 loop
            for Y in 1 .. Unit'Last - Size + 1 loop

               --  walk the sides
               for Offset in 1 .. Size - 1 loop
                  Larger_Levels (X, Y) := @
                     + Single_Cell_Levels (X + Size - 1, Y + Offset - 1)
                     + Single_Cell_Levels (X + Offset - 1, Y + Size - 1);
               end loop;

               --  grab the corner
               Larger_Levels (X, Y) := @
                  + Single_Cell_Levels (X + Size - 1, Y + Size - 1);

               if Larger_Levels (X, Y) > Result.Level then
                  Result := (X, Y, Size, Larger_Levels (X, Y));
               end if;

            end loop;
         end loop;

      end loop;
      return Result;
   end Part_2_Alternate;

begin

   Determine_Power_Levels;

   --  part 1
   declare
      Part_1_Result : constant Position_And_Level := Part_1;
   begin
      IO.Put_Line (
         "Maximum for 3x3 is" & Part_1_Result.Level'Image
         & " at" & Part_1_Result.X'Image & "," & Part_1_Result.Y'Image
      );
   end;

   --  part 2 the fast way
   declare
      Part_2_Result : constant Position_Size_And_Power
         := Part_2_Alternate;
   begin
      IO.Put_Line (
         "Maximum for arbitrary size is" & Part_2_Result.Level'Image
         & " at" & Part_2_Result.X'Image & "," & Part_2_Result.Y'Image
         & " with size" & Part_2_Result.Size'Image
      );
   end;

   --  part 2 the slower way
   declare
      Part_2_Result : constant Position_Size_And_Power := Part_2;
   begin
      IO.Put_Line (
         "Maximum for arbitrary size is" & Part_2_Result.Level'Image
         & " at" & Part_2_Result.X'Image & "," & Part_2_Result.Y'Image
         & " with size" & Part_2_Result.Size'Image
      );
   end;
end Day11;
