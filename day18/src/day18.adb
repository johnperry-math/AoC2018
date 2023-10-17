--  Advent of Code 2018
--
--  John Perry
--
--  Day 18: Settlers of The North Pole
--
--  part 1: compute the resource value of a rectangular area of trees and
--          lumberyards
--
--  part 2: what will the resource value be after 1 billion minutes?

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Hashed_Maps;

procedure Day18 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  Global types and variables

   type Filling is (Open, Woods, Lumberyard);

   Filling_Image : constant array (Filling) of Character := [
      Open => '.',
      Woods => '|',
      Lumberyard => '#'
   ];
   --  graphic representation for the Print_Area procedure (see below)

   Row_Dimension : constant Natural := 50;
   Col_Dimension : constant Natural := 50;

   subtype Row_Range is Natural range 1 .. Row_Dimension;
   subtype Col_Range is Natural range 1 .. Col_Dimension;

   type Area_Type is array (1 .. Row_Dimension, 1 .. Col_Dimension) of Filling;

   Initial_Area : Area_Type;

   --  SECTION
   --  I/O

   Bad_Input : exception;

   procedure Read_Input is
      F : IO.File_Type;
   begin

      IO.Open (F, IO.In_File, "input.txt");

      for Row in Row_Range loop

         declare
            S : constant String := IO.Get_Line (F);
         begin

            for Col in Col_Range loop
               Initial_Area (Row, Col) := (case S (Col) is
                  when '.' => Open,
                  when '|' => Woods,
                  when '#' => Lumberyard,
                  when others => raise Bad_Input
               );
            end loop;

         end;

      end loop;

   end Read_Input;

   procedure Print_Area (Area : Area_Type) is
   --  useful for debugging

   begin

      for Row in Row_Range loop
         for Col in Col_Range loop
            IO.Put (Filling_Image (Area (Row, Col)));
         end loop;
         IO.New_Line;
      end loop;

   end Print_Area;

   procedure Save_Area_To_File (Area : Area_Type; Iteration : Natural) is

      F : IO.File_Type;

      Suffix : String (1 .. 4) := "0000";
      Tmp_Suffix : constant String := Iteration'Image;

   begin

      --  copy suffix, accounting for initial space
      for Idx in reverse 2 .. Tmp_Suffix'Last loop
         Suffix (4 - (Tmp_Suffix'Last - Idx)) := Tmp_Suffix (Idx);
      end loop;

      --  open file and write header
      IO.Create (F, Name => "Images/frame_" & Suffix & ".ppm");
      IO.Put (F, "P3");
      IO.Put (F, " 200");
      IO.Put (F, " 200");
      IO.Put (F, " 255"); -- max color
      IO.New_Line (F);

      --  write image, dilating by a factor of 4 to make it more visible
      for Row in Row_Range loop
         for Each_Row in 1 .. 4 loop

            for Col in Col_Range loop
               for Each_Col in 1 .. 4 loop

                  IO.Put (F, (
                     case Area (Row, Col) is
                     when Open => " 129  63  11",
                     when Woods => "   0 192   0",
                     when Lumberyard => " 192 192 192"
                  ));

               end loop;
            end loop;

            IO.New_Line (F);
         end loop;

         IO.New_Line (F, 2);
      end loop;

      IO.Close (F);

   end Save_Area_To_File;

   --  SECTION
   --  Parts 1 and 2

   --  SUBSECTION
   --  counting and reporting neighbors

   type Offset is record
      Dx, Dy : Integer;
   end record;
   --  helps get to a neighboring position

   Offsets : constant array (1 .. 8) of Offset := [
      (-1, -1), ( 0, -1), ( 1, -1),
      (-1,  0),           ( 1,  0),
      (-1,  1), ( 0,  1), ( 1,  1)
   ];
   --  Dx, Dy offsets of neighbors

   type Neighbors is array (Filling) of Natural;
   --  how many neighbors contain each type of filling

   function Count_Neighbors (
      Area : Area_Type;
      Row : Row_Range;
      Col : Col_Range)
   return Neighbors
   is
   --  counts the cells adjacent to the one in the given row and column
   --  and returns the result

      Result : Neighbors := [Open => 0, Woods => 0, Lumberyard => 0];

   begin

      for O of Offsets loop
         if Row + O.Dy in Row_Range and then Col + O.Dx in Col_Range then
            Result (Area (Row + O.Dy, Col + O.Dx)) := @ + 1;
         end if;
      end loop;

      return Result;

   end Count_Neighbors;

   --  SUBSECTION
   --  the main iteration

   procedure Iterate (Area : in out Area_Type) is
      Next_Area : Area_Type;
   begin

      for Row in Row_Range loop
         for Col in Col_Range loop

            declare
               N : constant Neighbors := Count_Neighbors (Area, Row, Col);
            begin

               Next_Area (Row, Col) := (
                  case Area (Row, Col) is
                  when Open => (
                     if N (Woods) >= 3 then Woods
                     else Open
                  ),
                  when Woods => (
                     if N (Lumberyard) >= 3 then Lumberyard
                     else Woods
                  ),
                  when Lumberyard => (
                     if N (Lumberyard) >= 1 and then N (Woods) >= 1 then
                        Lumberyard
                     else
                        Open
                  )
               );

            end;

         end loop;
      end loop;

      Area := Next_Area;

   end Iterate;

   --  SUBSECTION
   --  reporting

   function Resource_Value (Area : Area_Type) return Natural is
      Num_Woods, Num_Yards : Natural := 0;
   begin
      for Row in Row_Range loop
         for Col in Col_Range loop
            case Area (Row, Col) is
            when Woods => Num_Woods := @ + 1;
            when Lumberyard => Num_Yards := @ + 1;
            when others => null;
            end case;
         end loop;
      end loop;
      return Num_Woods * Num_Yards;
   end Resource_Value;

   --  SUBSECTION
   --  Part 1

   function Part_1 return Natural is
      Area : Area_Type := Initial_Area;
   begin
      for Each in 1 .. 10 loop
         Iterate (Area);
      end loop;
      return Resource_Value (Area);
   end Part_1;

   --  SUBSECTION
   --  Part 2

   function Part_2 return Natural is
   --  we keep track of the number of times each value appears,
   --  quitting when it appears more than a predetermined number of times
   --  that convinces us we've hit a periodic cycle

      --  SUBSUBSECTION
      --  Part 2's personal playground

      Area : Area_Type := Initial_Area;

      --  SUBSUBSECTION
      --  tracking, part 1: data structures and variable

      function Nat_Hash (Value : Natural) return Ada.Containers.Hash_Type is
         (Ada.Containers.Hash_Type (Value));
      package Resource_Value_Maps is new Ada.Containers.Hashed_Maps (
         Key_Type => Natural,
         Element_Type => Positive,
         Hash => Nat_Hash,
         Equivalent_Keys => "="
      );
      use all type Resource_Value_Maps.Cursor;
      Resource_Value_Map : Resource_Value_Maps.Map;

      --  SUBSUBSECTION
      --  tracking, part 2: resource values and locating them in the map

      Value : Natural;
      Value_Location : Resource_Value_Maps.Cursor;

      --  SUBSUBSECTION
      --  calculation from observed cycle length, number of iterations, etc.

      Where_To_Find_It : Natural;
      Threshold : constant Positive := 4;
      Iterations, Guessed_Cycle_Length : Natural := 0;

   begin

      loop

         Save_Area_To_File (Area, Iterations);
         Iterations := @ + 1;
         Iterate (Area);

         --  compute and store resource value
         Value := Resource_Value (Area);
         Value_Location := Resource_Value_Map.Find (Value);
         if Value_Location = Resource_Value_Maps.No_Element then
            Resource_Value_Map.Insert (Value, 1);
         else
            Resource_Value_Map.Replace_Element (
               Value_Location,
               Resource_Value_Maps.Element (Value_Location) + 1
            );
            exit when Resource_Value_Maps.Element (Value_Location) > Threshold;
         end if;

      end loop;

      --  guess the cycle length from the number of values
      --  that have appeared at least the threshold number of times

      for C in Resource_Value_Map.Iterate loop
         if Resource_Value_Maps.Element (C) >= Threshold then
            Guessed_Cycle_Length := @ + 1;
         end if;
      end loop;

      IO.Put_Line (
         "Convinced after" & Iterations'Image & " iterations"
         & " that the cycle length is " & Guessed_Cycle_Length'Image
      );

      --  use it to determine how many more iterations we should run
      --  to obtain our eventual value

      Where_To_Find_It := (1_000_000_000 - Iterations)
         rem Guessed_Cycle_Length;

      --  run them

      for Each in 1 .. Where_To_Find_It loop
         Save_Area_To_File (Area, Iterations);
         Iterate (Area);
      end loop;

      return Resource_Value (Area);

   end Part_2;

begin
   Read_Input;
   Print_Area (Initial_Area);
   IO.Put_Line ("The resource value is" & Part_1'Image);
   IO.Put_Line (
      "The resource value after 1 billion minutes is" & Part_2'Image
   );
end Day18;
