--  Advent of Code 2018
--
--  John Perry
--
--  Day 1: Chronal Calibration
--
--  part 1: determine the total frequency drift
--
--  part 2: determine the first repeated drift

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;

procedure Day1 is

   --  SECTION
   --  global types and variables

   package IntVecs is new Ada.Containers.Vectors (
      Index_Type => Positive, Element_Type => Integer
   );
   Input : IntVecs.Vector;

   --  SECTION
   --  I/O

   package ATIO renames Ada.Text_IO;
   package IntIO is new Ada.Text_IO.Integer_IO (Num => Integer);

   procedure Read_Input is
      File  : ATIO.File_Type;
      Drift : Integer;
   begin
      ATIO.Open (File, ATIO.In_File, "input.txt");
      while not ATIO.End_Of_File (File) loop
         IntIO.Get (File, Drift, Width => 0);
         Input.Append (Drift);
      end loop;
      ATIO.Close (File);
   end Read_Input;

   --  SECTION
   --  parts 1 and 2

   function Part1 return Integer is (Input'Reduce ("+", 0));
   --  first opportunity to use the 'Reduce container attribute!
   --  hence the -gnatX configuration option

   Should_Not_Happen : exception;

   function Part2 return Integer is
   --  this solution computes all frequencies until it finds a repetition
   --  there might be a more clever way to do this, but this works

      function IntegerHash (value : Integer) return Ada.Containers.Hash_Type
      is (
         Ada.Containers.Hash_Type (abs (value))
      );

      package IntSets is new Ada.Containers.Hashed_Sets (
         Element_Type => Integer,
         Hash => IntegerHash,
         Equivalent_Elements => "="
      );
      Frequencies : IntSets.Set;
      --  we store each computed frequency here;
      --  when one repeats, we will see it in the set

      Frequency : Integer := 0;

   begin
      while True loop
         for Drift of Input loop
            Frequency := @ + Drift;
            if not Frequencies.Contains (Frequency) then
               Frequencies.Insert (Frequency);
            else
               ATIO.Put_Line (
                  "there are" & Frequencies.Length'Image
                  & " distinct frequencies"
               );
               return Frequency;
            end if;
         end loop;
      end loop;
      raise Should_Not_Happen;
   end Part2;

begin
   Read_Input;
   ATIO.Put_Line ("The frequency drift is" & Part1'Image);
   ATIO.Put_Line ("The first repeated frequency is" & Part2'Image);
end Day1;
