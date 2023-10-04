--  Advent of Code 2018
--
--  John Perry
--
--  Day 5: Alchemical Reduction
--
--  part 1: determine the length of a reduced chain of polymers
--
--  part 2: determine the smallest possible length when
--          when you remove every instance of one unit of the original chain
--          and then reduce it

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Vectors;

procedure Day5 is

   --  SECTION
   --  global types and variables

   --  SUBSECTION
   --  the Polymer type

   type Valid_Polymer is new Character range 'a' .. 'z';

   type Polymer (Upper_Case : Boolean) is record
      Value : Valid_Polymer;
   end record;

   function Is_Upper (C : Character) return Boolean
      renames Ada.Characters.Handling.Is_Upper;
   function To_Lower (C : Character) return Character
      renames Ada.Characters.Handling.To_Lower;

   function Polymer_From (C : Character) return Polymer is
   (
      Polymer'(
         Upper_Case => Is_Upper (C),
         Value => Valid_Polymer (To_Lower (C))
      )
   );

   --  SUBSECTION
   --  chains and reductions

   package Polymer_Vectors is new Ada.Containers.Indefinite_Vectors (
      Index_Type => Positive,
      Element_Type => Polymer
   );
   Polymers : Polymer_Vectors.Vector;

   function Cancel (Left, Right : Polymer) return Boolean is
   --  returns True iff Left and Right cancel
   (Left.Value = Right.Value and then Left.Upper_Case /= Right.Upper_Case);

   procedure Extend_Chain (
      Chain : in out Polymer_Vectors.Vector;
      P : Polymer
   )
   --  extends Chain by P, unless P cancels Chain's last element
   is

      use type Ada.Containers.Count_Type;

      Extended : constant Boolean := Chain.Length = 0 or else
         not Cancel (P, Chain.Last_Element);

   begin

      if Extended then
         Chain.Append (P);
      else
         Chain.Delete_Last;
      end if;

   end Extend_Chain;

   --  SECTION
   --  I/O and Part 1

   package Text_IO renames Ada.Text_IO;

   procedure Read_Input is
      F : Text_IO.File_Type;
      C : Character;
   begin
      Text_IO.Open (F, Text_IO.In_File, "input.txt");
      while not Text_IO.End_Of_File (F) loop
         Text_IO.Get_Immediate (F, C);
         declare
            P : constant Polymer := Polymer_From (C);
         begin
            Extend_Chain (Polymers, P);
         end;
      end loop;
   end Read_Input;

   --  SECTION
   --  Part 2

   function Improved_Chain (
      P_Old : Polymer_Vectors.Vector;
      C : Valid_Polymer
   )
      return Ada.Containers.Count_Type
   is

      Result : Polymer_Vectors.Vector;

   begin

      for P of P_Old loop
         if P.Value /= C then
            Extend_Chain (Result, P);
         end if;
      end loop;

      return Result.Length;

   end Improved_Chain;

   function Best_Chain return Natural is

      Shortest_Length : Natural := Natural (Polymers.Length);

   begin

      for C in Valid_Polymer loop
         Shortest_Length := Natural'Min (
            Shortest_Length,
            Natural (Improved_Chain (Polymers, C))
         );
      end loop;

      return Shortest_Length;

   end Best_Chain;

begin
   Read_Input;
   Text_IO.Put_Line ("After reading," & Polymers.Length'Image & " remain");
   Text_IO.Put_Line ("After improvement," & Best_Chain'Image & " remain");
end Day5;
