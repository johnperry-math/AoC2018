--  Advent of Code 2018
--
--  John Perry
--
--  Day 2: Inventory Management System
--
--  part 1: determine which boxes have two repeated symbols
--          and which have three repeated symbos;
--          some boxes may have both
--
--  part 2: determine the two boxes with identical symbols
--          **but for one position**. report the other symbols

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day2 is

   package ATIO renames Ada.Text_IO;

   --  SECTION
   --  global types and variables

   subtype CharRange is Character range 'a' .. 'z';

   type Id is array (1 .. 26) of CharRange;
   type Id_Count is array (CharRange) of Natural;

   package Id_Vecs is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Id
   );
   Ids : Id_Vecs.Vector;

   package Id_Count_Vecs is new Ada.Containers.Vectors (
      Index_Type => Positive,
      Element_Type => Id_Count
   );
   Id_Counts : Id_Count_Vecs.Vector;

   --  SECTION
   --  I/O

   function String_To_Id (S : String) return Id is
   ([for I in Id'Range => S (I)]);

   procedure Read_Input is
      F : ATIO.File_Type;
   begin
      ATIO.Open (F, ATIO.In_File, "input.txt");
      while not ATIO.End_Of_File (F) loop
         declare
            Id_In : constant String := ATIO.Get_Line (F);
            Count : Id_Count := [others => 0];
         begin
            Ids.Append (String_To_Id (Id_In));
            for c of Id_In loop
               Count (c) := @ + 1;
            end loop;
            Id_Counts.Append (Count);
         end;
      end loop;
      ATIO.Close (F);
   end Read_Input;

   --  SECTION
   --  parts 1 and 2

   --  SUBSECTION
   --  part 1

   function Any_Twos (Id : Id_Count) return Boolean is (
      for some c in CharRange => Id (c) = 2
   );

   function Any_Threes (Id : Id_Count) return Boolean is (
      for some c in CharRange => Id (c) = 3
   );

   function Part_1 return Integer is
      Num_Twos, Num_Threes : Natural := 0;
   begin
      for Id of Id_Counts loop
         if Any_Twos (Id) then
            Num_Twos := @ + 1;
         end if;
         if Any_Threes (Id) then
            Num_Threes := @ + 1;
         end if;
      end loop;
      return Num_Twos * Num_Threes;
   end Part_1;

   --  SUBSECTION
   --  Part 2

   function Differ_By_One (First, Second : Id) return Boolean is
      Differences : Natural := 0;
   begin
      for I in Id'Range when Differences < 2 loop
         if First (I) /= Second (I) then
            Differences := @ + 1;
         end if;
      end loop;
      return Differences = 1;
   end Differ_By_One;

   type Id_Pair is record
      First : Id;
      Second : Id;
   end record;

   Should_Not_Be_Raised : exception;
   --  to silence a warning

   function Part_2 return Id_Pair is
   begin
      for I in Ids.First_Index .. Ids.Last_Index loop
         for J in I + 1 .. Ids.Last_Index loop
            if Differ_By_One (Ids (I), Ids (J)) then
               ATIO.Put_Line ("Common ID's at indices" & I'Image & J'Image);
               return (First => Ids (I), Second => Ids (J));
            end if;
         end loop;
      end loop;
      raise Should_Not_Be_Raised;
   end Part_2;

   type Common_Id is array (1 .. 25) of Character;

   function Common (Pair : Id_Pair) return Common_Id is
      Result : Common_Id := [others => ' '];
      J : Natural := Common_Id'First;
   begin
      for I in Id'Range loop
         if Pair.First (I) = Pair.Second (I) then
            Result (J) := Pair.First (I);
            J := @ + 1;
         end if;
      end loop;
      return Result;
   end Common;

begin
   Read_Input;
   ATIO.Put_Line (
      "The product of Id's with two repetitions "
      & " and of Id's with three repetitions is"
      & Part_1'Image
   );
   ATIO.Put_Line ("The common ID is " & String (Common (Part_2)));
end Day2;
