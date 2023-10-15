--  Advent of Code 2018
--
--  John Perry
--
--  Day 3: No Matter How You Slice It
--
--  part 1: determine how many square inches of fabric share the most claims
--
--  part 2: determine the claim that shares no fabric with any other

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

procedure Day3 is

   --  SECTION
   --  global types and variables

   type Claim is record
      Left, Top     : Natural;
      Width, Height : Positive;
   end record;

   package Claim_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Claim
   );

   Claims : Claim_Vectors.Vector;
   Max_X, Max_Y : Natural := 0;

   --  SECTION
   --  I/O

   function Find (S : String; C : Character) return Natural is
   --  find the first instance of C in S
   --  returns S'Last + 1 if it doesn't find it

      Result : Natural := S'First;

   begin

      while Result <= S'Last and then S (Result) /= C loop
         Result := @ + 1;
      end loop;

      return Result;

   end Find;

   function Read_Nat (S : String; Idx : in out Natural) return Natural is
   --  reads a natural number from S without choking on a colon (:)

      Result : Natural := 0;

   begin

      while Idx < S'Last and then S (Idx) in '0' .. '9' loop
         Result := @ * 10 + Natural'Value (S (Idx .. Idx));
         Idx := @ + 1;
      end loop;

      Idx := @ - 1;
      return Result;

   end Read_Nat;

   package ATIO renames Ada.Text_IO;
   package Natural_IO is new ATIO.Integer_IO (Num => Natural);

   procedure Read_Input is

      F : ATIO.File_Type;

   begin

      ATIO.Open (F, ATIO.In_File, "input.txt");

      while not ATIO.End_Of_File (F) loop

         declare
            S : constant String := ATIO.Get_Line (F);
            Idx : Natural := Find (S, '@');
            C : Claim;
         begin

            Idx := @ + 2;
            Natural_IO.Get (S (Idx .. S'Last), C.Left, Idx);

            Idx := @ + 2;
            C.Top := Read_Nat (S, Idx);

            Idx := @ + 3;
            Natural_IO.Get (S (Idx .. S'Last), C.Width, Idx);

            Idx := @ + 2;
            Natural_IO.Get (S (Idx .. S'Last), C.Height, Idx);

            Max_X := Natural'Max (Max_X, C.Left + C.Width);
            Max_Y := Natural'Max (Max_Y, C.Top + C.Height);

            Claims.Append (C);

         end;

      end loop;

      ATIO.Close (F);

   end Read_Input;

   --  SECTION
   --  Parts 1 and 2

   --  SUBSECTION
   --  Part 1
   --  we use a hash map from fabric position to number of claims

   type Position is record
      X, Y : Natural;
   end record;

   function Position_Hash (Value : Position) return Ada.Containers.Hash_Type is
   (Ada.Containers.Hash_Type (Value.X * Value.Y));
   --  choice of hash can make a BIG difference

   package Nat_Maps is new Ada.Containers.Hashed_Maps (
      Key_Type => Position,
      Element_Type => Natural,
      Hash => Position_Hash,
      Equivalent_Keys => "="
   );

   Claim_Maps : Nat_Maps.Map;

   function Part_1 return Natural is

      Inserted : Boolean;         --  whether an insertion worked
      Location : Nat_Maps.Cursor; --  where position was inserted,
                                  --     or would have been
      Result   : Natural := 0;    --  number of positions sharing a claim

   begin

      --  loop through claims
      for C of Claims loop

         --  loop through positions
         for X in C.Left .. C.Left + C.Width - 1 loop
            for Y in C.Top .. C.Top + C.Height - 1 loop

               Nat_Maps.Insert (
                  Claim_Maps,
                  Position'(X, Y),
                  1,
                  Location,
                  Inserted
               );
               if not Inserted then
                  Claim_Maps.Replace_Element (
                     Location,
                     Nat_Maps.Element (Location) + 1
                  );
               end if;

            end loop; -- Y
         end loop; -- X

      end loop; -- Claims

      for Value of Claim_Maps loop
         if Value > 1 then
            Result := @ + 1;
         end if;
      end loop;

      return Result;

   end Part_1;

   --  SUBSECTION
   --  Part 2

   Should_Not_Happen : exception;

   function Part_2 return Positive is
   begin

      for Idx in Claims.First_Index .. Claims.Last_Index loop

         declare
            C : constant Claim := Claims (Idx);
            Still_A_Candidate : Boolean := True;
         begin

            for X in C.Left .. C.Left + C.Width - 1 when Still_A_Candidate
            loop
               for Y in C.Top .. C.Top + C.Height - 1 when Still_A_Candidate
               loop

                  if Claim_Maps (Position'(X, Y)) > 1 then
                     Still_A_Candidate := False;
                  end if;

               end loop;
            end loop;

            if Still_A_Candidate then
               return Idx;
            end if;

         end;
      end loop;

      raise Should_Not_Happen;

   end Part_2;

begin
   Read_Input;
   ATIO.Put_Line (
      "There are" & Part_1'Image
      & " square inches with multiple claims"
   );
   ATIO.Put_Line (
      "Claim" & Part_2'Image & " doesn't overlap"
   );
end Day3;
