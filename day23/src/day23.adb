--  Advent of Code 2018
--
--  John Perry
--
--  Day 23: Experimental Emergency Teleportation
--
--  part 1: identify the nanobot with the most nanobots in range
--
--  part 2: identify the position with the most nanobots in range
--          that is closest to the origin

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

procedure Day23 is

   package IO renames Ada.Text_IO;
   package Int_IO is new IO.Integer_IO (Num => Integer);

   Doing_Example : constant Boolean := False;
   --  not really helpful, but i did try it

   --  SECTION
   --  Global types and variables

   --  SUBSECTION
   --  Position and distance

   type Position is record
      X, Y, Z : Integer;
   end record;

   function Distance (One, Tother : Position) return Natural is
   (
      abs (One.X - Tother.X) + abs (One.Y - Tother.Y) + abs (One.Z - Tother.Z)
   );

   --  SUBSECTION
   --  nanobot

   type Nanobot is record
      Pos : Position;
      Radius : Positive;
   end record;

   function In_Range (Source, Target : Nanobot) return Boolean is
   (
      Distance (Source.Pos, Target.Pos) <= Source.Radius
   );

   package Nano_Vectors is new Ada.Containers.Vectors (
      Index_Type => Positive, Element_Type => Nanobot
   );

   All_Nanobots : Nano_Vectors.Vector;

   Min_X, Min_Y, Min_Z : Integer := Integer'Last;
   Max_X, Max_Y, Max_Z : Integer := Integer'First;

   --  SECTION
   --  I/O

   procedure Read_Input is

      F : IO.File_Type;
      Filename : constant String
         := (if Doing_Example then "example.txt" else "input.txt");

   begin

      IO.Open (F, IO.In_File, Filename);

      while not IO.End_Of_File (F) loop

         declare
            New_Bot : Nanobot;
            Pos : Positive := 6;
            S : constant String := IO.Get_Line (F);
         begin

            Int_IO.Get (S (Pos .. S'Last), New_Bot.Pos.X, Pos);
            Pos := Pos + 2;
            Int_IO.Get (S (Pos .. S'Last), New_Bot.Pos.Y, Pos);
            Pos := Pos + 2;
            Int_IO.Get (S (Pos .. S'Last), New_Bot.Pos.Z, Pos);
            Pos := Pos + 6;
            Int_IO.Get (S (Pos .. S'Last), New_Bot.Radius, Pos);
            All_Nanobots.Append (New_Bot);

            Min_X := Integer'Min (Min_X, New_Bot.Pos.X);
            Max_X := Integer'Max (Max_X, New_Bot.Pos.X);
            Min_Y := Integer'Min (Min_Y, New_Bot.Pos.Y);
            Max_Y := Integer'Max (Max_Y, New_Bot.Pos.Y);
            Min_Z := Integer'Min (Min_Z, New_Bot.Pos.Z);
            Max_Z := Integer'Max (Max_Z, New_Bot.Pos.Z);

         end;

      end loop;

   end Read_Input;

   procedure Put_Nanobot (N : Nanobot) is
   --  useful for debugging
   begin
      IO.Put ('(');
      Int_IO.Put (N.Pos.X, 0);
      IO.Put (", ");
      Int_IO.Put (N.Pos.Y, 0);
      IO.Put (", ");
      Int_IO.Put (N.Pos.Z, 0);
      IO.Put (", ");
      Int_IO.Put (N.Radius, 0);
      IO.Put (")");
   end Put_Nanobot;

   --  SECTION
   --  Part 1

   function Nanobots_In_Range_Of (Source : Nanobot) return Natural is
      Result : Natural := 0;
   begin

      for N of All_Nanobots loop
         if In_Range (Source, N) then
            Result := Result + 1;
         end if;
      end loop;

      return Result;

   end Nanobots_In_Range_Of;

   function Nanobots_In_Range_Of_Strongest return Natural is
      Strongest : Nanobot := ((0, 0, 0), 1);
   begin

      for N of All_Nanobots loop
         if N.Radius > Strongest.Radius then
            Strongest := N;
         end if;
      end loop;

      IO.Put ("Strongest nanobot: ");
      Put_Nanobot (Strongest);
      IO.New_Line;

      return Nanobots_In_Range_Of (Strongest);

   end Nanobots_In_Range_Of_Strongest;

   --  SECTION
   --  Part 2

   --  SUBSECTION
   --  keeping track of prisms we've already tried

   type Dist_And_Count (Valid : Boolean := False) is record
      case Valid is
      when True =>
         Dist : Natural;
         Count : Natural;
      when False => null;
      end case;
   end record;

   function Natural_Hash (Value : Natural) return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type (Value));

   package Tried_Maps is new Ada.Containers.Hashed_Maps (
      Key_Type => Natural,
      Element_Type => Dist_And_Count,
      Hash => Natural_Hash,
      Equivalent_Keys => "="
   );

   Tried : Tried_Maps.Map;

   --  SUBSECTION
   --  State is
   --  * Position, the center of the region
   --  * Count, the number of nanobots within the region
   --  * Dist, distance of the region from the origin

   type State is record
      Pos : Position;
      Count : Natural;
      Dist : Natural;
   end record;

   package State_Vecs is new Ada.Containers.Vectors (
      Index_Type => Positive,
      Element_Type => State
   );

   --  SUBSECTION
   --  algorithm, part 1: recursive search for regions with the most nanobots

   type Int_Array is array (1 .. 2) of Integer;
   --  used to keep track of values

   function Find (
      X_Vals, Y_Vals, Z_Vals : Int_Array; --  values to iterate through
      Resolution : Natural;               --  how fine the iteration mesh
      Threshold : Integer                 --  ???
   ) return Dist_And_Count
   is
   --  * steps through cubes at given resolution and counts nanobots in range
   --    (includes a fudge factor for bots whose radius intersects
   --    but doesn't include cube center)
   --  * for the boxes that meet the requested threshold,
   --    find the best one that, on recursion to all higher resolutions
   --    (consequently halving the cube's size)
   --    still produces the desired threshold

      Dist : Integer;
      Bots_In_Range : Natural;

      At_Target : State_Vecs.Vector;   --  states that look favorable

      --  related to positions
      Pos : Position;
      X, Y, Z : Integer;
      Min_X : constant Integer := X_Vals (1);
      Min_Y : constant Integer := Y_Vals (1);
      Min_Z : constant Integer := Z_Vals (1);
      Max_X : constant Integer := X_Vals (2);
      Max_Y : constant Integer := Y_Vals (2);
      Max_Z : constant Integer := Z_Vals (2);

   begin

      --  step through cubes

      X := Min_X;       --  Ada doesn't offer stepped loops
      Loop_X : loop     --  (e.g., "for X in 1 .. 10 by 2")
         Y := Min_Y;    --  so we have to hack them
         Loop_Y : loop
            Z := Min_Z;
            Loop_Z : loop

               Pos := (X, Y, Z);

               --  count nanobots in range
               Bots_In_Range := 0;

               for Bot of All_Nanobots loop

                  if Resolution = 1 then
                     if Distance (Pos, Bot.Pos) <= Bot.Radius then
                        Bots_In_Range := @ + 1;
                     end if;

                  else

                     --  simplification: original added an offset,
                     --  but this offset clearly canceled, so I removed it
                     Dist := Distance (Pos, Bot.Pos);

                     --  "- 3" is load-bearing!!!
                     --  "- 2" also works on my input, but not "- 1"
                     --  original cites that this is to include current cube
                     --  in bots near it; I think this refers to bots
                     --  that may intersect box but don't include the center
                     if Dist / Resolution - 3 <= Bot.Radius / Resolution then
                        Bots_In_Range := @ + 1;
                     end if;

                  end if;

               end loop;

               if Bots_In_Range >= Threshold then
                  At_Target.Append (State'(
                     Pos => Pos,
                     Count => Bots_In_Range,
                     Dist => abs (X) + abs (Y) + abs (Z)
                  ));
               end if;

               Z := Z + Resolution;          --  ugh
               exit Loop_Z when Z > Max_Z;   --  ugly
            end loop Loop_Z;                 --  oogla
            Y := Y + Resolution;
            exit Loop_Y when Y > Max_Y;
         end loop Loop_Y;
         X := X + Resolution;
         exit Loop_X when X > Max_X;
      end loop Loop_X;

      --  now identify the best 
      while Natural (At_Target.Length) > 0 loop

         declare
            Best : State;
            Best_I : Positive;
            Matches : Dist_And_Count;
            Have_Best : Boolean := False;
         begin

            for Idx in 1 .. At_Target.Last_Index loop
               if not Have_Best or else At_Target (Idx).Dist < Best.Dist then
                  Best := At_Target (Idx);
                  Best_I := Idx;
                  Have_Best := True;
               end if;
            end loop;

            if Resolution = 1 then
               --  base "winning" case
               return (Valid => True, Dist => Best.Dist, Count => Best.Count);

            else

               --  recurse
               Matches := Find (
                  [Best.Pos.X, Best.Pos.X + Resolution / 2],
                  [Best.Pos.Y, Best.Pos.Y + Resolution / 2],
                  [Best.Pos.Z, Best.Pos.Z + Resolution / 2],
                  Resolution / 2,
                  Threshold
               );

               if not Matches.Valid then
                  At_Target.Delete (Best_I);
               else
                  return Matches;
               end if;

            end if;

         end;

      end loop;

      return (Valid => False);

   end Find;

   --  SUBSECTION
   --  algorithm, part 2

   function Best_Points_Distance return Natural is
   --  iterates, based on nanobot resolution and threshold
   --  every time it matches desired threshold at given resolution,
   --  it reduces resolution and increases threshold by new resolution
   --  * initially, span is the number of nanobots, threshold is 1
   --  * when it finds a subregion with more than the threshold,
   --    it raises threshold by span, then tries again -- unless span is 1,
   --    in which case it quits
   --  * when it fails to find such a subregion,
   --    it halves span and subtracts that amount from the threshold
   --
   --  in my case, the dance goes like this:
   --   iteration   resolution   threshold   outcome
   --       1          1000           1      success
   --       2          1000        1001      failure
   --       3           500         501      success
   --       4           500        1001      failure
   --       5           250         751      success
   --       6           250        1001      failure
   --       7           125         876      success
   --       8           125        1001      failure
   --       9            62         939      success
   --      10            62        1001      failure
   --      11            31         970      failure
   --      12            15         955      success
   --      13            15         970      failure
   --      14             7         963      success*
   --      15             7         970      failure
   --      16             3         967      failure
   --      17             1         966      failure
   --      18             1         965      failure
   --      19             1         964      success*
   -- it finds the correct result twice in this process:
   -- the first time on iteration 14, the second on iteration 19
   -- of the two, only iteration 19 has resolution 1, so it terminates then

      X_Vals : constant Int_Array := [ Min_X, Max_X ];
      Y_Vals : constant Int_Array := [ Min_Y, Max_Y ];
      Z_Vals : constant Int_Array := [ Min_Z, Max_Z ];

      Max_Resolution : Natural := 1;
      --  smallest power of 2 larger with radius around origin
      --  that enclsoes all points

      Threshold : Integer := 1;
      Bot_Resolution : Positive := Positive (All_Nanobots.Length);

      Best, Test : Dist_And_Count;

   begin

      while Max_Resolution < Max_X - Min_X
         or else Max_Resolution < Max_Y - Min_Y
         or else Max_Resolution < Max_Z - Min_Z
      loop
         Max_Resolution := @ * 2;
      end loop;

      loop

         IO.Put_Line (
            "Nanobot resolution / threshold"
            & Bot_Resolution'Image & Threshold'Image
         );

         if not Tried.Contains (Threshold) then
            Tried.Insert (
               Threshold,
               Find (
                  X_Vals, Y_Vals, Z_Vals,
                  Max_Resolution,
                  Threshold
               )
            );
         end if;

         Test := Tried (Threshold);

         if Test.Valid then

            if not Best.Valid or else Test.Count > Best.Count then
               Best := Test;
            end if;
            exit when Bot_Resolution = 1;
            Threshold := @ + Bot_Resolution;

         else

            if Bot_Resolution > 1 then
               Bot_Resolution := @ / 2;
            end if;
            Threshold := Integer'Max (1, Threshold - Bot_Resolution);

         end if;

      end loop;

      IO.Put_Line ("The max count I found was:" & Best.Count'Image);

      return Best.Dist;

   end Best_Points_Distance;

begin
   Read_Input;
   IO.Put_Line (
      "In range of strongest:"
      & Nanobots_In_Range_Of_Strongest'Image
   );
   IO.Put_Line (
      "Best distance with most points:"
      & Best_Points_Distance'Image
   );
end Day23;
