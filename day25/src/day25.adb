--  Advent of Code 2018
--
--  John Perry
--
--  Day 25: Four-Dimensional Adventure
--
--  part 1: Build a portal through time to get the reindeer some hot chocolate
--          from those elves you saved from goblins back on day... whatever
--
--  part 2: the usual day 25 freebie

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

procedure Day25 is

   package IO renames Ada.Text_IO;

   Doing_Example : constant Natural := 0;

   --  SECTION
   --  Global types and variables

   subtype Dimension is Integer range -10 .. 12;

   package Dim_IO is new IO.Integer_IO (Num => Dimension);

   type Membership (Known : Boolean := False) is record
      case Known is
      when True => Constellation : Positive;
      when False => null;
      end case;
   end record;

   type Space (Has_Star : Boolean := False) is record
      case Has_Star is
      when True =>
         Constellation : Membership;
      when False => null;
      end case;
   end record;

   Universe : array (Dimension, Dimension, Dimension, Dimension) of Space;

   Next_Constellation : Positive := 1;

   --  SECTION
   --  I/O

   Bad_Example : exception;

   procedure Read_Input is
      F : IO.File_Type;
   begin
      IO.Open (
         F,
         IO.In_File,
         (
            case Doing_Example is
            when 0 => "input.txt",
            when 1 => "example1.txt",
            when 2 => "example2.txt",
            when 3 => "example3.txt",
            when 4 => "example4.txt",
            when others => raise Bad_Example
         )
      );
      while not IO.End_Of_File (F) loop
         declare
            S : constant String := IO.Get_Line (F);
            X, Y, Z, W : Dimension;
            Pos : Positive;
         begin
            Dim_IO.Get (S, X, Pos);
            Pos := @ + 2;
            Dim_IO.Get (S (Pos .. S'Length), Y, Pos);
            Pos := @ + 2;
            Dim_IO.Get (S (Pos .. S'Length), Z, Pos);
            Pos := @ + 2;
            Dim_IO.Get (S (Pos .. S'Length), W, Pos);
            Universe (X, Y, Z, W) := (
               Has_Star => True,
               Constellation => Membership'(Known => False)
            );
         end;
      end loop;
   end Read_Input;

   --  SECTION
   --  Part 1

   --  SUBSECTION
   --  breadth-first search support

   type Position is record
      X, Y, Z, W : Integer;
   end record;
   --  places we will explore, or have already

   package Queue_Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces
      (Element_Type => Position);

   package Queues is new Ada.Containers.Unbounded_Synchronized_Queues (
      Queue_Interfaces => Queue_Interfaces
   );

   Queue : Queues.Queue;

   function Distance (P, Q : Position) return Natural is (
      abs (P.X - Q.X) + abs (P.Y - Q.Y) + abs (P.Z - Q.Z) + abs (P.W - Q.W)
   );

   function "<" (Left, Right : Position) return Boolean is (
      Left.X < Right.X or else (
         Left.X = Right.X and then Left.Y < Right.Y
      ) or else (
         Left.X = Right.X and then Left.Y = Right.Y and then
         Left.Z < Right.Z
      ) or else (
         Left.X = Right.X and then Left.Y = Right.Y and then
         Left.Z = Right.Z and then Left.W < Right.W
      )
   );

   package Enqueued_Sets is new Ada.Containers.Ordered_Sets (
      Element_Type => Position
   );

   Enqueued : Enqueued_Sets.Set;

   Already_Visited : exception;

   procedure Queue_Up_Neighbors (P : Position) is
      Q : Position;
   begin
      for X in P.X - 3 .. P.X + 3  when X in Dimension loop
         for Y in P.Y - 3 .. P.Y + 3 when Y in Dimension loop
            for Z in P.Z - 3 .. P.Z + 3 when Z in Dimension loop
               for W in P.W - 3 .. P.W + 3 when W in Dimension loop
                  Q := (X, Y, Z, W);
                  if Distance (P, Q) <= 3 and then
                     Universe (X, Y, Z, W).Has_Star and then
                     not Enqueued.Contains (Q)
                  then
                     Queue.Enqueue (Q);
                     Enqueued.Include (Q);
                  end if;
               end loop;
            end loop;
         end loop;
      end loop;
   end Queue_Up_Neighbors;

   --  SUBSECTION
   --  constellation tracing

   procedure Complete_Constellation (P : Position) is
      O renames Universe (P.X, P.Y, P.Z, P.W);
      Q : Position;
   begin
      O.Constellation := (Known => True, Constellation => Next_Constellation);
      Next_Constellation := @ + 1;
      Enqueued.Clear;
      Enqueued.Insert (P);
      Queue_Up_Neighbors (P);
      while Natural (Queue.Current_Use) > 0 loop
         Queue.Dequeue (Q);
         declare
            U renames Universe (Q.X, Q.Y, Q.Z, Q.W);
         begin
            if U.Constellation.Known then
               raise Already_Visited with (
                  Q.X'Image & Q.Y'Image & Q.Z'Image & Q.W'Image
               );
            end if;
            U.Constellation := O.Constellation;
            Queue_Up_Neighbors (Q);
         end;
      end loop;
   end Complete_Constellation;

   procedure Part_1 is
   begin
      for X in Dimension loop
         for Y in Dimension loop
            for Z in Dimension loop
               for W in Dimension loop
                  declare
                     S : constant Space := Universe (X, Y, Z, W);
                  begin
                     if S.Has_Star and then not S.Constellation.Known then
                        Complete_Constellation ((X, Y, Z, W));
                     end if;
                  end;
               end loop;
            end loop;
         end loop;
      end loop;
   end Part_1;

begin
   Read_Input;
   Part_1;
   IO.Put_Line (
      "There are" & Positive'Image (Next_Constellation - 1) & " constellations"
   );
end Day25;
