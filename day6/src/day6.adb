--  Advent of Code 2018
--
--  John Perry
--
--  Day 6: Chronal Coordinates
--
--  part 1: report the size of the largest finite area around a coordinate
--          whose points are closest to that coordinate
--
--  part 2: report the size of the area whose points
--          all have a sum of distances from the coordinates less than 10_000

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

procedure Day6 is

   Doing_Example : constant Boolean := False;
   --  I really needed the example here!

   package Text_IO renames Ada.Text_IO;
   package Integer_IO is new Text_IO.Integer_IO (Num => Natural);

   --  SECTION
   --  Global types and variables

   type Position is record
      Row, Col : Integer;
   end record;

   function Manhattan_Distance (Start, Stop : Position) return Natural is
      (abs (Start.Row - Stop.Row) + abs (Start.Col - Stop.Col));

   package Position_Vecs is new Ada.Containers.Vectors (
      Index_Type => Positive,
      Element_Type => Position
   );

   Locations : Position_Vecs.Vector;

   Min_Row, Min_Col : Natural := Natural'Last;
   Max_Row, Max_Col : Natural := Natural'First;

   type Claim is record
      Claimed : Boolean;
      Claimant : Natural;
      Distance : Natural;
   end record;
   --  records whether the Claimant (index to Locations) claims this position

   type Grid_Type is array (Natural range <>, Natural range <>) of Claim;

   --  SECTION
   --  I/O

   procedure Print_Grid (Grid : Grid_Type) is
   --  useful for debugging
   begin

      for Row in Grid'Range (1) loop
         for Col in Grid'Range (2) loop

            if Grid (Row, Col).Claimed then
               Integer_IO.Put (Grid (Row, Col).Claimant, 3);
            else
               Text_IO.Put ("  .");
            end if;

         end loop;

         Text_IO.New_Line;

      end loop;

   end Print_Grid;

   procedure Read_Input is
      F : Text_IO.File_Type;
   begin

      Text_IO.Open (F, Text_IO.In_File, (
         if Doing_Example then "example.txt" else "input.txt"
      ));

      while not Text_IO.End_Of_File (F) loop

         declare
            S : constant String := Text_IO.Get_Line (F);
            Last : Integer := S'First;
            Row, Col : Natural;
         begin

            Integer_IO.Get (S, Col, Last);
            Integer_IO.Get (S (Last + 3 .. S'Last), Row, Last);

            Locations.Append (Position'(Row, Col));
            Min_Row := Natural'Min (Row, Min_Row);
            Min_Col := Natural'Min (Col, Min_Col);
            Max_Row := Natural'Max (Row, Max_Row);
            Max_Col := Natural'Max (Col, Max_Col);

         end;

      end loop;

      Text_IO.Close (F);

   end Read_Input;

   --  SECTION
   --  Part 1, first attempt

   --  procedure Claim_Possible (
   --     Claimant : Natural;
   --     Grid : in out Grid_Type;
   --     Distance : Natural;
   --     Claimed_Spaces : in out Natural;
   --     Made_Claim : out Boolean
   --  )
   --  is
   --     type Direction_Type is record
   --        Offset_X, Offset_Y, Dx, Dy : Integer;
   --     end record;
   --     Directions : constant array (1 .. 4) of Direction_Type
   --        := [
   --              ( 0, -1, -1,  1), --  start north, move southwest
   --              (-1,  0,  1,  1), --  start west,  move southeast
   --              ( 0,  1,  1, -1), --  start south, move northeast
   --              ( 1,  0, -1, -1)  --  start east,  move northwest
   --        ];
   --  begin
   --     if Distance = 0 then
   --        Made_Claim := True;
   --        Claimed_Spaces := @ + 1;
   --        Grid (Locations (Claimant).X, Locations (Claimant).Y).Claimed := True;
   --        Grid (Locations (Claimant).X, Locations (Claimant).Y).Claimant
   --           := Claimant;
   --     else
   --        Made_Claim := False;
   --        for D of Directions loop
   --           declare
   --              P : Position := (
   --                 Locations (Claimant).X + D.Offset_X * Distance,
   --                 Locations (Claimant).Y + D.Offset_Y * Distance
   --              );
   --           begin
   --              for Each in 1 .. Distance loop
   --                 if P.X in Min_X .. Max_X and then P.Y in Min_Y .. Max_Y then
   --                    if not Grid (P.X, P.Y).Claimed then
   --                       Grid (P.X, P.Y).Claimed := True;
   --                       Grid (P.X, P.Y).Claimant := Claimant;
   --                       Grid (P.X, P.Y).Distance := Distance;
   --                       Made_Claim := True;
   --                       Claimed_Spaces := @ + 1;
   --                    elsif Grid (P.X, P.Y).Distance = Distance then
   --                       Grid (P.X, P.Y).Claimant := 0;
   --                    end if;
   --                 end if;
   --                 P.X := @ + D.Dx;
   --                 P.Y := @ + D.Dy;
   --              end loop;
   --           end;
   --        end loop;
   --     end if;
   --  end Claim_Possible;

   --  function Part_1 return Natural is
   --     Grid : Grid_Type (Min_X .. Max_X, Min_Y .. Max_Y) := [
   --        others => [
   --           others => (Claimed => False, Claimant => 0, Distance => 0)
   --        ]
   --     ];
   --     Distance : Natural := 0;
   --     Claimed_Spaces : Natural := 0;
   --     Still_Claiming : array (Locations.First_Index .. Locations.Last_Index)
   --        of Boolean := [others => True];
   --  begin

   --     --  first let each location stake its claims
   --     while Claimed_Spaces < (Max_X - Min_X + 1) * (Max_Y - Min_Y + 1) loop

   --        for Idx in Locations.First_Index .. Locations.Last_Index loop
   --           if Still_Claiming (Idx) then
   --              Claim_Possible (
   --                 Idx,
   --                 Grid,
   --                 Distance,
   --                 Claimed_Spaces,
   --                 Still_Claiming (Idx)
   --              );
   --           end if;
   --        end loop;

   --        Distance := @ + 1;
   --        Text_IO.Put_Line (
   --           "Looking at distance" & Distance'Image
   --           & " claimed" & Claimed_Spaces'Image
   --        );
   --        Print_Grid (Grid);

   --     end loop;

   --     -- remove border elements from claimants
   --     for Idx in Locations.First_Index .. Locations.Last_Index loop
   --        declare
   --           P : constant Position := Locations (Idx);
   --        begin
   --           if P.X = Min_X or else P.X = Max_X
   --              or else P.Y = Min_Y or else P.Y = Max_Y
   --           then
   --              Still_Claiming (Idx) := True;
   --           end if;
   --        end;
   --     end loop;

   --     --  determine which location has the largest claim
   --     declare
   --        Claimed_Area : array (Locations.First_Index .. Locations.Last_Index)
   --           of Natural := [others => 0];
   --        Claimant : Natural;
   --     begin

   --        for P of Grid loop
   --           Claimant := P.Claimant;
   --           if Claimant /= 0 and then not Still_Claiming (Claimant) then
   --              Claimed_Area (Claimant) := @ + 1;
   --           end if;
   --        end loop;

   --        Text_IO.Put_Line (Claimed_Area'Image);

   --        return Claimed_Area'Reduce (Natural'Max, Natural'First);

   --     end;

   --  end Part_1;

   --  SECTION
   --  Part 1, second attempt

   function Part_1 return Natural is

      Grid : Grid_Type (Min_Row .. Max_Row, Min_Col .. Max_Col) := [
         others => [
            others => (Claimed => False, Claimant => 0, Distance => 0)
         ]
      ];

      Still_Claiming : array (Locations.First_Index .. Locations.Last_Index)
         of Boolean := [others => False];

   begin

      --  at each point, determine the minimum distance to a location
      --  this tells you which location gets the point in its area
      for Row in Grid'Range (1) loop
         for Col in Grid'Range (2) loop

            declare
               Min : Natural := Natural'Last;
               Claimed : Boolean := False;
               Claimant : Natural := 0;
            begin

               --  loop through Locations to find closest
               for Idx in Locations.First_Index .. Locations.Last_Index loop

                  declare
                     P : constant Position := Locations (Idx);
                     Distance : constant Natural
                        := Manhattan_Distance (P, Position'(Row, Col));
                  begin

                     if Distance < Min then
                        Claimed := True;
                        Claimant := Idx;
                        Min := Distance;
                     elsif Distance = Min then
                        Claimed := False;
                     end if;

                  end;

               end loop;

               Grid (Row, Col) := (Claimed, Claimant, Min);

               --  if we're on the edge then our claimant
               --  will still be claiming after it's done with us
               if Claimed
                  and then (
                     Row = Grid'First (1) or else Row = Grid'Last (1)
                     or else Col = Grid'First (2) or else Col = Grid'Last (2)
                  )
               then
                  Still_Claiming (Claimant) := True;
               end if;

            end;

         end loop;
      end loop;

      --  Print_Grid (Grid);

      --  determine the finite areas
      declare
         Claimed_Areas : array (Locations.First_Index .. Locations.Last_Index)
            of Natural := [others => 0];
      begin

         for P of Grid loop
            if P.Claimant /= 0 and then P.Claimed = True
               and then not Still_Claiming (P.Claimant)
            then
               Claimed_Areas (P.Claimant) := @ + 1;
            end if;
         end loop;

         return Claimed_Areas'Reduce (Natural'Max, 0);

      end;

   end Part_1;

   --  SECTION
   --  Part 2

   function Part_2 return Natural is

      --  it takes entirely too much trouble to set up
      --  a basic breadth-first search in Ada
      use type Ada.Containers.Count_Type;

      package Queue_Interfaces is
         new Ada.Containers.Synchronized_Queue_Interfaces
         (Element_Type => Position);

      package Queues is new Ada.Containers.Unbounded_Synchronized_Queues
         (Queue_Interfaces => Queue_Interfaces);

      function Position_Hash (Element : Position)
         return Ada.Containers.Hash_Type
      is
         (Ada.Containers.Hash_Type (Element.Row * Element.Col));

      package Position_Sets is new Ada.Containers.Hashed_Sets (
         Element_Type => Position,
         Hash => Position_Hash,
         Equivalent_Elements => "="
      );

      Queue : Queues.Queue;
      Explored : Position_Sets.Set;

      function All_Distances (P : Position) return Natural is
      --  returns the sum of distances from this position to all coordinates
         Result : Natural := 0;
      begin
         for Q of Locations loop
            Result := @ + Manhattan_Distance (P, Q);
         end loop;
         return Result;
      end All_Distances;

      Limit : constant Natural := (if Doing_Example then 32 else 10_000);

      Result : Natural := 0;

   begin

      --  start with the given locations
      for P of Locations loop
         Queue.Enqueue (P);
      end loop;

      --  BFS baby!
      while Queue.Current_Use > 0 loop

         declare
            P : Position;
         begin

            Queue.Dequeue (P);

            if not Explored.Contains (P) then

               Explored.Insert (P);

               if All_Distances (P) < Limit then
                  Result := Result + 1;
                  Queue.Enqueue ((P.Row - 1, P.Col));
                  Queue.Enqueue ((P.Row + 1, P.Col));
                  Queue.Enqueue ((P.Row, P.Col - 1));
                  Queue.Enqueue ((P.Row, P.Col + 1));
               end if;

            end if;

         end;

      end loop;

      return Result;

   end Part_2;

begin
   Read_Input;
   Text_IO.Put_Line ("The largest claimed area is" & Part_1'Image);
   Text_IO.Put_Line ("The safe area is" & Part_2'Image);
end Day6;
