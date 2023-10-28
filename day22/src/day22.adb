--  Advent of Code 2018
--
--  John Perry
--
--  Day 22: Mode Maze
--
--  part 1: calculate the risk factor of traveling through a cavern
--          that consists of rocky, wet, or narrow regions
--
--  part 2: determine the quickest time to reach the target region

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Synchronized_Queue_Interfaces;

procedure Day22 is

   package IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   --  SECTION
   --  Global types and variables

   type Region is (Rocky, Narrow, Wet);

   --  SUBSECTION
   --  Position, and the basic map

   type Position is record
      X, Y : Natural;
   end record;

   Target : constant Position
      := (if Doing_Example then (10, 10) else (11, 722));

   function Position_Hash (P : Position) return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type (P.X * 2000 + P.Y));

   Map_Bound : constant Natural := 1_000;

   Map : array (0 .. Map_Bound, 0 .. Map_Bound) of Region;

   --  SUBSECTION
   --  Erosion and geological indices

   Erosion_Modulus : constant Natural := 20183;

   type Erosion is mod Erosion_Modulus;

   Depth : constant Erosion := (if Doing_Example then 510 else 10_689);

   package Erosion_Maps is new Ada.Containers.Hashed_Maps (
      Key_Type => Position,
      Element_Type => Erosion,
      Hash => Position_Hash,
      Equivalent_Keys => "="
   );

   Erosion_Levels : Erosion_Maps.Map;

   function Geological_Index_At (Pos : Position) return Erosion is
   (
      (
         if Erosion_Levels.Contains ((Pos.X, Pos.Y)) then
            Erosion_Levels ((Pos.X, Pos.Y))
         elsif Pos = (0, 0) or else Pos = Target then 0
         elsif Pos.Y = 0 then
            Erosion (Pos.X) * Erosion (16_807)
         elsif Pos.X = 0 then
            Erosion (Pos.Y) * Erosion (48_271 mod Erosion_Modulus)
         else
            Geological_Index_At ((Pos.X - 1, Pos.Y))
               * Geological_Index_At ((Pos.X, Pos.Y - 1))
      )
   );

   --  SECTION
   --  Part 1

   --  SUBSECTION
   --  Filling in the map

   Feature_Exception : exception;

   function Feature (L : Erosion) return Region is
   (
      case Natural (L) mod 3 is
      when 0 => Rocky,
      when 1 => Wet,
      when 2 => Narrow,
      when others => raise Feature_Exception with L'Image
   );

   procedure Fill_Map is
   begin

      IO.Put_Line ("Filling the map takes a moment...");

      for X in 0 .. Map_Bound loop
         for Y in 0 .. Map_Bound loop

            Erosion_Levels
               .Insert ((X, Y), Depth + Geological_Index_At ((X, Y)));
            Map (X, Y) := Feature (Erosion_Levels ((X, Y)));

         end loop;
      end loop;

      IO.Put_Line ("Done!");

   end Fill_Map;

   procedure Put_Map is
   --  useful for debugging Part 1
   begin

      for Y in 0 .. Target.Y loop
         for X in 0 .. Target.X loop

            case Map (X, Y) is
               when Rocky  => IO.Put ('.');
               when Wet    => IO.Put ('=');
               when Narrow => IO.Put ('|');
            end case;

         end loop;

         IO.New_Line;
      end loop;

   end Put_Map;

   --  SUBSECTION
   --  Determining risk levels

   function Risk_Level (F : Region) return Natural is
   (
      case F is
      when Rocky  => 0,
      when Wet    => 1,
      when Narrow => 2
   );

   function Risk_Level return Natural is
      Result : Natural := 0;
   begin
      for X in 0 .. Target.X loop
         for Y in 0 .. Target.Y loop
            Result := @ + Risk_Level (Map (X, Y));
         end loop;
      end loop;
      return Result;
   end Risk_Level;

   --  SECTION
   --  Part 2

   --  SUBSECTION
   --  Tools and their compatibility with regions

   type Tools is (Torch, Gear, Neither);

   function Compatible (Tool : Tools; Pos : Position) return Boolean is
   (
      case Map (Pos.X, Pos.Y) is
      when Rocky  => Tool = Torch or else Tool = Gear,
      when Wet    => Tool = Gear  or else Tool = Neither,
      when Narrow => Tool = Torch or else Tool = Neither
   );

   --  SUBSECTION
   --  BFS state

   type State is record
      Pos : Position;
      Equipment : Tools;
      Time_Spent : Natural;
   end record;

   function State_Hash (S : State) return Ada.Containers.Hash_Type is
   (
      Ada.Containers.Hash_Type (
         S.Pos.X * 2_000 + S.Pos.Y * 3 + (
            case S.Equipment is
            when Torch   => 2,
            when Gear    => 1,
            when Neither => 0
         )
      )
   );

   function Equal_Sans_Time (Left, Right : State) return Boolean is
   --  "sans time" is kind of important; using "=" meant
   --  that most of the positions I wanted to exclude were not excluded!
   (
      Left.Equipment = Right.Equipment and then
         Left.Pos = Right.Pos
   );

   package Position_Maps is new Ada.Containers.Hashed_Maps
   (
      Key_Type => State,
      Element_Type => Natural,
      Hash => State_Hash,
      Equivalent_Keys => Equal_Sans_Time
      --  Equivalent_Keys => "="    --  left for future humble pie
   );

   Positions_Explored : Position_Maps.Map;

   --  SUBSECTION
   --  BFS queues
   --  originally I had just one queue, but I was hitting a serious bottleneck
   --  and, not knowing the cause, wondered if it was because
   --  everything was in one queue
   --  to try and fix it, I created an array of queues
   --  originally I tried a vector, but Ada doesn't like vectors of queues,
   --  because queues are a limited type...
   --  IMHO someone really didn't think that through...

   package Position_Queues_Interfaces
      is new Ada.Containers.Synchronized_Queue_Interfaces
   (
      Element_Type => State
   );

   package Position_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
   (
      Queue_Interfaces => Position_Queues_Interfaces
   );

   To_Do : array (0 .. Map_Bound * 2) of Position_Queues.Queue;

   --  SUBSECTION
   --  BFS statistics

   Rejected : Natural := 0;
   Best_Time : Natural := Natural'Last;

   --  SUBSECTION
   --  BFS Exploration

   type Position_Delta is record
      X, Y : Integer;
   end record;

   Position_Deltas : constant array (1 .. 4) of Position_Delta
      := [ (0, -1), (1, 0), (0, 1), (-1, 0) ];

   function Worth_Exploring (S : State) return Boolean is
   (
      S.Pos.X <= Map_Bound and then S.Pos.Y <= Map_Bound and then
      (
         not Positions_Explored.Contains (S)
         or else Positions_Explored (S) > S.Time_Spent
      )
      and then S.Time_Spent < Best_Time
      and then
         S.Time_Spent + abs (S.Pos.X - Target.X) + abs (S.Pos.Y - Target.Y)
            < Best_Time
   );

   procedure Explore (S : State) is
      New_Position : Position;
      New_State : State;
   begin

      for D of Position_Deltas loop

         if Integer (S.Pos.X) + D.X >= 0
            and then Integer (S.Pos.Y) + D.Y >= 0
         then

            New_Position := (S.Pos.X + D.X, S.Pos.Y + D.Y);

            for Tool in Tools loop

               if Compatible (Tool, New_Position)
                  and then Compatible (Tool, S.Pos)
               then

                  New_State := (
                     Pos => New_Position,
                     Equipment => Tool,
                     Time_Spent => (
                        if Tool = S.Equipment then S.Time_Spent + 1
                        else S.Time_Spent + 8
                     )
                  );

                  if Worth_Exploring (New_State) then

                     To_Do (New_State.Time_Spent).Enqueue (New_State);
                     if Positions_Explored.Contains (New_State) then
                        Positions_Explored
                           .Replace (New_State, New_State.Time_Spent);
                     else
                        Positions_Explored
                           .Insert (New_State, New_State.Time_Spent);
                     end if;

                  else
                     Rejected := @ + 1;

                  end if;
               end if;

            end loop;

         end if;

      end loop;

   end Explore;

   --  SUBSECTION
   --  BFS control

   function Worth_Dequeuing (S : State) return Boolean is
   (
      (
         not Positions_Explored.Contains (S)
         or else Positions_Explored (S) >= S.Time_Spent
      )
      and then S.Time_Spent < Best_Time
      and then
         S.Time_Spent + abs (S.Pos.X - Target.X) + abs (S.Pos.Y - Target.Y)
            < Best_Time
   );

   function Quickest_Route return Natural is

      States_Explored : Natural := 0;
      Current_Time_Spent : Natural := 0;
      S : State := (Pos => (0, 0), Equipment => Torch, Time_Spent => 0);

   begin

      To_Do (0).Enqueue (S);
      Positions_Explored.Insert (S, 0);

      loop

         --  jump to next largest queue
         while Current_Time_Spent <= To_Do'Last
               and then Natural (To_Do (Current_Time_Spent).Current_Use) = 0
         loop
            Current_Time_Spent := @ + 1;
         end loop;

         exit when Current_Time_Spent > To_Do'Last;

         To_Do (Current_Time_Spent).Dequeue (S);
         States_Explored := @ + 1;

         --  a few stats
         --  if States_Explored mod 10_000 = 0
         --  then
         --     IO.Put_Line ("Current time is" & Current_Time_Spent'Image);
         --     IO.Put_Line (
         --        "Explored"
         --        & Positions_Explored.Length'Image & " states over"
         --        & States_Explored'Image & " iterations, rejecting"
         --        & Rejected'Image
         --     );
         --     IO.Put_Line (
         --        "Still have to explore"
         --        & To_Do (Current_Time_Spent).Current_Use'Image
         --     );
         --     IO.Put_Line ("Best time so far:" & Best_Time'Image);
         --     IO.New_Line (2);
         --  end if;

         if Worth_Dequeuing (S) then

            if S.Pos = Target then
               if S.Equipment = Torch then
                  Best_Time := Natural'Min (Best_Time, S.Time_Spent);
               else
                  Best_Time := Natural'Min (Best_Time, S.Time_Spent + 7);
               end if;
            else
               Explore (S);
            end if;

         end if;

      end loop;

      --  more stats
      --  IO.Put_Line (
      --     "Rejected" & Rejected'Image
      --     & " and Dequeued" & States_Explored'Image
      --  );

      return Best_Time;

   end Quickest_Route;

begin
   Fill_Map;
   if Doing_Example then
      Put_Map;
      IO.Put_Line (Region'Image (Map ( 0,  0)));
      IO.Put_Line (Region'Image (Map ( 1,  0)));
      IO.Put_Line (Region'Image (Map ( 0,  1)));
      IO.Put_Line (Region'Image (Map ( 1,  1)));
      IO.Put_Line (Region'Image (Map (10, 10)));
   end if;
   IO.Put_Line ("The total risk level is" & Risk_Level'Image);
   IO.Put_Line ("The quickest route requires" & Quickest_Route'Image);
end Day22;
