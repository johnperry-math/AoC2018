--  Advent of Code 2018
--
--  John Perry
--
--  Day 15: Beverage Bandits
--
--  part 1: what is the outcome in a battle where elves are outnumbered
--          by thieving orcs?
--
--  part 2: what is the outcome from the smallest increase
--          in elf attack strength that keeps all the elves alive?

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

procedure Day15 is

   package IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;
   Which_Example : constant Positive := 3;

   --  SECTION
   --  Global types and variables

   --  SUBSECTION
   --  Positions and locations

   subtype Position is Natural range 1 .. (if Doing_Example then 7 else 32);

   type Location is record
      Row, Col : Position;
   end record;

   package Location_Vecs is new Ada.Containers.Vectors
      (Index_Type => Positive, Element_Type => Location);

   --  SUBSECTION
   --  Elves and goblins

   type Critter is (Elf, Goblin);

   type Warrior (Kind : Critter := Elf) is record
      Row, Col : Position;
      Hit_Points : Natural;
   end record;

   package Warrior_Vecs is new Ada.Containers.Vectors
      (Index_Type => Positive, Element_Type => Warrior);

   Critters : Warrior_Vecs.Vector;

   --  SUBSECTION
   --  Battlefield

   type Filler is (Being, Empty, Wall);

   type Cell (Contents : Filler := Wall) is record
      case Contents is
         when Being => Critter_Idx : Positive;
         when others => null;
      end case;
   end record;

   type Map_Array is array (Position, Position) of Cell;

   Map : Map_Array;

   --  SECTION
   --  I/O

   Bad_Input : exception;

   function New_Cell (C : Character) return Cell is (
      --  creates a new Cell based on the value of C

      case C is

      when '#' =>
         Cell'(Contents => Wall),

      when '.' =>
         Cell'(Contents => Empty),

      when 'G' | 'E' => Cell'(
         Contents => Being,
         Critter_Idx => Natural (Critters.Length) + 1
      ),

      when others => raise Bad_Input

   );

   procedure Read_Input is
      F : IO.File_Type;
   begin

      IO.Open (
         F,
         IO.In_File,
         (if Doing_Example then (
            if Which_Example = 1 then "example1.txt"
            elsif Which_Example = 2 then "example2.txt"
            elsif Which_Example = 3 then "example3.txt"
            else "example4.txt"
         ) else "input.txt"
         )
      );

      for Row in Position loop

         declare
            S : constant String := IO.Get_Line (F);
         begin

            for Col in Position loop

               Map (Row, Col) := New_Cell (S (Col));

               if S (Col) = 'E' then
                  Critters.Append (
                     Warrior'(
                        Kind => Elf,
                        Row => Row,
                        Col => Col,
                        Hit_Points => 200
                     )
                  );

               elsif S (Col) = 'G' then
                  Critters.Append (
                     Warrior'(
                        Kind => Goblin,
                        Row => Row,
                        Col => Col,
                        Hit_Points => 200
                     )
                  );
               end if;

            end loop;

         end;

      end loop;

   end Read_Input;

   procedure Put_Map is
   begin

      for Row in Position loop
         for Col in Position loop

            case Map (Row, Col).Contents is

            when Wall => IO.Put ('#');
            when Empty => IO.Put ('.');

            when Being =>
               declare
                  C : constant Warrior
                     := Critters (Map (Row, Col).Critter_Idx);
               begin
                  if C.Hit_Points > 0 then
                     case C.Kind is
                     when Elf => IO.Put ('E');
                     when Goblin => IO.Put ('G');
                     end case;
                  else
                     IO.Put ('X');
                  end if;
               end;

            end case;

         end loop;

         IO.New_Line;

      end loop;

   end Put_Map;

   --  SECTION
   --  Parts 1 and 2

   --  SUBSECTION
   --  Lexicographic orderings of Warrior and Location

   function "<" (Left, Right : Warrior) return Boolean is
      (
         Left.Row < Right.Row or else (
            Left.Row = Right.Row and then Left.Col < Right.Col
         )
      );

   function "<" (First, Second : Location) return Boolean is
      (
         First.Row < Second.Row
         or else
         (First.Row = Second.Row and then First.Col < Second.Col)
      );

   package Warrior_Sorter is new Warrior_Vecs.Generic_Sorting;

   --  SUBSECTION
   --  Part 1: Orienting oneself

   type Directions is (Up, Left, Down, Right);

   type Offset is record
      Drow, Dcol : Integer;
   end record;

   Offsets : constant array (Directions) of Offset := [
      Up => (Drow => -1, Dcol => 0),
      Down => (Drow => 1, Dcol => 0),
      Left => (Drow => 0, Dcol => -1),
      Right => (Drow => 0, Dcol => 1)
   ];

   --  SUBSECTION
   --  Part 1: Attacking, if possible

   Elf_Attack : Natural := 3;
   Goblin_Attack : constant Natural := 3;

   procedure Attack (To : Location) is
   --  Removes the appropriate number of hit points from the critter at To,
   --  and if the critter dies it removes it from the map

      Debug : constant Boolean := False;
      C : Warrior renames Critters (Map (To.Row, To.Col).Critter_Idx);

   begin

      if Debug then
         IO.Put_Line (
            "   attacks" & To.Row'Image & To.Col'Image
            & " (" & C.Hit_Points'Image & ")"
         );
      end if;

      C.Hit_Points := Integer'Max (
         0,
         Integer (@) - (if C.Kind = Goblin then Elf_Attack else Goblin_Attack)
      );
      if C.Hit_Points = 0 then
         if Debug then
            IO.Put_Line (
               "   critter @" & To.Row'Image & To.Col'Image & " dies!"
            );
         end if;
         Map (To.Row, To.Col) := Cell'(Contents => Empty);
      end if;

   end Attack;

   type Target (Exists : Boolean := False) is record

      case Exists is
      when True =>
         Loc : Location;
         Hit_Points : Natural;
      when False => null;
      end case;

   end record;

   function Find_Target_From (Here : Location) return Target is
   --  Checks adjacent cells to see if a target is present

      Result : Target;
      Debug : constant Boolean := False;
      Self : constant Critter
         := Critters (Map (Here.Row, Here.Col).Critter_Idx).Kind;

   begin

      for O of Offsets loop

         declare
            There : constant Location
               := (Row => Here.Row + O.Drow, Col => Here.Col + O.Dcol);
            Place : constant Cell := Map (There.Row, There.Col);
         begin

            if Place.Contents = Being
               and then Critters (Place.Critter_Idx).Kind /= Self
               and then Critters (Place.Critter_Idx).Hit_Points > 0
            then

               if Debug then
                  IO.Put_Line (
                     "   could attack" & There.Row'Image & There.Col'Image
                     & " (" & Critters (Place.Critter_Idx).Hit_Points'Image
                     & ")"
                  );
               end if;

               if not Result.Exists
                  or else
                     Critters (Place.Critter_Idx).Hit_Points
                        < Result.Hit_Points
                  or else (
                     Critters (Place.Critter_Idx).Hit_Points
                           = Result.Hit_Points
                        and then There < Result.Loc
                  )
               then
                  Result := Target'(
                     Exists => True,
                     Loc => There,
                     Hit_Points => Critters (Place.Critter_Idx).Hit_Points
                  );
               end if;

            end if;

         end;

      end loop;

      return Result;

   end Find_Target_From;

   --  SUBSECTION
   --  Part 1: Moving around

   function Branch_Possible_Routes (Self : Critter; L : Location)
      return Location_Vecs.Vector
   is
   --  returns locations of the cells which can be reached in one step from L
      Result : Location_Vecs.Vector;
   begin

      for O of Offsets loop

         declare
            New_Location : constant Location := (
               Row => L.Row + O.Drow,
               Col => L.Col + O.Dcol
            );
            Data renames Map (New_Location.Row, New_Location.Col);
            Contents renames Data.Contents;
         begin

            if Contents = Empty
               or else (
                  Contents = Being
                  and then Self /= Critters (Data.Critter_Idx).Kind
               )
            then
               Result.Append (New_Location);
            end if;

         end;

      end loop;

      return Result;

   end Branch_Possible_Routes;

   --  SUBSECTION
   --  Part 1: Breadth-First-Search

   package Path_Q_Interface is new Ada.Containers.Synchronized_Queue_Interfaces
      (Element_Type => Location_Vecs.Vector);

   package Path_Queues is new Ada.Containers.Unbounded_Synchronized_Queues
      (Queue_Interfaces => Path_Q_Interface);

   type Explored_Route is record
      First_Step, Last_Step : Location;
      Distance : Natural;
   end record;

   package Explored_Route_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type => Location, Element_Type => Explored_Route);

   function Preferred_Move_From (Here : Location) return Location is
   --  I apologize in advance for this monstrosity

      use all type Ada.Containers.Count_Type;

      Possible_Result : Explored_Route := (
         First_Step => Here,
         Last_Step => Here,
         Distance => Natural'Last
      );

      --  BFS support
      Paths : Path_Queues.Queue;
      Explored_Locations : Explored_Route_Maps.Map;

      Self : constant Critter
         := Critters (Map (Here.Row, Here.Col).Critter_Idx).Kind;

      Debug : constant Boolean   --  yes, I made heavy use of this... :-(
         := False;
         --  := True;
         --  := Here = (Row => 12, Col => 8);

   begin

      if Debug then
         Put_Map;
         IO.Put_Line ("Starting from" & Here.Row'Image & "," & Here.Col'Image);
      end if;

      --  prime the queue!
      declare
         No_Steps : Location_Vecs.Vector;
      begin
         No_Steps.Append (Here);
         Paths.Enqueue (No_Steps);
      end;

      while Paths.Current_Use > 0 loop

         declare
            Option : Location_Vecs.Vector;
            Option_Distance : Natural;
         begin

            Paths.Dequeue (Option);

            --  first check if a better path to destination has been queued
            if Option.Length > 1 and then
               Explored_Locations (Option.Last_Element)
                  .First_Step /= Option (2)
            then
               if Debug and then Option.Length > 1 then
                  IO.Put_Line (
                     "Skipped" & Option (2).Row'Image & Option (2).Col'Image
                     & " because"
                     & Explored_Locations (Option.Last_Element)
                        .First_Step.Row'Image
                     & Explored_Locations (Option.Last_Element)
                        .First_Step.Col'Image
                     & " is preferred"
                  );
               end if;
               goto End_Of_Loop;
            end if;

            Option_Distance := Natural (Option.Length) - 1;
            if Debug and then Option_Distance > 0 then
               IO.Put_Line (
                  "   Trying route of length" & Option_Distance'Image
                  & " starting from" & Option.First_Element.Row'Image
                  & "," & Option.First_Element.Col'Image
                  & " heading to" & Option.Last_Element.Row'Image
                  & "," & Option.Last_Element.Col'Image
                  & " via" & Option (2).Row'Image & "," & Option (2).Col'Image
               );
            end if;

            --  pursue this path if it's potentially profitable
            if Option_Distance < Possible_Result.Distance then

               for Each of Branch_Possible_Routes (Self, Option.Last_Element)
               loop

                  if Debug then
                     IO.Put_Line (
                        "      can reach" & Each.Row'Image
                        & "," & Each.Col'Image & " in"
                        & Natural'Image (Option_Distance + 1) & " steps"
                     );
                  end if;

                  --  is our destination an opponent?
                  if Map (Each.Row, Each.Col).Contents = Being
                     and then
                        Critters (Map (Each.Row, Each.Col).Critter_Idx).Kind
                           /= Self
                     and then Critters (Map (Each.Row, Each.Col).Critter_Idx)
                        .Hit_Points > 0
                  then

                     declare
                        --  you might think this next line is fatal
                        --  in maps that start with a neighboring opponent,
                        --  but this function is not called in that case!
                        First_Step : constant Location := Option (2);
                        Last_Step : constant Location := Option.Last_Element;
                     begin

                        if Debug then
                           IO.Put_Line (
                              "         Possible target near"
                              & Last_Step.Row'Image & Last_Step.Col'Image
                           );
                        end if;

                        --  is this preferred to any choice of equal distance?
                        if Possible_Result.Distance = Option_Distance + 1
                           and then (
                              Last_Step < Possible_Result.Last_Step
                              or else (
                                 Last_Step = Possible_Result.Last_Step
                                 and then First_Step
                                    < Possible_Result.First_Step
                              )
                           )
                        then

                           if Debug then
                              IO.Put_Line ("         keeping b/c better!");
                              IO.Put_Line ("         ("
                                 & Last_Step.Row'Image & ","
                                 & Last_Step.Col'Image & ") < ("
                                 & Possible_Result.Last_Step.Row'Image & ","
                                 & Possible_Result.Last_Step.Col'Image & ")");
                           end if;
                           Possible_Result := Explored_Route'(
                              First_Step => First_Step,
                              Last_Step => Last_Step,
                              Distance => Option_Distance + 1
                           );

                        --  is this a shorter route to the same
                        elsif Possible_Result.Distance > Option_Distance + 1
                        then
                           if Debug then
                              IO.Put_Line (
                                 "         keeping it because this is shorter!"
                              );
                           end if;
                           Possible_Result := Explored_Route'(
                              First_Step => First_Step,
                              Last_Step => Last_Step,
                              Distance => Option_Distance + 1
                           );
                        end if;

                     end;

                  --  not an opponent, but hopefully on the way to an opponent
                  --  let's queue it up, assuming a better route hasn't already
                  elsif not Explored_Locations.Contains (Each)
                     or else
                        Explored_Locations.Element (Each).Distance
                           > Option_Distance + 1
                     or else (
                        Explored_Locations.Element (Each).Distance
                           = Option_Distance + 1
                        and then Option.Length > 1
                           and then Option (2)
                              < Explored_Locations.Element (Each).First_Step
                     )
                  then

                     if Debug then
                        IO.Put_Line (
                           "         exploring"
                           & Each.Row'Image & "," & Each.Col'Image
                        );
                     end if;

                     declare
                        New_Option : Location_Vecs.Vector := Option.Copy;
                        New_Explore : constant Explored_Route := (
                           First_Step => (
                              if Option.Length = 1 then Each else Option (2)
                           ),
                           Last_Step => Option.Last_Element,
                           Distance => Option_Distance + 1
                        );
                     begin

                        New_Option.Append (Each);
                        Paths.Enqueue (New_Option);

                        if Explored_Locations.Contains (Each) then
                           Explored_Locations.Replace (Each, New_Explore);
                        else
                           Explored_Locations.Insert (Each, New_Explore);
                        end if;

                     end;

                  end if;

               end loop;

            end if;

         end;

         <<End_Of_Loop>>

      end loop;

      return Possible_Result.First_Step;

   end Preferred_Move_From;

   function Still_Fighting return Boolean is (
      (for some C of Critters => C.Hit_Points > 0 and then C.Kind = Elf)
      and then
      (for some C of Critters => C.Hit_Points > 0 and then C.Kind = Goblin)
   );

   function Part_1 return Natural is
      Round : Natural := 0;
      Result : Natural := 0;
      Debug : constant Boolean := False;
   begin

      while Still_Fighting loop

         Round := @ + 1;
         if Debug then
            IO.Put_Line ("----- ROUND" & Round'Image & " -----");
            --  Put_Map;
         end if;

         --  sort warriors
         --  since the map contains references to warriors, update those, too
         Warrior_Sorter.Sort (Critters);
         for Idx in Critters.First_Index .. Critters.Last_Index loop
            if Critters (Idx).Hit_Points > 0 then
               Map (Critters (Idx).Row, Critters (Idx).Col).Critter_Idx := Idx;
            end if;
         end loop;

         --  handle each living critter in turn
         for Idx in Critters.First_Index .. Critters.Last_Index loop

            if not Still_Fighting then    -- don't forget to check this again!
               Round := @ - 1;
               goto Game_Over;
            end if;

            declare
               C : constant Warrior := Critters (Idx);
               CLoc : constant Location
                  := Location'(Row => C.Row, Col => C.Col);
            begin

               if C.Hit_Points = 0 then
               --  I'm dead!
                  goto Turn_Over;
               end if;

               if Debug then
                  IO.Put_Line (
                     "Start of turn for" & CLoc.Row'Image & CLoc.Col'Image
                     & (if C.Kind = Elf then " (elf)" else " (goblin)")
                     & " with" & C.Hit_Points'Image & " hit points"
                  );
               end if;

               declare
                  T : Target := Find_Target_From (CLoc);
               begin

                  --  can we attack an enemy?
                  if T.Exists then
                     Attack (T.Loc);

                  else

                     declare
                        Dest : constant Location := Preferred_Move_From (CLoc);
                     begin

                        if Dest /= CLoc then

                           if Debug then
                              IO.Put_Line (
                                 "   moves to" & Dest.Row'Image
                                 & Dest.Col'Image
                              );
                           end if;

                           Map (Dest.Row, Dest.Col) := Map (C.Row, C.Col);
                           Map (C.Row, C.Col) := Cell'(Contents => Empty);
                           Critters (Idx).Row := Dest.Row;
                           Critters (Idx).Col := Dest.Col;

                           --  can we attack now?
                           T := Find_Target_From (Dest);
                           if T.Exists then
                              Attack (T.Loc);
                           end if;

                        end if;

                     end;

                  end if;

               end;

               if Debug then
                  IO.Put_Line ("End of turn for" & C.Row'Image & C.Col'Image);
               end if;

            end;

            <<Turn_Over>>

         end loop;

      end loop;

      <<Game_Over>>
      IO.Put_Line ("Combat ended after" & Round'Image & " rounds");

      for C of Critters loop
         if C.Hit_Points > 0 then
            IO.Put_Line (
               "Critter at" & C.Row'Image & C.Col'Image
               & " has" & C.Hit_Points'Image
            );
            Result := @ + C.Hit_Points;
         end if;
      end loop;
      IO.Put_Line ("Total hit points:" & Result'Image);

      return Result * Round;

   end Part_1;

   --  SUBSECTION
   --  Part 2
   --  After the nightmare of Part 1, Part 2 is actually quite easy!

   Original_Map : Map_Array;
   Original_Critters : Warrior_Vecs.Vector;

   function Part_2 return Natural is
   begin

      loop

         --  reset to initial state
         Map := Original_Map;
         Critters := Original_Critters.Copy;
         Elf_Attack := @ + 1;

         IO.New_Line;
         IO.Put_Line ("Trying part 2 with elf attack of" & Elf_Attack'Image);

         declare
            Result : constant Natural := Part_1;
         begin

            if (
               for all C of Critters => (if C.Kind = Elf then C.Hit_Points > 0)
            )
            then
               return Result;
            end if;

         end;

      end loop;

   end Part_2;

begin
   Read_Input;
   Original_Map := Map;
   Original_Critters := Critters.Copy;
   Put_Map;

   IO.Put_Line ("the outcome of Part 1 is" & Part_1'Image);

   IO.Put_Line (
      "to win, without losing any elves (outcome of" & Part_2'Image
      & "), elves need an attack strength of at least" & Elf_Attack'Image
   );

end Day15;
