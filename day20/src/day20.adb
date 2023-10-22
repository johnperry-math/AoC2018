--  Advent of Code 2018
--
--  John Perry
--
--  Day 20: A Regular Map
--
--  part 1: determine the room that requires the most steps to reach
--          (maximum of all shortest distances)
--
--  part 2: how many rooms are at least 1000 steps away?

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Hashed_Maps;

procedure Day20 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  Global types and variables

   --  SUBSECTION
   --  debugging; didn't need during the second attempt, amazingly enough

   Indentation : Natural := 0;
   Debug : constant Boolean := False;

   procedure Indent is
   begin
      for Each in 1 .. Indentation loop
         IO.Put (' ');
      end loop;
   end Indent;

   --  SUBSECTION
   --  Rooms and room tracking

   type Room is record
      Row, Col : Integer;
   end record;

   --  SUBSECTION
   --  terminating a path

   function Terminates_Path (C : Character) return Boolean is
      (C = '$' or else C = '|' or else C = ')');

   --  SECTION
   --  first attempt, which works with the examples, but *not* with the input,
   --  apparently because one path backtracks in just the right way
   --  to make the path one step longer

   function Path_Length (S : String; Pos : in out Positive) return Natural;
   --  forward declaration

   function Option_Lengths (S : String; Pos : in out Positive) return Natural
   is
   --  computes the lengths of each option and returns the longest,
   --  unless they all backtrack on each other, in which case it returns 0
   --
   --  expects to begin on first letter of first option, NOT on its opening (;
   --  ends on its closing )

      This_Option : Natural;
      Result : Natural := 0;

   begin

      Indentation := @ + 3;

      while S (Pos) /= ')' loop

         if Debug then
            Indent;
            IO.Put ("Option starting at" & Pos'Image & " (");
            IO.Put (S (Pos));
            IO.Put_Line (")");
         end if;

         This_Option := Path_Length (S, Pos);

         if Debug then
            Indent;
            IO.Put_Line (
               "ends at" & Pos'Image & " with length" & This_Option'Image
            );
         end if;

         Result := Natural'Max (@, This_Option);

         if  S (Pos) = '|' then
            Pos := @ + 1;
            if S (Pos) = ')' then
               Result := 0;
            end if;
         end if;

      end loop;

      if Debug then
         Indent;
         IO.Put_Line ("Chosen option has length" & Result'Image);
      end if;

      Indentation := @ - 3;

      return Result;

   end Option_Lengths;

   function Path_Length (S : String; Pos : in out Positive) return Natural is
   --  determines the length of the path
   --  expects to begin on first letter, NOT on some other symbol;
   --  ends on a terminating symbol

      Result : Natural := 0;

   begin

      while not Terminates_Path (S (Pos)) loop

         if S (Pos) = '(' then

            Pos := Pos + 1;   --  skip (
            if Debug then
               IO.Put_Line (
                  "Options for path at" & Pos'Image & " with" & Result'Image
               );
            end if;
            Result := @ + Option_Lengths (S, Pos);
            Pos := Pos + 1;   --  skip )

         else
            Pos := Pos + 1;
            Result := @ + 1;

         end if;

      end loop;

      if Debug then
         Indent;
         IO.Put_Line ("Path has length" & Result'Image);
      end if;

      return Result;

   end Path_Length;

   --  SECTION
   --  second attempt
   --
   --  this approach maps each reachable location
   --  to the number of steps it would take to get there,
   --  after which you can simply iterate through the map to examine distances

   type Room_Distance is record
      Loc : Room;
      Distance : Natural;
   end record;

   function Room_Hash (Element : Room) return Ada.Containers.Hash_Type is (
      Ada.Containers.Hash_Type (abs (Element.Row) * 10_000 + abs (Element.Col))
   );

   package Room_Length_Maps is new Ada.Containers.Hashed_Maps (
      Key_Type => Room,
      Element_Type => Natural,
      Hash => Room_Hash,
      Equivalent_Keys => "="
   );

   Room_Lengths : Room_Length_Maps.Map;

   procedure Remember (Location : Room_Distance) is
      use all type Room_Length_Maps.Cursor;
      Cursor : constant Room_Length_Maps.Cursor
         := Room_Lengths.Find (Location.Loc);
   begin
      if Cursor = Room_Length_Maps.No_Element then
         Room_Lengths.Insert (Location.Loc, Location.Distance);
      else
         if Room_Lengths (Cursor) > Location.Distance then
            Room_Lengths.Replace_Element (Cursor, Location.Distance);
         end if;
      end if;
   end Remember;

   procedure Explore_Path_From (
      S : String; Pos : in out Positive; Start : Room_Distance
   );
   --  forward declaration

   procedure Explore_Options_From (
      S : String;
      Pos : in out Positive;
      Start : Room_Distance
   ) is
   --  expects to begin on letter; ends on its own closing )

      This_Option : constant Room_Distance := Start;

   begin

      Indentation := @ + 3;

      while S (Pos) /= ')' loop

         Explore_Path_From (S, Pos, This_Option);

         if  S (Pos) = '|' then
            Pos := @ + 1;
         end if;

      end loop;

      Indentation := @ - 3;

   end Explore_Options_From;

   Unexpected_Character : exception;

   procedure Explore_Path_From (
      S : String;
      Pos : in out Positive;
      Start : Room_Distance
   )
   is
      Current : Room_Distance := Start;
   begin

      while not Terminates_Path (S (Pos)) loop

         if S (Pos) = '(' then

            Pos := @ + 1;   --  skip (
            Explore_Options_From (S, Pos, Current);
            Pos := Pos + 1;   --  skip )

         else

            case S (Pos) is
            when 'N' => Current.Loc.Row := @ - 1;
            when 'S' => Current.Loc.Row := @ + 1;
            when 'E' => Current.Loc.Col := @ + 1;
            when 'W' => Current.Loc.Col := @ - 1;
            when others => raise Unexpected_Character with S (Pos .. Pos + 1);
            end case;
            Current.Distance := @ + 1;
            Remember (Current);

            Pos := Pos + 1;

         end if;

      end loop;

   end Explore_Path_From;

   F : IO.File_Type;

begin

   IO.Open (F, IO.In_File, "input.txt");

   declare

      Pos : Positive := 2;
      Max_Distance : Natural := 0;
      Rooms_At_Least_1000 : Natural := 0;

      Start : constant Room_Distance := ((0, 0), 0);

      Regex : constant String := IO.Get_Line (F);

      --  examples, used for debugging
      --  Regex : constant String := "^ENWWW(NEEE|SSE(EE|N))$";
      --  Regex : constant String
      --     := "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$)";
      --  Regex : constant String
      --     := "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$";
      --  Regex : constant String
      --     := "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$";

   begin

      --  first attempt turned out to be wrong;
      --  keeping for the sake of humility
      IO.Put_Line (
        "Probably wrong answer:" & Path_Length (Regex, Pos)'Image
      );
      Pos := 2;

      Explore_Path_From (Regex, Pos, Start);

      for Value of Room_Lengths loop
         Max_Distance := Natural'Max (Max_Distance, Value);
         if Value >= 1000 then
            Rooms_At_Least_1000 := @ + 1;
         end if;
      end loop;

      IO.Put_Line ("Largest # of doors is" & Max_Distance'Image);
      IO.Put_Line (
         "At least" & Rooms_At_Least_1000'Image
         & " rooms need at least 1000 doors"
      );

   end;

   IO.Close (F);

end Day20;
