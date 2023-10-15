--  Advent of Code 2018
--
--  John Perry
--
--  Day 4: Repose Record
--
--  part 1: During what minute does the guard who sleeps most, sleep most?
--
--  part 2: During which minute does some guard sleep most?

with Ada.Text_IO;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;

procedure Day4 is

   --  SECTION
   --  global types and variables

   --  SUBSECTION
   --  Guards

   type Minutes is array (0 .. 59) of Natural;
   Initial_Minutes : constant Minutes := [others => 0];

   package Guard_Maps is new Ada.Containers.Ordered_Maps (
      Key_Type     => Positive,
      Element_Type => Minutes
   );
   Guards : Guard_Maps.Map;

   --  SUBSECTION
   --  guard behaviors

   type Date is record
      Month, Day, Hour, Minute : Natural;
   end record;

   type Action is (Shift, Sleep, Wake);

   type Behavior (Performed : Action) is record
      Timestamp : Date;
      case Performed is
         when Shift => Id : Positive;
         when others => null;
      end case;
   end record;

   function New_Behavior (
      Performed : Action;
      Timestamp : Date;
      Id : Positive := 1
   )
      return Behavior
   is
   begin
      return Result : Behavior (Performed) do
         Result.Timestamp := Timestamp;
         case Performed is
            when Shift  => Result.Id := Id;
            when others => null;
         end case;
      end return;
   end New_Behavior;

   package Shift_Vecs is new Ada.Containers.Indefinite_Vectors (
      Index_Type   => Positive,
      Element_Type => Behavior
   );

   Shifts : Shift_Vecs.Vector;

   --  SUBSECTION
   --  Sorting the Shift Vectors

   function "<" (Left, Right : Date) return Boolean is (
      Left.Month < Right.Month or else (
         Left.Month = Right.Month and then Left.Day < Right.Day
      ) or else (
         Left.Month = Right.Month and then Left.Day = Right.Day
            and then Left.Hour < Right.Hour
      ) or else (
         Left.Month = Right.Month and then Left.Day = Right.Day
            and then Left.Hour = Right.Hour and then Left.Minute < Right.Minute
      )
   );

   function "<" (Left, Right : Behavior) return Boolean is
   (Left.Timestamp < Right.Timestamp);

   package Shift_Vec_Sorter is new Shift_Vecs.Generic_Sorting;

   --  SECTION
   --  I/O

   package TIO renames Ada.Text_IO;

   function Read_Two_Digit (S : String) return Natural is
   (Natural'Value (S (S'First .. S'First + 1)));

   Bad_Action : exception;

   procedure Read_Input is

      F : TIO.File_Type;

   begin

      TIO.Open (F, TIO.In_File, "input.txt");

      while not TIO.End_Of_File (F) loop

         declare
            S      : constant String  := TIO.Get_Line (F);
            Month  : constant Natural := Read_Two_Digit (S (7 .. S'Last));
            Day    : constant Natural := Read_Two_Digit (S (10 .. S'Last));
            Hour   : constant Natural := Read_Two_Digit (S (13 .. S'Last));
            Minute : constant Natural := Read_Two_Digit (S (16 .. S'Last));
            Deed   : constant Action  := (
               case S (20) is
                  when 'G' => Shift,
                  when 'f' => Sleep,
                  when 'w' => Wake,
                  when others =>
                     raise Bad_Action
            );
         begin

            case Deed is

               when Shift =>

                  declare Id : constant Positive
                     := Positive'Value (S (27 .. 30));
                  begin

                     if not Guards.Contains (Id) then
                        Guards.Insert (Id, Initial_Minutes);
                     end if;

                     Shifts.Append (
                        Behavior'(
                           Performed => Shift,
                           Timestamp => Date'(Month, Day, Hour, Minute),
                           Id => Id
                        )
                     );

                  end;

               when others =>

                  Shifts.Append (
                     New_Behavior (Deed, Date'(Month, Day, Hour, Minute))
                  );

            end case;

         end;

      end loop;

      TIO.Close (F);

      Shift_Vec_Sorter.Sort (Shifts);

   end Read_Input;

   --  Parts 1 and 2

   procedure Determine_Minutes is
   --  for each shift, determines which minutes the guards slept
   --  assumes shifts are already sorted by time

      Id : Positive;
      I  : Positive := Shifts.First_Index;

   begin

      while I <= Shifts.Last_Index loop

         Id := Shifts (I).Id;
         I := I + 1;

         while I <= Shifts.Last_Index and then
               Shifts (I).Performed /= Shift
         loop

            for Minute in
               Shifts (I).Timestamp.Minute ..
                  Shifts (I + 1).Timestamp.Minute - 1
            loop
               Guards (Id) (Minute) := @ + 1;
            end loop;

            I := I + 2;

         end loop;

      end loop;

   end Determine_Minutes;

   function Part_1 return Natural is

      Winner : Positive;
      Max_Sleep : Natural := 0;
      Minute_Max : Natural := 0;

   begin

      for Position in Guards.Iterate loop

         declare
            Id : constant Positive := Guard_Maps.Key (Position);
            Total_Sleep : constant Natural
               := Guard_Maps.Element (Position)'Reduce ("+", 0);
         begin

            if Total_Sleep > Max_Sleep then
               Winner := Id;
               Max_Sleep := Total_Sleep;
            end if;

         end;

      end loop;

      Max_Sleep := 0;

      for Minute in 0 .. 59 loop

         if Guards (Winner) (Minute) > Max_Sleep then
            Minute_Max := Minute;
            Max_Sleep := Guards (Winner) (Minute);
         end if;

      end loop;

      return Winner * Minute_Max;

   end Part_1;

   function Part_2 return Natural is

      Winner    : Positive;
      Min_Mode  : Natural := 0;
      Max_Sleep : Natural := 0;

   begin

      for Position in Guards.Iterate loop

         declare
            Id : constant Positive := Guard_Maps.Key (Position);
         begin

            for Min in 0 .. 59 loop
               if Guard_Maps.Element (Position) (Min) > Max_Sleep then
                  Winner := Id;
                  Min_Mode := Min;
                  Max_Sleep := Guard_Maps.Element (Position) (Min);
               end if;
            end loop;

         end;

      end loop;

      return Winner * Min_Mode;

   end Part_2;

begin
   Read_Input;
   Determine_Minutes;
   TIO.Put_Line ("Strategy 1 gives ID" & Part_1'Image);
   TIO.Put_Line ("Strategy 2 gives ID" & Part_2'Image);
end Day4;
