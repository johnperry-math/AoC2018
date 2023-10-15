--  Advent of Code 2018
--
--  John Perry
--
--  Day 10: The Stars Align
--
--  part 1: determine the message spelled out by moving points in the sky
--
--  part 2: how many seconds would it take for the message to spell out?

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day10 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  Global types and variables

   type Point is record
      X, Y : Integer;
      Dx, Dy : Integer;
   end record;

   package Point_Vecs is new Ada.Containers.Vectors
      (Index_Type => Positive, Element_Type => Point);

   Points : Point_Vecs.Vector;

   --  I/O

   procedure Read_Input is
      package Int_IO is new IO.Integer_IO (Num => Integer);
      F : IO.File_Type;
      Pos : Natural;
   begin
      IO.Open (F, IO.In_File, "input.txt");
      while not IO.End_Of_File (F) loop
         declare
            S : constant String := IO.Get_Line (F);
            X, Y, Dx, Dy : Integer;
         begin
            Int_IO.Get (S (12 .. S'Last),  X, Pos);
            Int_IO.Get (S (20 .. S'Last),  Y, Pos);
            Int_IO.Get (S (38 .. S'Last), Dx, Pos);
            Int_IO.Get (S (42 .. S'Last), Dy, Pos);
            if S (11) = '-' then  X := - X; end if;
            if S (19) = '-' then  Y := - Y; end if;
            if S (37) = '-' then Dx := -Dx; end if;
            if S (41) = '-' then Dy := -Dy; end if;
            Points.Append (Point'(X, Y, Dx, Dy));
         end;
      end loop;
      IO.Close (F);
   end Read_Input;

   --  SECTION
   --  Parts 1 and 2

   procedure Puzzle is

      Iteration : Natural := 0;
      Possible_Messages : Natural := 0;
      Min_X, Min_Y, Max_X, Max_Y : Integer;

      --  termination conditions

      Old_Width : Positive := Positive'Last;
      Old_Height : Positive := Positive'Last;
      Current_Width, Current_Height : Positive;

      --  screen display

      Screen_Width : constant Positive := 100;
      Screen_Height : constant Positive := 100;

      type ScreenType is
         array (1 .. Screen_Width, 1 .. Screen_Height) of Character;
      Fresh_Screen : constant ScreenType := [others => [others => ' ']];
      Screen : ScreenType;

   begin

      loop

         --  determine current dimensions of points;
         --  terminate when points start to grow further apart

         Min_X := Integer'Last;
         Min_Y := Integer'Last;
         Max_X := Integer'First;
         Max_Y := Integer'First;

         for P of Points loop
            Min_X := Integer'Min (Min_X, P.X);
            Min_Y := Integer'Min (Min_Y, P.Y);
            Max_X := Integer'Max (Max_X, P.X);
            Max_Y := Integer'Max (Max_Y, P.Y);
         end loop;

         Current_Width := Max_X - Min_X;
         Current_Height := Max_Y - Min_Y;

         exit when Current_Width > Old_Width
            or else Current_Height > Old_Height;

         --  if small enough, print it

         if Current_Width < Screen_Width
            and then Current_Height < Screen_Height
         then

            --  status message
            Possible_Messages := @ + 1;
            IO.Put_Line (
               "Iteration" & Iteration'Image & ":"
               & Current_Width'Image & " x"
               & Current_Height'Image & " with"
               & Possible_Messages'Image & " possible messages"
            );

            --  display "message"
            Screen := Fresh_Screen;
            for P of Points loop
               Screen (P.X - Min_X + 1, P.Y - Min_Y + 1) := '#';
            end loop;
            for Y in ScreenType'First (1) .. Max_Y - Min_Y + 1 loop
               for X in ScreenType'First (2) .. Max_X - Min_X + 1 loop
                  IO.Put (Screen (X, Y));
               end loop;
               IO.New_Line;
            end loop;
            IO.New_Line (5);

         end if;

         --  apply motion
         for P of Points loop
            P.X := @ + P.Dx;
            P.Y := @ + P.Dy;
         end loop;

         --  status update
         Iteration := @ + 1;
         Old_Width := Current_Width;
         Old_Height := Current_Height;

      end loop;

   end Puzzle;

begin
   Read_Input;
   Puzzle;
end Day10;
