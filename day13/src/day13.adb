--  Advent of Code 2018
--
--  John Perry
--
--  Day 13: Mine Cart Madness
--
--  part 1: locate the first cart crash
--
--  part 2: locate the sole surviving cart

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Generic_Array_Sort;

procedure Day13 is

   package IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   --  SECTION
   --  Global types and variables

   --  SUBSECTION
   --  Carts

   type Cart_Dir is (Up, Down, Left, Right);

   type Position is record
      Row, Col : Natural;
   end record;

   type Choice is (Choose_Left, Choose_Straight, Choose_Right);

   Dir_After_Choice : constant array (Cart_Dir, Choice) of Cart_Dir := [
      Up =>
         [Choose_Left => Left, Choose_Straight => Up, Choose_Right => Right],
      Down =>
         [Choose_Left => Right, Choose_Straight => Down, Choose_Right => Left],
      Left =>
         [Choose_Left => Down, Choose_Straight => Left, Choose_Right => Up],
      Right =>
         [Choose_Left => Up, Choose_Straight => Right, Choose_Right => Down]
   ];

   type Cart is record
      Pos : Position;
      Dir : Cart_Dir;
      Next_Choice : Choice;
      Crashed : Boolean;
   end record;

   --  SUBSECTION
   --  Railroad

   Horiz_Dim : constant Natural := (if Doing_Example then 7 else 150);
   Vert_Dim : constant Natural := (if Doing_Example then 7 else 150);

   subtype Horizontal_Range is Natural range 1 .. Horiz_Dim;
   subtype Vertical_Range is Natural range 1 .. Vert_Dim;

   type Rail_Type is (
      North_South, East_West, North_East, South_East, Inter, None
   );

   type Rail is array (Vertical_Range, Horizontal_Range) of Rail_Type;

   Railroad : Rail;

   Number_of_Carts : constant Natural := (if Doing_Example then 9 else 17);

   subtype Cart_Range is Natural range 1 .. Number_of_Carts;

   type Cart_Array is array (Cart_Range range <>) of Cart;

   Carts : Cart_Array (Cart_Range);

   --  SECTION
   --  I/O

   --  SUBSECTION
   --  Input

   Whoops : exception;     --  a very meaningful message ;-)

   procedure Read_Input is

      F       : IO.File_Type;

      Cart_Id : Cart_Range     := Cart_Range'First;
      Row     : Vertical_Range := Vertical_Range'First;

   begin

      IO.Open (F, IO.In_File,
         (if Doing_Example then "example.txt" else "input.txt")
      );

      while not IO.End_Of_File (F) loop

         declare
            S : constant String := IO.Get_Line (F);
         begin

            for Col in 1 .. Horiz_Dim loop

               Railroad (Row, Col) := (
                  case (S (Col)) is
                  when '-' | '>' | '<' => East_West,
                  when '|' | '^' | 'v' => North_South,
                  when '/' => North_East,
                  when '\' => South_East,
                  when '+' => Inter,
                  when ' ' => None,
                  when others =>
                        raise Whoops
               );

               if S (Col) = '>' then
                  Carts (Cart_Id) := Cart'(
                     Pos => Position'(Row, Col),
                     Dir => Right,
                     Next_Choice => Choose_Left,
                     Crashed => False
                  );
                  if Cart_Id < Cart_Range'Last then Cart_Id := @ + 1; end if;
               elsif S (Col) = '<' then
                  Carts (Cart_Id) := Cart'(
                     Pos => Position'(Row, Col),
                     Dir => Left,
                     Next_Choice => Choose_Left,
                     Crashed => False
                  );
                  if Cart_Id < Cart_Range'Last then Cart_Id := @ + 1; end if;
               elsif S (Col) = '^' then
                  Carts (Cart_Id) := Cart'(
                     Pos => Position'(Row, Col),
                     Dir => Up,
                     Next_Choice => Choose_Left,
                     Crashed => False
                  );
                  if Cart_Id < Cart_Range'Last then Cart_Id := @ + 1; end if;
               elsif S (Col) = 'v' then
                  Carts (Cart_Id) := Cart'(
                     Pos => Position'(Row, Col),
                     Dir => Down,
                     Next_Choice => Choose_Left,
                     Crashed => False
                  );
                  if Cart_Id < Cart_Range'Last then Cart_Id := @ + 1; end if;
               end if;

            end loop;

         end;

         if Row < Vertical_Range'Last then Row := @ + 1; end if;

      end loop;

      IO.Close (F);

   end Read_Input;

   --  SUBSECTION
   --  Ouput

   Cart_Dir_Image : constant array (Cart_Dir) of Character
      := ['^', 'v', '<', '>'];

   Rail_Image : constant array (Rail_Type) of Character
      := ['|', '-', '/', '\', '+', ' '];

   procedure Put_Railroad is
   begin

      for Row in Rail'Range (1) loop
         for Col in Rail'Range (2) loop

            declare
               Dir          : Cart_Dir;
               Showed_Cart  : Boolean := False;
               Crashed_Cart : Boolean := False;
            begin

               --  check if there's a cart or a crash
               --  (if both, prefer cart)
               for C of Carts loop
                  if C.Pos = Position'(Row, Col) then
                     if not C.Crashed then
                        Showed_Cart := True;
                        Crashed_Cart := False;
                        Dir := C.Dir;
                     else
                        if not Showed_Cart then
                           Crashed_Cart := True;
                        end if;
                     end if;
                  end if;
               end loop;

               if Showed_Cart then
                  IO.Put (Cart_Dir_Image (Dir));
               elsif Crashed_Cart then
                  IO.Put ('X');
               else
                  IO.Put (Rail_Image (Railroad (Row, Col)));
               end if;

            end;

         end loop;

         IO.New_Line;

      end loop;

   end Put_Railroad;

   --  SECTION
   --  Parts 1 and 2

   Inconsistent_Direction, Off_Track : exception;
   --  somewhat more meaningful error messages ;-)

   --  SUBSECTION
   --  Sorting: need to sort the carts at the beginning of each iteration

   function "<" (Left, Right : Cart) return Boolean is
      (
         Left.Pos.Row < Right.Pos.Row or else (
            Left.Pos.Row = Right.Pos.Row and then Left.Pos.Col < Right.Pos.Col
         )
      );

   procedure Sort is new Ada.Containers.Generic_Array_Sort (
      Index_Type => Cart_Range,
      Element_Type => Cart,
      Array_Type => Cart_Array
   );

   --  SUBSECTION
   --  Moving carts around

   Iterations : Natural := 0;

   procedure Iterate is
   begin

      Iterations := @ + 1;

      Sort (Carts);     --  reading the directions carefully matters...

      for I in Cart_Range loop

         if not Carts (I).Crashed then
            declare
               C renames Carts (I);
               R : constant Rail_Type := Railroad (C.Pos.Row, C.Pos.Col);
            begin

               case R is

               when North_South =>

                  case C.Dir is
                  when Up => C.Pos.Row := @ - 1;
                  when Down => C.Pos.Row := @ + 1;
                  when others => raise Inconsistent_Direction;
                  end case;

               when East_West =>

                  case C.Dir is
                  when Left => C.Pos.Col := @ - 1;
                  when Right => C.Pos.Col := @ + 1;
                  when others => raise Inconsistent_Direction;
                  end case;

               when North_East =>

                  case C.Dir is
                  when Up =>
                     C.Pos.Col := @ + 1;
                     C.Dir := Right;
                  when Down =>
                     C.Pos.Col := @ - 1;
                     C.Dir := Left;
                  when Left =>
                     C.Pos.Row := @ + 1;
                     C.Dir := Down;
                  when Right =>
                     C.Pos.Row := @ - 1;
                     C.Dir := Up;
                  end case;

               when South_East =>

                  case C.Dir is
                  when Up =>
                     C.Pos.Col := @ - 1;
                     C.Dir := Left;
                  when Down =>
                     C.Pos.Col := @ + 1;
                     C.Dir := Right;
                  when Left =>
                     C.Pos.Row := @ - 1;
                     C.Dir := Up;
                  when Right =>
                     C.Pos.Row := @ + 1;
                     C.Dir := Down;
                  end case;

               when Inter =>

                  C.Dir := Dir_After_Choice (C.Dir, C.Next_Choice);
                  C.Next_Choice := (
                     if C.Next_Choice = Choice'Last then Choice'First
                     else Choice'Succ (C.Next_Choice)
                  );

                  case C.Dir is
                  when Up => C.Pos.Row := @ - 1;
                  when Down => C.Pos.Row := @ + 1;
                  when Left => C.Pos.Col := @ - 1;
                  when Right => C.Pos.Col := @ + 1;
                  end case;

               when None => raise Off_Track;

               end case;

            end;

            for J in Cart_Range loop

               if I /= J and then Carts (J).Pos = Carts (I).Pos
                  and then not Carts (J).Crashed
               then

                  Carts (I).Crashed := True;
                  Carts (J).Crashed := True;

                  IO.Put ("carts" & I'Image & " &" & J'Image & " crashed at");
                  IO.Put (
                     Carts (I).Pos.Col'Image & ","
                     & Carts (I).Pos.Row'Image
                  );
                  IO.New_Line;

               end if;

            end loop;

         end if;

      end loop;

   end Iterate;

   --  SUBSECTION
   --  Part 1

   function Part_1 return Position is
      Iteration : Natural := 0;
   begin

      loop

         Iterate;

         for Idx in Cart_Range loop
            if Carts (Idx).Crashed then
               return Carts (Idx).Pos;
            end if;
         end loop;

      end loop;

   end Part_1;

   --  SUBSECTION
   --  Part 2

   function Part_2 return Position is
      Iteration : Natural := 0;
      Surviving_Cart : Cart_Range;
   begin

      loop

         Iterate;

         declare
            Crashes : Natural := 0;
         begin

            Surviving_Cart := Cart_Range'First;

            for Idx in Cart_Range loop
               if Carts (Idx).Crashed then
                  Crashes := @ + 1;
               else
                  Surviving_Cart := Idx;
               end if;
            end loop;
            
            if Crashes = Number_of_Carts - 1 then
               IO.Put_Line ("cart" & Surviving_Cart'Image & " survived");
               exit;
            end if;

         end;

      end loop;

      return Carts (Surviving_Cart).Pos;

   end Part_2;

begin

   Read_Input;

   declare
      Crash_Location : constant Position := Part_1;
   begin

      Put_Railroad;
      IO.Put_Line (
         "First crash occurrs on iteration" & Iterations'Image
         & " at" & Natural'Image (Crash_Location.Col - 1)
         & "," & Natural'Image (Crash_Location.Row - 1)
      );

   end;

   declare
      Survivor : constant Position := Part_2;
   begin

      Put_Railroad;

      IO.Put_Line (
         "Last surviving cart on iteration" & Iterations'Image
         & " at" & Natural'Image (Survivor.Col - 1)
         & "," & Natural'Image (Survivor.Row - 1)
      );

   end;

end Day13;
