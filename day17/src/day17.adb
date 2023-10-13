--  Advent of Code 2018
--
--  John Perry
--
--  Day 17: Reservoir Research
--
--  part 1: how many positions will the spring water sink? limit the count
--          to positions that like within the y-values given in the input
--
--  part 2: how many of these positions will be retained once the spring
--          dries up?

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

procedure Day17 is

   package IO renames Ada.Text_IO;
   package Nat_IO is new IO.Integer_IO (Num => Natural);
   use all type Ada.Containers.Count_Type;

   --  SECTION
   --  Global types and variables

   package X_Vectors is new Ada.Containers.Vectors (
      Index_Type => Positive, Element_Type => Positive
   );
   --  contains the X values corresponding to a Y value

   function "=" (Left, Right : X_Vectors.Vector) return Boolean is
      (
         Left.Length = Right.Length and then
         (
            for all I in Left.First_Index .. Left.Last_Index =>
               Left (I) = Right (I - Left.First_Index + Right.First_Index)
         )
      );
   --  really, Ada?

   package X_Sorter is new X_Vectors.Generic_Sorting;

   package Y_Vectors is new Ada.Containers.Vectors (
      Index_Type => Positive, Element_Type => X_Vectors.Vector
   );

   Bounds : Y_Vectors.Vector;
   --  each Y value's correspondence with its X values

   Min_X : Natural := Natural'Last;
   Max_X : Natural := Natural'First;
   Min_Y : Natural := Natural'Last;
   Max_Y : Natural := Natural'First;
   --  helpful with drawing output (and, it turns out, with Part 1...)

   type Position is record
      X, Y : Natural;
   end record;

   function Position_Hash (P : Position) return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type (P.X * 2000 + P.Y));

   package Position_Sets is new Ada.Containers.Hashed_Sets (
      Element_Type => Position,
      Hash => Position_Hash,
      Equivalent_Elements => "="
   );

   Water : Position_Sets.Set;
   --  recorded positions of water
   Streams : Position_Sets.Set;
   --  a subset of the above that corresponds to streams of falling water

   --  SECTION
   --  I/O

   procedure Sort_And_Remove_Duplicates (V : in out X_Vectors.Vector) is
   --  our reader is a little sloppy, so we might have duplicates
   --  moreover, we'll want to have the x values sorted

      Result : X_Vectors.Vector;

   begin

      X_Sorter.Sort (V);

      for X of V loop
         if Result.Length = 0 or else Result.Last_Element /= X then
            Result.Append (X);
         end if;
      end loop;

      V := Result;

   end Sort_And_Remove_Duplicates;

   procedure Read_Input is
      F : IO.File_Type;
   begin

      IO.Open (F, IO.In_File, "input.txt");

      while not IO.End_Of_File (F) loop
         declare
            S : constant String := IO.Get_Line (F);
            X1, X2, Y1, Y2 : Natural;
            Pos : Positive := 3;
         begin

            if S (1) = 'x' then
               --  this is a column
               Nat_IO.Get (S (Pos .. S'Length), X1, Pos);
               Pos := @ + 5;
               Nat_IO.Get (S (Pos .. S'Length), Y1, Pos);
               Pos := @ + 3;
               Nat_IO.Get (S (Pos .. S'Length), Y2, Pos);
               Max_X := Natural'Max (X1, Max_X);
               Min_X := Natural'Min (X1, Min_X);
               Min_Y := Natural'Min (Y1, Min_Y);
               if Max_Y < Natural'Max (Y1, Y2) then
                  Max_Y := Natural'Max (Y1, Y2);
                  Bounds.Set_Length (Ada.Containers.Count_Type (Max_Y + 1));
               end if;
               for Y in Y1 .. Y2 loop
                  Bounds (Y).Append (X1);
               end loop;

            else
               --  this is a row
               Nat_IO.Get (S (Pos .. S'Length), Y1, Pos);
               Pos := @ + 5;
               Nat_IO.Get (S (Pos .. S'Length), X1, Pos);
               Pos := @ + 3;
               Nat_IO.Get (S (Pos .. S'Length), X2, Pos);
               if Max_Y < Y1 then
                  Max_Y := Y1;
                  Bounds.Set_Length (Ada.Containers.Count_Type (Max_Y + 1));
               end if;
               Max_Y := Natural'Max (Y1, Max_Y);
               Min_Y := Natural'Min (Y1, Min_Y);
               Min_X := Natural'Min (X1, Min_X);
               for X in X1 .. X2 loop
                  Max_X := Natural'Max (X, Max_X);
                  Bounds (Y1).Append (X);
               end loop;

            end if;

         end;

      end loop;

      for Each of Bounds loop
         Sort_And_Remove_Duplicates (Each);
      end loop;

      IO.Close (F);

   end Read_Input;

   procedure Draw_Soil (
      First : Positive := Bounds.First_Index;
      Last : Positive := Bounds.Last_Index
   ) is
   --  useful for debugging
   begin

      for Y in First .. Last loop

         declare
            Idx : Positive := 1;
            Row : X_Vectors.Vector renames Bounds (Y);
         begin

            for X in Min_X - 5 .. Max_X + 5 loop

               if Idx <= Row.Last_Index and then Row (Idx) = X then
                  IO.Put ('#');
                  Idx := Idx + 1;

               elsif Water.Contains ((X, Y)) then
                  IO.Put ('~');

               else
                  IO.Put (' ');

               end if;

            end loop;

         end;

         IO.New_Line;

      end loop;

   end Draw_Soil;

   procedure Write_Soil_To_File is

      F : IO.File_Type;
      Name : String (1 .. 6) := [others => '0'];
      File_Number : constant String := Water.Length'Image;

      type Rgb is record
         Red, Green, Blue : Natural;
      end record;

      Clay_Color : constant Rgb := (Red => 129, Green => 63, Blue => 11);
      Sand_Color : constant Rgb := (Red => 244, Green => 164, Blue => 96);
      Water_Color : constant Rgb := (Red => 102, Green => 205, Blue => 170);

      Min_X_Water : Natural := Min_X;

   begin

      for P of Water loop
         Min_X_Water := Natural'Min (P.X, Min_X_Water);
      end loop;

      Min_X_Water := @ - 2;

      for Idx in 2 .. File_Number'Length loop
         Name (6 - File_Number'Length + Idx) := File_Number (Idx);
      end loop;

      IO.Create (F, Name => "Images/Frame_" & Name & ".ppm");
      IO.Put (F, "P3");
      IO.Put (F, Positive'Image (3 * (Max_X - Min_X_Water + 1)));
      IO.Put (F, Positive'Image (3 * (Max_Y - Min_Y + 1)));
      IO.Put (F, " 255"); -- max color
      IO.New_Line (F);

      for Y in Min_Y .. Max_Y loop
         for Each_Y in 1 .. 3 loop
            declare
               Idx : Positive := 1;
               Row : X_Vectors.Vector renames Bounds (Y);
            begin
               for X in Min_X_Water .. Max_X loop
                  if Idx <= Natural (Row.Last_Index) and then Row (Idx) = X
                  then
                     for Each_X in 1 .. 3 loop
                        IO.Put_Line (F,
                           Clay_Color.Red'Image
                           & Clay_Color.Blue'Image
                           & Clay_Color.Green'Image
                        );
                     end loop;
                     Idx := @ + 1;

                  elsif Water.Contains ((X, Y)) then
                     for Each_X in 1 .. 3 loop
                        IO.Put_Line (F,
                           Water_Color.Red'Image
                           & Water_Color.Blue'Image
                           & Water_Color.Green'Image
                        );
                     end loop;
                  else
                     for Each_X in 1 .. 3 loop
                        IO.Put_Line (F,
                           Sand_Color.Red'Image
                           & Sand_Color.Green'Image
                           & Sand_Color.Blue'Image
                        );
                     end loop;
                  end if;
               end loop;
               IO.New_Line (F);
            end;
         end loop;
         IO.New_Line (F);
      end loop;

      IO.Close (F);

   end Write_Soil_To_File;

   --  SECTION
   --  Part 1

   --  Breadth-First Search where exploration can only happen in 3 directions
   --  The queue is `Fallers`, which contains locations where water falls
   --  Once it strikes a block, it spreads out to find new places to fall
   --  The trick part is how to make it rise

   --  SUBSECTION
   --  The queue

   package Falling_Queue_Interface is
      new Ada.Containers.Synchronized_Queue_Interfaces (
         Element_Type => Position
      );
   package Position_Queues is
      new Ada.Containers.Unbounded_Synchronized_Queues (
         Queue_Interfaces => Falling_Queue_Interface
      );

   Fallers : Position_Queues.Queue;

   --  SUBSECTION
   --  Important test functions

   function Is_Empty (P : Position) return Boolean is
   --  true iff P holds neither clay nor water
      (not (Bounds (P.Y).Contains (P.X) or else Water.Contains (P)));

   function Can_Fall (P : Position) return Boolean is
   --  true iff P is empty, the position below P is empty,
   --  or else water has already fallen here
   --  (the third test prevents multiple streams from building up
   --  on each other once they reach the top of a pit)
   (
      Is_Empty (P)
      and then (
         Is_Empty ((P.X, P.Y + 1))
         or else Streams.Contains ((P.X, P.Y + 1))
      )
   );

   function Blocked_Water_At (P : Position) return Boolean is
   --  true iff water spreading out from P would eventually hit clay

      Left : Natural := P.X - 1;
      Right : Natural := P.X + 1;
      Y : constant Natural := P.Y;

   begin

      while Water.Contains ((Left, Y)) loop
         Left := @ - 1;
      end loop;

      while Water.Contains ((Right, Y)) loop
         Right := @ + 1;
      end loop;

      return Bounds (Y).Contains (Left) and then Bounds (Y).Contains (Right);

   end Blocked_Water_At;

   procedure Spread_From (P : Position) is
   --  spreads left and right from P, rising if it hits a wall

      Y : Natural := P.Y;
      Left, Right : Natural;

      Rising_Left : Boolean := True;   -- whether we fall left
      Rising_Right : Boolean := True;  -- whether we fall right

      Debug : constant Boolean := False;

   begin

      while Rising_Left and then Rising_Right loop
      --  as long as the left or right side is still rising,
      --  we need to check for spread

         Left := P.X;
         Right := P.X;

         if not Water.Contains ((P.X, Y)) then
            --  we may rise above the stream that created this spread,
            --  which still not risigin above a parent stream
            Water.Insert ((P.X, Y));
         end if;

         if Debug then
            IO.Put_Line ("Spreading left at" & Y'Image);
         end if;

         --  spread left until we hit a wall or we can fall
         while Rising_Left and then not Bounds (Y).Contains (Left) loop

            if not Water.Contains ((Left, Y)) then
               Water.Insert ((Left, Y));
            end if;

            if Can_Fall ((Left - 1, Y + 1)) then
               Fallers.Enqueue ((Left - 1, Y));
               Rising_Left := False;
               if Debug then
                  IO.Put_Line (
                     "Falls at" & Natural'Image (Left - 1) & Y'Image
                  );
               end if;

            else
               Left := @ - 1;

            end if;

         end loop;

         if Debug then
            IO.Put_Line ("Spreading right at" & Y'Image);
         end if;

         while Rising_Right and then not Bounds (Y).Contains (Right) loop

            if not Water.Contains ((Right, Y)) then
               Water.Insert ((Right, Y));
            end if;

            if Can_Fall ((Right + 1, Y + 1)) then
               Fallers.Enqueue ((Right + 1, Y));
               Rising_Right := False;
               if Debug then
                  IO.Put_Line (
                     "Falls at" & Natural'Image (Right + 1) & Y'Image
                  );
               end if;

            else
               Right := @ + 1;

            end if;

         end loop;

         if Debug then
            Draw_Soil (809, 823);
         end if;

         Y := @ - 1;

      end loop;

   end Spread_From;

   procedure Fall is
   --  take the next planned fall element and if it can fall then do so;
   --  then check for further falls; if blocked by clay, then spread

      P : Position;

   begin

      Fallers.Dequeue (P);

      if Is_Empty (P) then

         Water.Insert (P);
         Streams.Insert (P);

         if P.Y + 1 <= Max_Y then

            if Is_Empty ((P.X, P.Y + 1)) then
               if not Streams.Contains ((P.X, P.Y + 1)) then
                  Fallers.Enqueue ((P.X, P.Y + 1));
               end if;

            else
               if Bounds (P.Y + 1).Contains (P.X)
                  or else Blocked_Water_At ((P.X, P.Y + 1))
               then
                  Spread_From (P);
               end if;

            end if;

         end if;

      end if;

   end Fall;

   function Part1 return Natural is
      Result : Natural := 0;
   begin

      Fallers.Enqueue ((500, 1));

      while Fallers.Current_Use > 0 loop
         Fall;
      end loop;

      Result := Natural (Water.Length);

      --  i don't like the following way of pruning
      --  but i don't feel like coming up with a better one at the moment
      for P of Water loop
         if P.Y in 1 .. Min_Y - 1 then
            Result := @ - 1;
         end if;
      end loop;

      return Result;

   end Part1;

   --  SECTION
   --  Part 2 (much shorter)

   function Part2 return Natural is
      Retained_Water : Position_Sets.Set;
   begin

      while not Water.Is_Empty loop

         declare
            P : Position;
            Left, Right : Natural;
         begin

            declare
               C : constant Position_Sets.Cursor := Water.First;
            begin
               P := Water (C);
               Left := P.X - 1;
               Right := P.X + 1;
            end;

            while not Bounds (P.Y).Contains (Left)
               and then Water.Contains ((Left, P.Y))
            loop
               Left := @ - 1;
            end loop;

            while not Bounds (P.Y).Contains (Right)
               and then Water.Contains ((Right, P.Y))
            loop
               Right := @ + 1;
            end loop;

            if Bounds (P.Y).Contains (Left)
               and then Bounds (P.Y).Contains (Right)
            then
               for X in Left + 1 .. Right - 1 loop
                  Retained_Water.Insert ((X, P.Y));
               end loop;
            end if;

            for X in Left + 1 .. Right - 1 loop
               Water.Delete ((X, P.Y));
            end loop;

         end;

      end loop;

      Water := Retained_Water;
      return Natural (Water.Length);

   end Part2;

begin

   Read_Input;
   --  Draw_Soil;

   declare
      Water_Cells : constant Natural := Part1;
   begin
      --  Draw_Soil;
      IO.Put_Line ("There are" & Water_Cells'Image & " cells of water");
   end;
   Write_Soil_To_File;

   IO.Put_Line ("There are" & Part2'Image & " cells retained");
   --  Draw_Soil;

end Day17;
