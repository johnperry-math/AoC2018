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
with Ada.Containers.Ordered_Sets;

--  glpk
with glpk_h;
with Interfaces.C;
with Interfaces.C.Strings;

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

   function In_Range (Source : Nanobot; Target : Position) return Boolean is
   (
      Distance (Source.Pos, Target) <= Source.Radius
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
      IO.Put ("; ");
      Int_IO.Put (N.Radius, 0);
      IO.Put (")");
   end Put_Nanobot;

   --  SECTION
   --  Part 1

   function Nanobots_In_Range_Of (Source : Nanobot) return Natural is
      Result : Natural := 0;
   begin

      for N of All_Nanobots loop
         if In_Range (Source, N.Pos) then
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

            IO.Put_Line (
               "Accepted "
               & "[" & Best.Pos.X'Image & "," & Best.Pos.Y'Image & ","
               & Best.Pos.Z'Image & " ;" & Resolution'Image & "] :"
               & Best.Count'Image
            );

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
   --  it finds the correct result twice in this process:
   --  the first time on iteration 14, the second on iteration 19
   --  of the two, only iteration 19 has resolution 1, so it terminates then

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

   --  SECTION
   --  Part 2, Take 2

   --  SUBSECTION
   --  Balls
   --
   --  a "Ball" is a neighborhood defined by the "max" norm: that is,
   --  the distance between P and Q is determined by the maximum difference
   --  between corresponding components
   --
   --  for example: if P = (10, 10, 10) and Q = (10, 5, -5) then
   --  the distance between P and Q is 15, since the z component
   --  has the largest difference

   Origin : constant Position := (0, 0, 0);

   type Ball is record
      Center : Position;
      Radius : Natural;
      Count : Natural;
   end record;
   --  neighborhood of given Radius around Center
   --  Count is the number of Nanobots for which a point of Ball is in range

   procedure Put_Ball (B : Ball) is
   begin
      IO.Put (
         "[ (" & B.Center.X'Image
         & " ," & B.Center.Y'Image
         & " ," & B.Center.Z'Image
         & ")," & B.Radius'Image & "] :"
         & B.Count'Image
      );
   end Put_Ball;

   function Preferred (One, Tother : Ball) return Ball is
   --  returns the ball whose count is higher, breaking ties by
   --  shorter distance to origin
   (
      if One.Count > Tother.Count then One
      elsif One.Count < Tother.Count then Tother
      elsif Distance (One.Center, Origin) <= Distance (Tother.Center, Origin)
         then One
      else Tother
   );

   function First_Ball return Ball is
   --  generates a first ball for the search algorithm
   --  based on the max and min values of the coordinates

      Min_X, Min_Y, Min_Z : Integer := Integer'Last;
      Max_X, Max_Y, Max_Z : Integer := Integer'First;

      Midpoint : Position;
      Radius : Natural := 0;

   begin

      for N of All_Nanobots loop
         Min_X := Integer'Min (Min_X, N.Pos.X - N.Radius);
         Min_Y := Integer'Min (Min_Y, N.Pos.Y - N.Radius);
         Min_Z := Integer'Min (Min_Z, N.Pos.Z - N.Radius);
         Max_X := Integer'Max (Max_X, N.Pos.X + N.Radius);
         Max_Y := Integer'Max (Max_Y, N.Pos.Y + N.Radius);
         Max_Z := Integer'Max (Max_Z, N.Pos.Z + N.Radius);
      end loop;

      Radius := Natural'Max (Radius * 2, Max_X - Min_X) / 2;
      Radius := Natural'Max (Radius * 2, Max_Y - Min_Y) / 2;
      Radius := Natural'Max (Radius * 2, Max_Z - Min_Z) / 2;
      if Radius mod 2 = 1 then   --  ensure an even radius
         Radius := @ + 1;
      end if;

      Midpoint := (
         X => Min_X + Radius,
         Y => Min_Y + Radius,
         Z => Min_Z + Radius
      );

      return (
         Center => Midpoint,
         Radius => Radius,
         Count => 0
      );

   end First_Ball;

   --  SUBSECTION
   --  algorithm

   --  SUBSUBSECTION
   --  intersection

   function Intersect (B : Ball; N : Nanobot) return Boolean is
   --  true iff the cube neighborhood around B intersects
   --  the octohedron neighborhood around N
   --
   --  the technique here is to use glpk, a linear solver,
   --  to solve the relevant system of linear inequalities,
   --  which are determined by the planes defining the faces of the polyhedra
   --
   --  see comments throughout for details

      package C renames Interfaces.C;
      package Glpk renames glpk_h;
      use type C.double;

      Problem : access Glpk.glp_prob;        --  linear program
      Parameters : aliased Glpk.glp_smcp;    --  used to quiet glpk

      Space : constant Positive := 1001;     --  need extra space for solving

      type Index_Array is array (1 .. Space) of aliased C.int;
      --  row and column indices to give sparse definition of linear program
      type Value_Array is array (1 .. Space) of aliased C.double;
      --  values of coefficients

      Stuff_Added, Error_Code : C.int;
      --  return values we don't need, but glpk returns, so Ada won't ignore

      --  Row_Indices, Col_Indices, and Coeffs below specify the linear program
      --
      --  we chose to approach this by using only the planes from the nanobots;
      --  the planes for the balls can be used as bounds on the variables:
      --  for the ball centered at (a, b, c) the points in range are
      --
      --     | x - a | <= r , | y - b | <= r , | z - c | <= r
      --
      --  and by considering each of the two cases for each absolute value,
      --  we have
      --
      --     a - r <= x <= a + r , b - r <= y <= b + r , c - r <= z <= c + r
      --
      --  these are clearly constraints on the variables,
      --  which gplk allows us to specify as constraints on the columns

      Ball_UB_X : constant C.double := C.double (B.Center.X + B.Radius);
      Ball_LB_X : constant C.double := C.double (B.Center.X - B.Radius);
      Ball_UB_Y : constant C.double := C.double (B.Center.Y + B.Radius);
      Ball_LB_Y : constant C.double := C.double (B.Center.Y - B.Radius);
      Ball_UB_Z : constant C.double := C.double (B.Center.Z + B.Radius);
      Ball_LB_Z : constant C.double := C.double (B.Center.Z - B.Radius);

      --  for the nanobot with radius r centered at (a, b, c),
      --  the points in range are
      --
      --     | x - a | + | y - b | + | z - c | <= r
      --
      --  this isn't linear, but can be made into a linear program
      --  by considering each of the two cases for each absolute value:
      --
      --     ( x - a ) + ( y - b ) + ( z - c ) <= r
      --     ( x - a ) + ( y - b ) - ( z - c ) <= r
      --     ( x - a ) - ( y - b ) + ( z - c ) <= r
      --
      --  ...etc. these translate into
      --
      --     x + y + z <= r + a + b + c
      --     x + y - z <= r + a + b - c
      --     x - y + z <= r + a - b + c
      --
      --  ...etc.

      Row_Indices : Index_Array := [
         0,
         1, 1, 1,    --  x + y + z in 1st row
         2, 2, 2,    --  x + y - z in 2nd row
         3, 3, 3,    --  x - y + z in 3rd row
         4, 4, 4,    --  etc.
         5, 5, 5,
         6, 6, 6,
         7, 7, 7,
         8, 8, 8,
         others => 0
      ];

      Col_Indices : Index_Array := [
         0,
         1, 2, 3,    --  x in 1st col, y in 2nd col, z in 3rd col, etc.
         1, 2, 3,
         1, 2, 3,
         1, 2, 3,
         1, 2, 3,
         1, 2, 3,
         1, 2, 3,
         1, 2, 3,
         others => 0
      ];

      Coeffs : Value_Array := [
         0.0,
          1.0,  1.0,  1.0,    --  x + y + z in 1st row
          1.0,  1.0, -1.0,    --  x + y - z in 2nd row
          1.0, -1.0,  1.0,    --  x - y + z in 3rd row
          1.0, -1.0, -1.0,    --  etc.
         -1.0,  1.0,  1.0,
         -1.0,  1.0, -1.0,
         -1.0, -1.0,  1.0,
         -1.0, -1.0, -1.0,
         others => 0.0
      ];

      Bot_Radius : constant C.double := C.double (N.Radius);   -- r
      Bot_X : constant C.double := C.double (N.Pos.X);         -- a
      Bot_Y : constant C.double := C.double (N.Pos.Y);         -- b
      Bot_Z : constant C.double := C.double (N.Pos.Z);         -- c

   begin

      --  quiet the solver
      Glpk.glp_init_smcp (Parameters'Access);
      Parameters.msg_lev := Glpk.GLP_MSG_OFF;

      --  set up space
      Problem := Glpk.glp_create_prob;

      --  define the linear program's rows
      Stuff_Added := Glpk.glp_add_rows (Problem, 8);

      Glpk.glp_set_row_bnds (Problem, 1, Glpk.GLP_UP, 0.0,
         Bot_Radius + Bot_X + Bot_Y + Bot_Z);  --  ... <= r + a + b + c
      Glpk.glp_set_row_bnds (Problem, 2, Glpk.GLP_UP, 0.0,
         Bot_Radius + Bot_X + Bot_Y - Bot_Z);  --  ... <= r + a + b - c
      Glpk.glp_set_row_bnds (Problem, 3, Glpk.GLP_UP, 0.0,
         Bot_Radius + Bot_X - Bot_Y + Bot_Z);  --  ... <= r + a - b + c
      Glpk.glp_set_row_bnds (Problem, 4, Glpk.GLP_UP, 0.0,
         Bot_Radius + Bot_X - Bot_Y - Bot_Z);  --  etc.
      Glpk.glp_set_row_bnds (Problem, 5, Glpk.GLP_UP, 0.0,
         Bot_Radius - Bot_X + Bot_Y + Bot_Z);
      Glpk.glp_set_row_bnds (Problem, 6, Glpk.GLP_UP, 0.0,
         Bot_Radius - Bot_X + Bot_Y - Bot_Z);
      Glpk.glp_set_row_bnds (Problem, 7, Glpk.GLP_UP, 0.0,
         Bot_Radius - Bot_X - Bot_Y + Bot_Z);
      Glpk.glp_set_row_bnds (Problem, 8, Glpk.GLP_UP, 0.0,
         Bot_Radius - Bot_X - Bot_Y - Bot_Z);

      --  define the linear program's columns

      Stuff_Added := Glpk.glp_add_cols (Problem, 3);

      --  we want **integer** solutions
      Glpk.glp_set_col_kind (Problem, 1, Glpk.GLP_IV);
      Glpk.glp_set_col_kind (Problem, 2, Glpk.GLP_IV);
      Glpk.glp_set_col_kind (Problem, 3, Glpk.GLP_IV);

      Glpk.glp_set_col_name (Problem, 1,
         C.Strings.New_Char_Array (C.To_C ("x"))
      );
      Glpk.glp_set_col_name (Problem, 2,
         C.Strings.New_Char_Array (C.To_C ("y"))
      );
      Glpk.glp_set_col_name (Problem, 3,
         C.Strings.New_Char_Array (C.To_C ("z"))
      );

      --  restrict values to Ball's limits
      Glpk.glp_set_col_bnds (Problem, 1, Glpk.GLP_DB, Ball_LB_X, Ball_UB_X);
      Glpk.glp_set_col_bnds (Problem, 2, Glpk.GLP_DB, Ball_LB_Y, Ball_UB_Y);
      Glpk.glp_set_col_bnds (Problem, 3, Glpk.GLP_DB, Ball_LB_Z, Ball_UB_Z);

      --  we seek to minimize x + y + z
      Glpk.glp_set_obj_dir (Problem, Glpk.GLP_MIN);
      Glpk.glp_set_obj_coef (Problem, 1, 1.0);
      Glpk.glp_set_obj_coef (Problem, 2, 1.0);
      Glpk.glp_set_obj_coef (Problem, 3, 1.0);

      --  read in coefficients
      Glpk.glp_load_matrix (
         Problem,
         24,
         Row_Indices (1)'Access,
         Col_Indices (1)'Access,
         Coeffs (1)'Access
      );

      --  solve it!
      Error_Code := Glpk.glp_simplex (Problem, Parameters'Access);
      --  make sure the solution is valid
      Error_Code := Glpk.glp_get_status (Problem);
      --  clean up
      Glpk.glp_delete_prob (Problem);

      --  GLP_OPT means "optimal solution was found"
      return (if Integer (Error_Code) = Glpk.GLP_OPT then True else False);

   end Intersect;

   --  SUBSUBSECTION
   --  subdivision of balls

   type Directions is (
      Left_Back_Down,
      Left_Back_Up,
      Left_Fore_Down,
      Left_Fore_Up,
      Right_Back_Down,
      Right_Back_Up,
      Right_Fore_Down,
      Right_Fore_Up
   );

   Max_Offsets : constant array (Directions) of Position := [
      (-1, -1, -1),
      (-1, -1,  1),
      (-1,  1, -1),
      (-1,  1,  1),
      ( 1, -1, -1),
      ( 1, -1,  1),
      ( 1,  1, -1),
      ( 1,  1,  1)
   ];
   --  used to compute Ball corners

   type Eight_Balls is array (Directions) of Ball;
   --  we store the split cube here
   --  there are 8 b/c we have 4 on top and 4 on bottom:
   --      ______
   --     |\  \  \
   --     | \__\__\
   --     | |\  \  \
   --     |\| \__\__\
   --     | \ |  |  |
   --     | |\|  |  |
   --      \| |--+--|
   --       \ |  |  |
   --        \|__|__|

   procedure Split (B : Ball; Balls : out Eight_Balls) is
   --  splits the cube as illustrated above

      New_Radius : Natural := B.Radius / 2 + (B.Radius mod 2);
      --  designed to make sure the new radius is AT LEAST HALF the old radius:
      --  10 -> 5 -> 3 -> 2 -> 1 -> 1 -> 1 -> ...
      --   9 -> 5 -> 3 -> 2 -> 1 -> 1 -> 1 -> ...
      --   8 -> 4 -> 2 -> 1 -> 1 -> 1 -> ...
      --   7 -> 4 -> 2 -> 1 -> 1 -> 1 -> ...

   begin

      --  ...but we want an even radius, so that the center will be an integer
      --  force this
      if B.Radius > 2 and then New_Radius mod 2 = 1 then
         New_Radius := @ + 1;
      end if;

      for Dir in Directions loop
         Balls (Dir) := (
            Center => (
               B.Center.X + Max_Offsets (Dir).X * New_Radius,
               B.Center.Y + Max_Offsets (Dir).Y * New_Radius,
               B.Center.Z + Max_Offsets (Dir).Z * New_Radius
            ),
            Radius => New_Radius,
            Count => 0
         );
      end loop;

   end Split;

   --  SUBSUBSECTION
   --  algorithm proper

   procedure Score_Ball (B : in out Ball) is
   --  sets B.Count according to nanobots in range
   begin

      B.Count := 0;
      for N of All_Nanobots loop
         if Intersect (B, N) then
            B.Count := @ + 1;
         end if;
      end loop;

   end Score_Ball;

   function Divide_And_Conquer return Ball is
   --  starting from First_Ball:
   --  loop
   --     select a ball
   --     if ball's radius is 1, save for below and pick another of equal count
   --     split into 8
   --     count nanobots in range
   --  from all balls of radius 1, which will have equal count (see above),
   --     pick the one closest to Origin

      Winner : Ball := ((0, 0, 0), 0, 0);
      Old : Ball := First_Ball;

      Balls : Eight_Balls;

      Winning_Value : Natural := 0;

      --  IMPLEMENTATION DETAILS
      --  we store a vector of sets of balls
      --  a ball B is stored in the set at index B.Count
      --
      --  first we have to set up the sets
      function "<" (Left, Right : Ball) return Boolean is
      (
         Left.Center.X < Right.Center.X or else
         (
            Left.Center.X = Right.Center.X and then
               Left.Center.Y < Right.Center.Y
         ) or else
         (
            Left.Center.X = Right.Center.X and then
               Left.Center.Y = Right.Center.Y and then
               Left.Center.Z < Right.Center.Z
         )
      );

      package Ball_Sets is new Ada.Containers.Ordered_Sets (
         Element_Type => Ball
      );

      Winners : Ball_Sets.Set;

      function "=" (Left, Right : Ball_Sets.Set) return Boolean is
      --  still don't know why vector needs to decide equality of elements
      (
         Natural (Left.Length) = Natural (Right.Length) and then (
            for all B of Left => Right.Contains (B)
         )
      );

      package Ball_Set_Vectors is new Ada.Containers.Vectors (
         Index_Type => Positive,
         Element_Type => Ball_Sets.Set
      );

      Ball_Scores : Ball_Set_Vectors.Vector;

   begin

      Ball_Scores.Set_Length (All_Nanobots.Length);

      Score_Ball (Old);
      Ball_Scores (Old.Count).Insert (Old);

      --  part 1: use binary search to find balls in range of most bots

      loop

         Old.Count := 0;   --  this ugly hack, which you'll see below,
                           --  is here to "reset" the ball to allow a search

         --  select ball in range of most bots
         for In_Range in reverse 1 .. Ball_Scores.Last_Index loop

            if In_Range >= Natural'Max (Winning_Value, Old.Count) and then
               not Ball_Scores (In_Range).Is_Empty
            then

               --  select ball from those with count In_Range
               loop

                  Old := Ball_Scores (In_Range).First_Element;

                  if Old.Radius = 1 and then In_Range >= Winning_Value then
                     --  save for later

                     Ball_Scores (In_Range).Delete_First;

                     if not Winners.Contains (Old) then

                        if In_Range > Winning_Value then
                           --  this should happen all of once
                           Winners.Clear;
                           Winning_Value := In_Range;
                        end if;

                        IO.Put ("Found winner! "); Put_Ball (Old); IO.New_Line;
                        Winners.Insert (Old);

                     end if;

                     Old.Count := 0;   --  reset to search

                  else
                     exit;
                  end if;

               end loop;

               exit;

            end if;

         end loop;

         if Old.Count = 0 then
            --  we failed to find any balls w/most bots in range; so,
            --  terminate the search
            exit;
         end if;

         Ball_Scores (Old.Count).Delete_First;  --  remove selection from queue

         if Winning_Value > 0 then
            IO.Put_Line ("Winning value:" & Winning_Value'Image);
            IO.Put_Line (
               "Number of balls still at winning value:"
               & Ball_Scores (Winning_Value).Length'Image
            );
         end if;

         IO.Put ("Current ball: ");
         Put_Ball (Old);
         IO.New_Line;

         Split (Old, Balls);

         for B of Balls loop
            Score_Ball (B);
            if B.Count > 0 and then not Ball_Scores(B.Count).Contains (B) then
               Ball_Scores (B.Count).Insert (B);
            end if;
         end loop;

      end loop;

      IO.Put_Line ("Number of winners:" & Winners.Length'Image);

      for W of Winners loop

         IO.Put ("   ");
         Put_Ball (W);
         IO.Put_Line (Distance (W.Center, Origin)'Image);

         --  check every point in this neighborhood
         --  (a point is a ball of radius 0)
         for Sx in -1 .. 1 loop
            for Sy in -1 .. 1 loop
               for Sz in -1 .. 1 loop

                  declare
                     B : Ball := (
                        Center => (
                           X => W.Center.X + Sx,
                           Y => W.Center.Y + Sy,
                           Z => W.Center.Z + Sz
                        ),
                        Radius => 0,
                        Count => 0
                     );
                  begin

                     for N of All_Nanobots loop
                        if In_Range (N, B.Center) then
                           B.Count := @ + 1;
                        end if;
                     end loop;
                     Winner := Preferred (Winner, B);

                  end;

               end loop;
            end loop;
         end loop;
      end loop;

      IO.Put ("Winner: "); Put_Ball (Winner); IO.New_Line;

      return Winner;

   end Divide_And_Conquer;

begin

   Read_Input;

   IO.Put_Line (
      "In range of strongest:"
      & Nanobots_In_Range_Of_Strongest'Image
   );

   IO.Put_Line (
      "Best distance with most points, algorithm 1:"
      & Best_Points_Distance'Image
   );
   IO.Put_Line (
      "Best distance ewith most points, algorithm 2:"
      & Natural'Image (Distance (Divide_And_Conquer.Center, Origin))
   );

end Day23;
