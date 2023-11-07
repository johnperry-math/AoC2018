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
               IO.Put_Line ("Found"); IO.New_Line;
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
                  IO.Put_Line ("Found"); IO.New_Line;
                  return Matches;
               end if;

            end if;

         end;

      end loop;

      IO.Put_Line ("Mot Found"); IO.New_Line;
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

   type Ball is record
      Center : Position;
      Radius : Natural;
      Count : Natural;
   end record;

   Origin : Position := (0, 0, 0);

   Max_Offsets : array (1 .. 8) of Position := [
      (-1, -1, -1),
      (-1, -1,  1),
      (-1,  1, -1),
      (-1,  1,  1),
      ( 1, -1, -1),
      ( 1, -1,  1),
      ( 1,  1, -1),
      ( 1,  1,  1)
   ];

   function Distance (B : Ball; P : Position) return Natural is
      Result : Natural := Natural'Last;
   begin
      for D of Max_Offsets loop
         Result := Natural'Min (
            Result,
            Natural'Max (
               Natural'Max (
                  abs (B.Center.X - P.X),
                  abs (B.Center.Y - P.Y)
               ),
               abs (B.Center.Z - P.Z)
            )
         );
      end loop;
      return Result;
   end Distance;

   function Contains (B : Ball; P : Position) return Boolean is
      (Distance (B, P) <= B.Radius);

   function Preferred (One, Tother : Ball) return Ball is
   (
      if One.Count > Tother.Count then One
      elsif One.Count < Tother.Count then Tother
      elsif Distance (One, Origin) <= Distance (Tother, Origin) then One
      else Tother
   );

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

   type Dxyz is record
      X, Y, Z : Integer;
   end record;

   Offsets : constant array (1 .. 6) of Dxyz := [
      (-1, 0, 0), (1, 0, 0),
      (0, -1, 0), (0, 1, 0),
      (0, 0, -1), (0, 0, 1)
   ];

   function Intersect (N : Nanobot; B : Ball) return Boolean is
   (
      Distance (B, N.Pos) <= B.Radius or else
      (
         for some D of Offsets =>
            Distance (
               B,
               (
                  N.Pos.X + D.X * N.Radius,
                  N.Pos.Y + D.Y * N.Radius,
                  N.Pos.Z + D.Z * N.Radius
               )
            ) <= B.Radius
      )
       or else (
         for some D of Max_Offsets =>
            Distance (
               N.Pos,
               (
                  B.Center.X + D.X * B.Radius,
                  B.Center.Y + D.Y * B.Radius,
                  B.Center.Z + D.Z * B.Radius
               )
            ) <= N.Radius
      )
   );
   
   --  function Alternate_Intersect (B : Ball; N : Nanobot) return Boolean is
   --     Dx, Dy, Dz : Integer;
   --     Sx, Sy, Sz : Integer;
   --     P : Position;
   --  begin
   --     Dx := N.Pos.X - B.Center.X;
   --     Dy := N.Pos.Y - B.Center.Y;
   --     Dz := N.Pos.Z - B.Center.Z;
   --     Sx := Natural'Min (B.Radius, abs (Dx));
   --     Sy := Natural'Min (B.Radius, abs (Dy));
   --     Sz := Natural'Min (B.Radius, abs (Dz));
   --     if Dx < 0 then Sx := -Sx; end if;
   --     if Dy < 0 then Sy := -Sy; end if;
   --     if Dz < 0 then Sz := -Sz; end if;
   --     P := (B.Center.X + Sx, B.Center.Y + Sy, B.Center.Z + Sz);
   --     return In_Range (N, P);
   --  end Alternate_Intersect;

   function Alternate_Intersect (B : Ball; N : Nanobot) return Boolean is

      package C renames Interfaces.C;
      package Glpk renames glpk_h;
      use type C.double;

      Problem : access Glpk.glp_prob;
      Parameters : aliased Glpk.glp_smcp;

      Space : constant Positive := 1001;
      type Index_Array is array (1 .. Space) of aliased C.int;
      type Value_Array is array (1 .. Space) of aliased C.double;
      
      Stuff_Added, Error_Code : C.int;

      Row_Indices : Index_Array := [
         0,
         1, 1, 1,    -- sum ( | xi - si | ) <= Rj
         2, 2, 2,
         3, 3, 3,
         4, 4, 4,
         5, 5, 5,
         6, 6, 6,
         7, 7, 7,
         8, 8, 8,
         others => 0
      ];
      Col_Indices : Index_Array := [
         0,
         1, 2, 3,    -- sum ( | xi - si | ) <= Rj
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
          1.0,  1.0,  1.0,
          1.0,  1.0, -1.0,
          1.0, -1.0,  1.0,
          1.0, -1.0, -1.0,
         -1.0,  1.0,  1.0,
         -1.0,  1.0, -1.0,
         -1.0, -1.0,  1.0,
         -1.0, -1.0, -1.0,
         others => 0.0
      ];

      Ball_UB_X : C.double := C.double (B.Center.X + B.Radius);
      Ball_LB_X : C.double := C.double (B.Center.X - B.Radius);
      Ball_UB_Y : C.double := C.double (B.Center.Y + B.Radius);
      Ball_LB_Y : C.double := C.double (B.Center.Y - B.Radius);
      Ball_UB_Z : C.double := C.double (B.Center.Z + B.Radius);
      Ball_LB_Z : C.double := C.double (B.Center.Z - B.Radius);

      Bot_Radius : C.double := C.double (N.Radius);
      Bot_X : C.double := C.double (N.Pos.X);
      Bot_Y : C.double := C.double (N.Pos.Y);
      Bot_Z : C.double := C.double (N.Pos.Z);

   begin
      Glpk.glp_init_smcp (Parameters'Access);
      Parameters.msg_lev := Glpk.GLP_MSG_OFF;

      Problem := Glpk.glp_create_prob;
      Glpk.glp_set_obj_dir (Problem, Glpk.GLP_MIN);

      Stuff_Added := Glpk.glp_add_rows (Problem, 8);

      Glpk.glp_set_row_bnds (Problem, 1, Glpk.GLP_UP, 0.0,
         Bot_Radius + Bot_X + Bot_Y + Bot_Z);
      Glpk.glp_set_row_bnds (Problem, 2, Glpk.GLP_UP, 0.0, 
         Bot_Radius + Bot_X + Bot_Y - Bot_Z);
      Glpk.glp_set_row_bnds (Problem, 3, Glpk.GLP_UP, 0.0, 
         Bot_Radius + Bot_X - Bot_Y + Bot_Z);
      Glpk.glp_set_row_bnds (Problem, 4, Glpk.GLP_UP, 0.0, 
         Bot_Radius + Bot_X - Bot_Y - Bot_Z);
      Glpk.glp_set_row_bnds (Problem, 5, Glpk.GLP_UP, 0.0, 
         Bot_Radius - Bot_X + Bot_Y + Bot_Z);
      Glpk.glp_set_row_bnds (Problem, 6, Glpk.GLP_UP, 0.0, 
         Bot_Radius - Bot_X + Bot_Y - Bot_Z);
      Glpk.glp_set_row_bnds (Problem, 7, Glpk.GLP_UP, 0.0, 
         Bot_Radius - Bot_X - Bot_Y + Bot_Z);
      Glpk.glp_set_row_bnds (Problem, 8, Glpk.GLP_UP, 0.0, 
         Bot_Radius - Bot_X - Bot_Y - Bot_Z);

      Stuff_Added := Glpk.glp_add_cols (Problem, 3);
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

      Glpk.glp_set_col_bnds (Problem, 1, Glpk.GLP_DB, Ball_LB_X, Ball_UB_X);
      Glpk.glp_set_col_bnds (Problem, 2, Glpk.GLP_DB, Ball_LB_Y, Ball_UB_Y);
      Glpk.glp_set_col_bnds (Problem, 3, Glpk.GLP_DB, Ball_LB_Z, Ball_UB_Z);

      Glpk.glp_set_obj_coef (Problem, 1, 1.0);
      Glpk.glp_set_obj_coef (Problem, 2, 1.0);
      Glpk.glp_set_obj_coef (Problem, 3, 1.0);

      Glpk.glp_load_matrix (Problem, 24, Row_Indices (1)'Access, Col_Indices (1)'Access, Coeffs (1)'Access);
      --  Error_Code := Glpk.glp_write_lp (Problem, null, C.Strings.New_Char_Array (C.To_C ("problem.cplex")));
      Error_Code := Glpk.glp_simplex (Problem, Parameters'Access);
      Error_Code := Glpk.glp_get_status (Problem);
      Glpk.glp_delete_prob (Problem);
      return (if Integer (Error_Code) = Glpk.GLP_OPT then True else False);
   end Alternate_Intersect;

   type Eight_Balls is array (1 .. 8) of Ball;

   procedure Split (B : Ball; Balls : out Eight_Balls) is
      New_Radius : Natural := B.Radius / 2 + (B.Radius mod 2);
   begin
      if B.Radius > 2 and then New_Radius mod 2 = 1 then
         New_Radius := @ + 1;
      end if;
      for Idx in 1 .. 8 loop
         Balls (Idx) := (
            Center => (
               B.Center.X + Max_Offsets (Idx).X * New_Radius,
               B.Center.Y + Max_Offsets (Idx).Y * New_Radius,
               B.Center.Z + Max_Offsets (Idx).Z * New_Radius
            ),
            Radius => New_Radius,
            Count => 0
         );
      end loop;
   end Split;

   function First_Ball return Ball is
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
      if Radius mod 2 = 1 then
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

   procedure Score_Ball (B : in out Ball) is
   begin
      B.Count := 0;
      for N of All_Nanobots loop
         if Alternate_Intersect (B, N) then
            B.Count := @ + 1;
         end if;
      end loop;
   end Score_Ball;

   function Divide_And_Conquer return Ball is
      Old : Ball := First_Ball;
      Balls : Eight_Balls;
      Winner : Ball := ((0, 0, 0), 0, 0);

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
      function "=" (Left, Right : Ball_Sets.Set) return Boolean is
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
      Score : Natural;
      use type Ada.Containers.Count_Type;
      Winning_Value : Natural := 0;
      Winners : Ball_Sets.Set;
   begin
      Ball_Scores.Set_Length (All_Nanobots.Length);
      Score_Ball (Old);
      IO.Put_Line (Old.Count'Image);
      Ball_Scores (Old.Count).Insert (Old);
      loop
         Old.Count := 0;
         for I in reverse 1 .. Ball_Scores.Last_Index loop
            if I >= Natural'Max (Winning_Value, Old.Count) and then
               not Ball_Scores (I).Is_Empty
            then
               loop
                  Old := Ball_Scores (I).First_Element;
                  if Old.Radius = 1 and then I >= Winning_Value then
                     Ball_Scores (I).Delete_First;
                     if not Winners.Contains (Old) then
                        if I > Winning_Value then
                           Winners.Clear;
                           Winning_Value := I;
                        end if;
                        IO.Put ("Found winner! "); Put_Ball (Old); IO.New_Line;
                        Winners.Insert (Old);
                     end if;
                     Old.Count := 0;
                  else
                     exit;
                  end if;
               end loop;
               exit;
            end if;
         end loop;
         if Old.Count = 0 then
            exit;
         end if;
         Ball_Scores (Old.Count).Delete_First;
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
            --  IO.Put ("   Inserting ");
            --  Put_Ball (B);
            --  IO.New_Line;
            if B.Count > 0 and then not Ball_Scores(B.Count).Contains (B) then
               Ball_Scores (B.Count).Insert (B);
            end if;
         end loop;
      end loop;
      IO.Put_Line ("Number of winners:" & Winners.Length'Image);
      for W of Winners loop
         IO.Put ("   "); Put_Ball (W); IO.Put_Line (Distance (W.Center, Origin)'Image);
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
                     Num_In_Range : Natural := 0;
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
      IO.Put_Line (Winner'Image);
      return Winner;
   end Divide_And_Conquer;

   type Plane is record
      Nx, Ny, Nz : Integer;
      Ox, Oy, Oz : Integer;
   end record;

   type Plane_Array is array (1 .. 8) of Plane;

   function New_Plane (P, Q, R : Position) return Plane is
      Result : Plane;
      PQ, PR : Position;
   begin
      PQ := (Q.X - P.X, Q.Y - P.Y, Q.Z - P.Z);
      PR := (R.X - P.X, R.Y - P.Y, R.Z - P.Z);
      --  normal vector
      Result.Nx := PQ.Y * PR.Z - PQ.Z * PR.Y;
      Result.Ny := PQ.Z * PR.X - PQ.X * PR.Z;
      Result.Nz := PQ.X * PR.Y - PQ.Y * PR.X;
      Result.Ox := P.X;
      Result.Oy := P.Y;
      Result.Oz := P.Z;
      return Result;
   end New_Plane;

   function Verify (P: Plane; Pos : Position) return Boolean is
   (
      P.Nx * (Pos.X - P.Ox) + P.Ny * (Pos.Y - P.Oy) + P.Nz * (Pos.Z - P.Oz) = 0
   );

   function Planes (B : Ball) return Plane_Array is
      Result : Plane_Array;
      P, Q, R : Position;
      PQ, PR : Position;
   begin
      P := (B.Center.X, B.Center.Y, B.Center.Z + B.Radius);    --  top
      Q := (B.Center.X + B.Radius, B.Center.Y, B.Center.Z);
      R := (B.Center.X, B.Center.Y + B.Radius, B.Center.Z);
      Result (1) := New_Plane (P, Q, R);
      Q := (B.Center.X - B.Radius, B.Center.Y, B.Center.Z);
      Result (2) := New_Plane (P, Q, R);
      R := (B.Center.X, B.Center.Y - B.Radius, B.Center.Z);
      Result (3) := New_Plane (P, Q, R);
      Q := (B.Center.X + B.Radius, B.Center.Y, B.Center.Z);
      Result (4) := New_Plane (P, Q, R);
      P := (B.Center.X, B.Center.Y, B.Center.Z - B.Radius);    --  bottom
      Q := (B.Center.X + B.Radius, B.Center.Y, B.Center.Z);
      R := (B.Center.X, B.Center.Y + B.Radius, B.Center.Z);
      Result (5) := New_Plane (P, Q, R);
      Q := (B.Center.X - B.Radius, B.Center.Y, B.Center.Z);
      Result (6) := New_Plane (P, Q, R);
      R := (B.Center.X, B.Center.Y - B.Radius, B.Center.Z);
      Result (7) := New_Plane (P, Q, R);
      Q := (B.Center.X + B.Radius, B.Center.Y, B.Center.Z);
      Result (8) := New_Plane (P, Q, R);
      return Result;
   end Planes;

begin
   Read_Input;
   --  IO.Put_Line (
   --     "In range of strongest:"
   --     & Nanobots_In_Range_Of_Strongest'Image
   --  );
   --  IO.Put_Line (
   --     "Best distance with most points:"
   --     & Best_Points_Distance'Image
   --  );
   --  Split (((0, 0, 0), 9));
   --  for B of Balls loop
   --     Put_Ball (B);
   --     IO.New_Line;
   --  end loop;
   IO.Put_Line (Natural'Image (Distance (Divide_And_Conquer.Center, Origin)));
   --  declare
   --     Good_Bots : Nano_Vectors.Vector;
   --     Good_Pos : Position := (33141656, 47488217, 49740661);
   --     --  Good_Ball : Ball := ((35466720, 43052284, 52254792), 4455952, 0);
   --     Good_Ball : Ball := (( 31010768 , 38596332 , 21063132), 17823806, 0);
   --     Good_Bot : Nanobot := ((35530582, 84367399, 26424603), 62584532);
   --     Another_Good_Ball : Ball := ((13186962, 56420138, 3239326), 35647612, 0);
   --     Bad_Bot : Nanobot := ((110857676, 49213932, 43292716), 85889782);
   --     Happy_Bots : Natural := 0;
   --     Very_Bad_Bots : array (1 .. 33) of Nanobot := [
   --        ((135550995, 127900747, 8337871), 52178968),
   --        ((210356533, 50342748, 27786515), 74614378),
   --        ((62605893, 146012567, 145236167), 77460342),
   --        ((56075723, 200075610, 24249521), 53265918),
   --        ((-32390642, 96794130, -39631847), 79593175),
   --        ((220507970, 62351975, 37279625), 72254465),
   --        ((93282068, -55706088, -8590487), 84873166),
   --        ((31843203, 68102261, 81070363), 53242260),
   --        ((198312304, 78158173, 41935481), 65596769),
   --        ((143569659, 101340093, -10678689), 76082500),
   --        ((229689668, 51577859, 35278007), 94146993),
   --        ((-45289459, 105422502, -29571485), 62363878),
   --        ((180230940, 87030129, 63114795), 86087663),
   --        ((60993485, -116341471, 40308009), 76771665),
   --        ((173993689, 75998196, 6864438), 82454107),
   --        ((106399436, 125822410, -17915007), 93317974),
   --        ((171688348, 71550560, -11128933), 76227310),
   --        ((232141916, 49769257, 51216213), 90681159),
   --        ((191800777, 84088203, 28351140), 88172658),
   --        ((100697653, 99550410, -54486204), 71017066),
   --        ((52246652, -9240409, -81204088), 50966298),
   --        ((99383401, 146030304, 88409823), 73873960),
   --        ((45141391, -12526410, -81240507), 81990979),
   --        ((60787433, 120857878, -53218737), 51539653),
   --        ((109703044, 96876496, -27071358), 95644195),
   --        ((159630263, 64353283, -13718722), 75930614),
   --        ((216644213, 19856107, 38332409), 67921068),
   --        ((238700985, 51350135, 44639309), 95314466),
   --        ((227033046, 49429705, 35092569), 85997165),
   --        ((200070638, 16020550, 59141214), 50437420),
   --        ((31477636, 131890053, -78006680), 96992044),
   --        ((124193979, 106570642, 102045214), 83182121),
   --        ((-170534126, 62752335, 47882666), 65812186)
   --     ];
   --  begin
   --     for N of All_Nanobots loop
   --        if In_Range (N, Good_Pos) then
   --           Good_Bots.Append (N);
   --        end if;
   --     end loop;
   --     IO.Put_Line ("Bots in range of good position:" & Good_Bots.Length'Image);
   --     for N of All_Nanobots loop
   --        if not Alternate_Intersect (Good_Ball, N) then
   --        --  if not Intersect (N, Good_Ball) then
   --        --  if not Intersect (N, Another_Good_Ball) then
   --           IO.Put ("unhappy: "); Put_Nanobot (N); IO.New_Line;
   --           null;
   --        else
   --           --  Put_Nanobot (N); IO.New_Line;
   --           Happy_Bots := @ + 1;
   --        end if;
   --     end loop;
   --     IO.Put_Line ("Happy bots:" & Happy_Bots'Image);
   --     IO.New_Line (2);
   --     --  IO.Put_Line (Boolean'Image (Distance (Another_Good_Ball, Good_Bot.Pos) < Another_Good_Ball.Radius));
   --     --  IO.Put_Line (Boolean'Image (In_Range (Good_Bot, (48834574, 92067750, 38886938))));
   --     --  IO.Put_Line (Boolean'Image (Intersect (Good_Bot, Another_Good_Ball)));
   --     --  IO.Put_Line (Boolean'Image (Intersect (Bad_Bot, Another_Good_Ball)));
   --  end;

   --  trivial testing
   --  declare
   --     B1 : Ball := (Center => Position'
   --                               (X => 1,
   --                                Y => 0,
   --                                Z => 0),
   --                   Radius => 1,
   --                   Count  => 0);
   --     N1 : Nanobot := (Pos => Position'(X => 400, Y => 0, Z => 0),
   --                      Radius => 1);
   --  begin
   --     IO.Put_Line (Alternate_Intersect (B1, N1)'Image);
   --  end;
end Day23;
