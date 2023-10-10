--  Advent of Code 2018
--
--  John Perry
--
--  Day 16: Chronal Classification
--
--  part 1: how many samples from your time traveling piece satisfy at least 3
--          of the manual's instructions?
--
--  part 2: decipher the machine code, run the program, and report the result

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day16 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  Global types and variables

   --  SUBSECTION
   --  Machine values

   type Value is mod 4096;
   --  A value on the machine.
   --  My first time through, I made Value too small, which gave me the wrong
   --  answer on Part 2. (I believe I chose 256, which is sensible for 8 bits.)
   --  This is one of those times where I wish Ada allowed one to perform
   --  bitwise operations on ordinary integers.
   --  Or, alternately, that the puzzle master would indicate how large
   --  the values can grow.

   type Register is array (0 .. 3) of Value;

   --  SUBSECTION
   --  Opcodes

   subtype Opcode_Range is Natural range 0 .. 15;

   type Opcode is (
      Addr, Addi,          --  Add register, Add immediate
      Mulr, Muli,          --  Multiply register, Multiply immediate
      BAnr, BAni,          --  Bitwise And register, Bitwise And immediate
      BOrr, BOri,          --  Bitwise Or register, Bitwise Or immediate
      Setr, Seti,          --  Set register, Set immediate
      Gtir, Gtri, Gtrr,    --  Greater-than imm-reg, imm-reg, reg-reg
      Eqir, Eqri, Eqrr     --  Equality imm-reg, reg-imm, reg-reg
   );

   type Which_Opcodes is array (Opcode) of Boolean;
   type Satisfying_Array is array (Positive range <>) of Which_Opcodes;
   --  Satisfying_Array records for each sample which Opcode it would sastisfy

   type Parameters is record
      A, B, C : Integer;
   end record;
   --  parameters to a machine instruction

   --  SUBSECTION
   --  specialized I/O packages for both parts

   package Dig_IO is new IO.Modular_IO (Num => Value);
   package Nat_IO is new IO.Integer_IO (Num => Natural);
   package Opcode_IO is new IO.Integer_IO (Num => Opcode_Range);

   --  SECTION
   --  Part 1

   --  SUBSECTION
   --  Part 1's types and variables:

   type Instruction is record
      O : Opcode_Range;
      P : Parameters;
   end record;
   --  this version of an instruction is for reading the samples,
   --  before we know which value corresponds to which Opcode
   --  once we figure that information in Part 2, we can set up
   --  a Program_Instruction; see below

   type Sample is record
      I : Instruction;
      Before, After : Register;
      --  register states before and after executing I
   end record;

   package Sample_Vectors is new Ada.Containers.Vectors
      (Index_Type => Positive, Element_Type => Sample);

   Samples : Sample_Vectors.Vector;

   procedure Read_Samples is
      F : IO.File_Type;
   begin

      IO.Open (F, IO.In_File, "input.txt");

      Read_Samples : loop

         declare
            Line_1 : constant String := IO.Get_Line (F);
         begin

            --  the input starts with samples, structured as
            --  Line_1 : registers before instruction
            --  Line_2 : instruction
            --  Line_3 : registers after instruction
            --  Blank line
            --  we don't want to read the program yet, so we quit on blank line
            if Line_1'Length = 0 then
               exit Read_Samples;
            end if;

            declare
               S : Sample;
               Pos : Positive;   --  position

               Line_2 : constant String := IO.Get_Line (F);
               Line_3 : constant String := IO.Get_Line (F);
            begin

               Dig_IO.Get (Line_1 (10 .. Line_1'Last),
                  Value (S.Before (0)), Pos);
               Dig_IO.Get (Line_1 (13 .. Line_1'Last),
                  Value (S.Before (1)), Pos);
               Dig_IO.Get (Line_1 (16 .. Line_1'Last),
                  Value (S.Before (2)), Pos);
               Dig_IO.Get (Line_1 (19 .. Line_1'Last),
                  Value (S.Before (3)), Pos);

               Nat_IO.Get (Line_2 (1 .. Line_2'Last), S.I.O, Pos);
               Nat_IO.Get (Line_2 (3 .. Line_2'Last), S.I.P.A, Pos);
               Nat_IO.Get (Line_2 (5 .. Line_2'Last), S.I.P.B, Pos);
               Nat_IO.Get (Line_2 (7 .. Line_2'Last), S.I.P.C, Pos);

               Dig_IO.Get (Line_3 (10 .. Line_3'Last),
                  Value (S.After (0)), Pos);
               Dig_IO.Get (Line_3 (13 .. Line_3'Last),
                  Value (S.After (1)), Pos);
               Dig_IO.Get (Line_3 (16 .. Line_3'Last),
                  Value (S.After (2)), Pos);
               Dig_IO.Get (Line_3 (19 .. Line_3'Last),
                  Value (S.After (3)), Pos);

               Samples.Append (S);
               IO.Skip_Line (F);

            end;
         end;

      end loop Read_Samples;

      IO.Close (F);

   end Read_Samples;

   --  SUBSECTION
   --  virtual machine instructions

   function Do_Addr (P : Parameters; R_0 : Register) return Register is
      R : Register := R_0;
   begin
      R (P.C) := R (P.A) + R (P.B);
      return R;
   end Do_Addr;

   function Do_Addi (P : Parameters; R_0 : Register) return Register is
      R : Register := R_0;
   begin
      R (P.C) := R (P.A) + Value (P.B);
      return R;
   end Do_Addi;

   function Do_Mulr (P : Parameters; R_0 : Register) return Register is
      R : Register := R_0;
   begin
      R (P.C) := R (P.A) * R (P.B);
      return R;
   end Do_Mulr;

   function Do_Muli (P : Parameters; R_0 : Register) return Register is
      R : Register := R_0;
   begin
      R (P.C) := R (P.A) * Value (P.B);
      return R;
   end Do_Muli;

   function Do_BAnr (P : Parameters; R_0 : Register) return Register is
      R : Register := R_0;
   begin
      R (P.C) := R (P.A) and R (P.B);
      return R;
   end Do_BAnr;

   function Do_BAni (P : Parameters; R_0 : Register) return Register is
      R : Register := R_0;
   begin
      R (P.C) := R (P.A) and Value (P.B);
      return R;
   end Do_BAni;

   function Do_BOrr (P : Parameters; R_0 : Register) return Register is
      R : Register := R_0;
   begin
      R (P.C) := R (P.A) or R (P.B);
      return R;
   end Do_BOrr;

   function Do_BOri (P : Parameters; R_0 : Register) return Register is
      R : Register := R_0;
   begin
      R (P.C) := R (P.A) or Value (P.B);
      return R;
   end Do_BOri;

   function Do_Setr (P : Parameters; R_0 : Register) return Register is
      R : Register := R_0;
   begin
      R (P.C) := R (P.A);
      return R;
   end Do_Setr;

   function Do_Seti (P : Parameters; R_0 : Register) return Register is
      R : Register := R_0;
   begin
      R (P.C) := Value (P.A);
      return R;
   end Do_Seti;

   function Do_Gtir (P : Parameters; R_0 : Register) return Register is
      R : Register := R_0;
   begin
      R (P.C) := (if P.A > Integer (R (P.B)) then 1 else 0);
      return R;
   end Do_Gtir;

   function Do_Gtri (P : Parameters; R_0 : Register) return Register is
      R : Register := R_0;
   begin
      R (P.C) := (if Integer (R (P.A)) > P.B then 1 else 0);
      return R;
   end Do_Gtri;

   function Do_Gtrr (P : Parameters; R_0 : Register) return Register is
      R : Register := R_0;
   begin
      R (P.C) := (if Integer (R (P.A)) > Integer (R (P.B)) then 1 else 0);
      return R;
   end Do_Gtrr;

   function Do_Eqir (P : Parameters; R_0 : Register) return Register is
      R : Register := R_0;
   begin
      R (P.C) := (if P.A = Integer (R (P.B)) then 1 else 0);
      return R;
   end Do_Eqir;

   function Do_Eqri (P : Parameters; R_0 : Register) return Register is
      R : Register := R_0;
   begin
      R (P.C) := (if Integer (R (P.A)) = P.B then 1 else 0);
      return R;
   end Do_Eqri;

   function Do_Eqrr (P : Parameters; R_0 : Register) return Register is
      R : Register := R_0;
   begin
      R (P.C) := (if Integer (R (P.A)) = Integer (R (P.B)) then 1 else 0);
      return R;
   end Do_Eqrr;

   --  SUBSECTION
   --  each function below evaluates the sample's parameters
   --  against its "Before" value and checks the result against
   --  its "After" value, returning True iff they agree

   function Satisfies_Addr (S : Sample) return Boolean is
   begin
      return S.After = Do_Addr (S.I.P, S.Before);
   end Satisfies_Addr;

   function Satisfies_Addi (S : Sample) return Boolean is
   begin
      return S.After = Do_Addi (S.I.P, S.Before);
   end Satisfies_Addi;

   function Satisfies_Mulr (S : Sample) return Boolean is
   begin
      return S.After = Do_Mulr (S.I.P, S.Before);
   end Satisfies_Mulr;

   function Satisfies_Muli (S : Sample) return Boolean is
   begin
      return S.After = Do_Muli (S.I.P, S.Before);
   end Satisfies_Muli;

   function Satisfies_BAnr (S : Sample) return Boolean is
   begin
      return S.After = Do_BAnr (S.I.P, S.Before);
   end Satisfies_BAnr;

   function Satisfies_BAni (S : Sample) return Boolean is
   begin
      return S.After = Do_BAni (S.I.P, S.Before);
   end Satisfies_BAni;

   function Satisfies_BOrr (S : Sample) return Boolean is
   begin
      return S.After = Do_BOrr (S.I.P, S.Before);
   end Satisfies_BOrr;

   function Satisfies_BOri (S : Sample) return Boolean is
   begin
      return S.After = Do_BOri (S.I.P, S.Before);
   end Satisfies_BOri;

   function Satisfies_Setr (S : Sample) return Boolean is
   begin
      return S.After = Do_Setr (S.I.P, S.Before);
   end Satisfies_Setr;

   function Satisfies_Seti (S : Sample) return Boolean is
   begin
      return S.After = Do_Seti (S.I.P, S.Before);
   end Satisfies_Seti;

   function Satisfies_Gtir (S : Sample) return Boolean is
   begin
      return S.After = Do_Gtir (S.I.P, S.Before);
   end Satisfies_Gtir;

   function Satisfies_Gtri (S : Sample) return Boolean is
   begin
      return S.After = Do_Gtri (S.I.P, S.Before);
   end Satisfies_Gtri;

   function Satisfies_Gtrr (S : Sample) return Boolean is
   begin
      return S.After = Do_Gtrr (S.I.P, S.Before);
   end Satisfies_Gtrr;

   function Satisfies_Eqir (S : Sample) return Boolean is
   begin
      return S.After = Do_Eqir (S.I.P, S.Before);
   end Satisfies_Eqir;

   function Satisfies_Eqri (S : Sample) return Boolean is
   begin
      return S.After = Do_Eqri (S.I.P, S.Before);
   end Satisfies_Eqri;

   function Satisfies_Eqrr (S : Sample) return Boolean is
   begin
      return S.After = Do_Eqrr (S.I.P, S.Before);
   end Satisfies_Eqrr;

   --  SUBSECTION
   --  these next couple of lines let me call each function above
   --  from a loop, simplifying Part_1

   type Satisfier is access function (S : Sample) return Boolean;

   Satisfier_Fns : constant array (Opcode) of Satisfier := [
      Addr => Satisfies_Addr'Access,
      Addi => Satisfies_Addi'Access,
      Mulr => Satisfies_Mulr'Access,
      Muli => Satisfies_Muli'Access,
      BAnr => Satisfies_BAnr'Access,
      BAni => Satisfies_BAni'Access,
      BOrr => Satisfies_BOrr'Access,
      BOri => Satisfies_BOri'Access,
      Setr => Satisfies_Setr'Access,
      Seti => Satisfies_Seti'Access,
      Gtir => Satisfies_Gtir'Access,
      Gtri => Satisfies_Gtri'Access,
      Gtrr => Satisfies_Gtrr'Access,
      Eqir => Satisfies_Eqir'Access,
      Eqri => Satisfies_Eqri'Access,
      Eqrr => Satisfies_Eqrr'Access
   ];

   --  SUBSECTION
   --  Part 1 itself! look at how simple this is!

   function Part_1 return Satisfying_Array is
      Result : Satisfying_Array (Samples.First_Index .. Samples.Last_Index);
   begin
      for Idx in Samples.First_Index .. Samples.Last_Index loop
         for O in Opcode loop
            Result (Idx) (O) := Satisfier_Fns (O) (Samples (Idx));
         end loop;
      end loop;
      return Result;
   end Part_1;

   --  SECTION
   --  Part 2

   --  SUBSECTION
   --  Instructions, for soon we know the opcodes

   type Program_Instruction is record
      O : Opcode;
      P : Parameters;
   end record;

   package Program_Vectors is new Ada.Containers.Vectors
      (Index_Type => Positive, Element_Type => Program_Instruction);

   Program : Program_Vectors.Vector;

   --  SUBSECTION
   --  type & variable for working out the assignments

   type Assignment (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Opcode;
         when False => null;
      end case;
   end record;

   Assignments : array (0 .. 15) of Assignment;

   --  SUBSECTION
   --  Reading the program

   procedure Read_Program is
      F : IO.File_Type;
   begin

      IO.Open (F, IO.In_File, "input.txt");

      --  we don't need to read the samples again
      IO.Skip_Line (F, Ada.Text_IO.Count (Natural (Samples.Length) * 4 + 2));

      while not IO.End_Of_File (F) loop

         declare
            O : Opcode_Range;
            A, B, C : Integer;

            S : constant String := IO.Get_Line (F);
            Pos : Positive := 1;    --  position in S
         begin

            Opcode_IO.Get (S, O, Pos);
            Nat_IO.Get (S (Pos + 2 .. S'Last), A, Pos);
            Nat_IO.Get (S (Pos + 2 .. S'Last), B, Pos);
            Nat_IO.Get (S (Pos + 2 .. S'Last), C, Pos);
            Program.Append (Program_Instruction'(
               O => Assignments (O).Value,
               P => Parameters'(
                  A => A,
                  B => B,
                  C => C
               )
            ));
         end;
      end loop;
      IO.Close (F);
   end Read_Program;

   --  SUBSECTION
   --  much like Satisfier_Fns, this allows for convenient, simple execution
   --  of a virtual machine instruction when you know its Opcode

   type Executor is
      access function (P : Parameters; R_0 : Register) return Register;

   Executor_Fns : constant array (Opcode) of Executor := [
      Addr => Do_Addr'Access,
      Addi => Do_Addi'Access,
      Mulr => Do_Mulr'Access,
      Muli => Do_Muli'Access,
      BAnr => Do_BAnr'Access,
      BAni => Do_BAni'Access,
      BOrr => Do_BOrr'Access,
      BOri => Do_BOri'Access,
      Setr => Do_Setr'Access,
      Seti => Do_Seti'Access,
      Gtir => Do_Gtir'Access,
      Gtri => Do_Gtri'Access,
      Gtrr => Do_Gtrr'Access,
      Eqir => Do_Eqir'Access,
      Eqri => Do_Eqri'Access,
      Eqrr => Do_Eqrr'Access
   ];

   --  SUBSECTION
   --  Part 2 itself! not quite as simple as Part 1, I'm afraid :-(

   function Part_2 (Original_Sample_Data : Satisfying_Array) return Value is
      Machine : Register := [others => 0];
      Sample_Data : Satisfying_Array := Original_Sample_Data;
   begin

      --  first we work out the sample / opcode correspondence
      --  we examine which samples satisfy only one opcode,
      --  assign them accordingly, then remove that potential correspondence
      --  from any other samples that satisfied the opcode
      --  this quickly works them all out
      while (for some A of Assignments => not A.Valid) loop
         for Idx in Sample_Data'First .. Sample_Data'Last loop

            if not Assignments (Samples (Idx).I.O).Valid then

               declare
                  Last_Valid_Op_Code : Opcode;
                  --  the last opcode this sample satisfies
                  --  if only one does, then we have a match
                  Num_Satisfied : Natural := 0;
               begin

                  --  check for matches
                  for O in Opcode loop
                     if Sample_Data (Idx) (O) then
                        Num_Satisfied := @ + 1;
                        Last_Valid_Op_Code := O;
                     end if;
                  end loop;

                  --  do we have only 1? record it and remove it from others
                  if Num_Satisfied = 1 then

                     Assignments (Samples (Idx).I.O) := Assignment'
                        (Valid => True, Value => Last_Valid_Op_Code);

                     for Jdx in Sample_Data'First .. Sample_Data'Last loop
                        if Samples (Jdx).I.O /= Samples (Idx).I.O then
                           Sample_Data (Jdx) (Last_Valid_Op_Code) := False;
                        end if;
                     end loop;

                  end if;

               end;

            end if;

         end loop;
      end loop;

      --  some eye candy: report the correspondence
      for A in Assignments'First .. Assignments'Last loop
         IO.Put_Line (
            "Opcode" & A'Image & " corresponds to "
             & Assignments (A).Value'Image
            );
      end loop;

      --  read and run the program
      Read_Program;
      for Instr of Program loop
         Machine := Executor_Fns (Instr.O) (Instr.P, Machine);
      end loop;

      return Machine (0);

   end Part_2;

begin

   Read_Samples;

   declare
      P1_Result : Natural := 0;
      Sample_Data : constant Satisfying_Array := Part_1;
   begin

      --  process the checks performed in Part 1
      for Idx in Sample_Data'First .. Sample_Data'Last loop

         declare
            Number_Satisfied : Natural := 0;
         begin

            for O in Opcode loop
               if Sample_Data (Idx) (O) then
                  Number_Satisfied := @ + 1;
               end if;
            end loop;

            if Number_Satisfied >= 3 then
               P1_Result := @ + 1;
            end if;

         end;

      end loop;

      IO.Put_Line (
         "There are" & P1_Result'Image & " samples w/at least 3 matches"
      );
      IO.Put_Line ("The program's output is" & Part_2 (Sample_Data)'Image);
   end;
end Day16;
