--  Advent of Code 2018
--
--  John Perry
--
--  Day 19: Go With The Flow
--
--  part 1: determine the value of register 0 after a background program
--          on your device stops running
--
--  part 2: determine the same, after a second program stops running
--
--  a lot of this is copied from Day 16

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day19 is

   package IO renames Ada.Text_IO;

   type Value is mod 4_000_000_000;
   --  A value on the machine.
   --  My first time through, I made Value too small, which gave me the wrong
   --  answer on Part 1. (I carried over the value from Day 16.)
   --  This is one of those times where I wish Ada allowed one to perform
   --  bitwise operations on ordinary integers.
   --  Or, alternately, that the puzzle master would indicate how large
   --  the values can grow.
   --  If we have to use this in the future and the values grow even larger,
   --  I may be in a quandary...

   type Register is array (0 .. 5) of Value;

   --  SUBSECTION
   --  Opcodes

   type Opcode is (
      Addr, Addi,          --  Add register, Add immediate
      Mulr, Muli,          --  Multiply register, Multiply immediate
      BAnr, BAni,          --  Bitwise And register, Bitwise And immediate
      BOrr, BOri,          --  Bitwise Or register, Bitwise Or immediate
      Setr, Seti,          --  Set register, Set immediate
      Gtir, Gtri, Gtrr,    --  Greater-than imm-reg, imm-reg, reg-reg
      Eqir, Eqri, Eqrr     --  Equality imm-reg, reg-imm, reg-reg
   );

   type Parameters is record
      A, B, C : Integer;
   end record;
   --  parameters to a machine instruction

   --  SUBSECTION
   --  specialized I/O packages for both parts

   package Nat_IO is new IO.Integer_IO (Num => Natural);
   package Opcode_IO is new IO.Enumeration_IO (Enum => Opcode);

   --  SECTION
   --  instructions and program

   type Instruction is record
      O : Opcode;
      P : Parameters;
   end record;
   --  this version of an instruction is for reading the samples,
   --  before we know which value corresponds to which Opcode
   --  once we figure that information in Part 2, we can set up
   --  a Program_Instruction; see below

   IP : Natural;  --  instruction pointer

   package Opcode_Vecs is new Ada.Containers.Vectors (
      Index_Type => Natural, Element_Type => Instruction
   );

   Program : Opcode_Vecs.Vector;

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
   --  allows for convenient, simple execution of a virtual machine instruction

   type Executor is
      access function (P : Parameters; R_0 : Register) return Register;

   Executor_Fns : constant array (Opcode) of Executor  := [
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

   --  SECTION
   --  I/O

   procedure Read_Program is

      F : IO.File_Type;

   begin

      IO.Open (F, IO.In_File, "input.txt");

      --  instruction pointer
      declare
         S : constant String := IO.Get_Line (F);
         Pos : Positive := 1;
      begin
         Nat_IO.Get (S (5 .. S'Last), IP, Pos);
      end;

      while not IO.End_Of_File (F) loop

         declare

            O : Opcode;
            A, B, C : Integer;

            S : constant String := IO.Get_Line (F);
            Pos : Positive := 1;    --  position in S

         begin

            Opcode_IO.Get (S, O, Pos);
            Nat_IO.Get (S (Pos + 2 .. S'Last), A, Pos);
            Nat_IO.Get (S (Pos + 2 .. S'Last), B, Pos);
            Nat_IO.Get (S (Pos + 2 .. S'Last), C, Pos);
            Program.Append (Instruction'(
               O => O,
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

   procedure Put_Instruction (Idx : Natural) is
      Instr : constant Instruction := Program (Idx);
   begin
      IO.Put_Line (
         "IP" & Idx'Image & " " & Instr.O'Image
         & Instr.P.A'Image & Instr.P.B'Image & Instr.P.C'Image
      );
   end Put_Instruction;

   procedure Put_Register (M : Register) is
   begin
      IO.Put ("[");
      for Each of M loop
         IO.Put (Each'Image);
      end loop;
      IO.Put_Line ("]");
   end Put_Register;

   procedure Put_Program is
   begin
      IO.Put_Line ("#ip" & IP'Image);
      for Instr of Program loop
         Opcode_IO.Put (Instr.O); IO.Put (' ');
         Nat_IO.Put (Instr.P.A, 0); IO.Put (' ');
         Nat_IO.Put (Instr.P.B, 0); IO.Put (' ');
         Nat_IO.Put (Instr.P.C, 0); IO.Put (' ');
         IO.New_Line;
      end loop;
   end Put_Program;

   --  SECTION
   --  Part 1 (and 2, if you have the patience for it)

   function Part (R0 : Value := 0) return Value is
      Machine : Register := [0 => R0, others => 0];
      Instructions_Executed : Natural := 0;
   begin
      Machine (IP) := 0;
      while Natural (Machine (IP)) in Program.First_Index .. Program.Last_Index
      loop
         --  Put_Register (Machine);
         --  Put_Instruction (Natural (Machine (IP)));
         Machine := Executor_Fns (Program (Natural (Machine (IP))).O)
               (Program (Natural (Machine (IP))).P, Machine); --  argument
         --  Put_Register (Machine);
         --  IO.New_Line (2);
         Machine (IP) := @ + 1;
         Instructions_Executed := @ + 1;
      end loop;
      Machine (IP) := @ - 1;
      IO.Put_Line ("Executed" & Instructions_Executed'Image & " instructions");
      return Machine (0);
   end Part;

   --  SECTION
   --  a better approach to Part 2
   --  my input jumps to a subprogram after line 16, which sets up R3
   --  with a number used to bound two nested loops, the outer one at line 2
   --  the input then jumps to the outer loop, which turns out to be
   --  a highly inefficient program to sum the factors of the value in R3
   --  I decided to look for when the value in R3 explodes, but
   --  you may just want to look for when the program makes it to line 2,
   --  then add the factors of the largest number in the machine

   function Better_Part_2 return Natural is
      Machine : Register := [0 => 1, others => 0];
      Giant : Natural;
      Result : Natural := 0;
   begin
      Machine (IP) := 0;
      while Machine (IP) /= 3 loop
         Machine := Executor_Fns (Program (Natural (Machine (IP))).O)
               (Program (Natural (Machine (IP))).P, Machine); --  argument
         --  Put_Register (Machine);
         --  IO.New_Line (2);
         Machine (IP) := @ + 1;
      end loop;
      IO.Put ("At start of inner loop, machine has: ");
      Put_Register (Machine);
      --  turns out the inner loop merely sums the factors of R3
      Giant := 0;
      for Each of Machine loop
         if Natural (Each) > Giant then
            Giant := Natural (Each);
         end if;
      end loop;
      for Each in 1 .. Giant loop --  need a better factoring algorithm :-)
         if Giant rem Each = 0 then
            IO.Put_Line ("found factor" & Each'Image);
            Result := @ + Each;
         end if;
      end loop;
      return Result;
   end Better_Part_2;

begin
   Read_Program;
   --  Put_Program;
   IO.Put_Line ("Program ended with register 0 at" & Part'Image);
   --  next line is a REALLY bad idea unless you have a LOT of time
   --  IO.Put_Line ("New program ended with register 0 at" & Part (1)'Image);
   IO.Put_Line ("New program ended with register 0 at" & Better_Part_2'Image);
end Day19;
