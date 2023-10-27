--  Advent of Code 2018
--
--  John Perry
--
--  Day 21: Chronal Conversion
--
--  part 1: find the lowest non-negative value for register 0
--          that halts the program after the fewest operations
--
--  part 2: same, but after the most operations
--
--  much of this is copied from day 19; if this keeps up
--  i'll make a special crate for it

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;

procedure Day21 is

   package IO renames Ada.Text_IO;

   type Value is mod 2**24;   -- determined by program analysis (bani)

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
   --  Parts 1 and 2)

   function Part_1 return Value is
   --  to determine the smallest value that takes the fewest operations,
   --  we run the program until it hits line 28 the first time
   --  this was determined by analyzing the machine code:
   --  the only way it exits is if you hit line 29,
   --  and the only way that happens is if you hit line 28,
   --  which compares R0 to R3.
   --  since R0 is the input, it suffices to know the first value of R3
   --  that we encounter

      Machine : Register := [others => 0];

   begin

      Machine (IP) := 0;

      while Natural (Machine (IP)) in Program.First_Index .. Program.Last_Index
      loop

         Machine := Executor_Fns (Program (Natural (Machine (IP))).O)
               (Program (Natural (Machine (IP))).P, Machine); --  argument

         exit when Machine (IP) = 28;

         Machine (IP) := @ + 1;

      end loop;

      return Machine (Program (28).P.A);

   end Part_1;

   function Value_Hash (V : Value) return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type (V));
   package Value_Sets is new Ada.Containers.Hashed_Sets (
      Element_Type => Value,
      Hash => Value_Hash,
      Equivalent_Elements => "="
   );

   function Slower_Part_2 return Value is
   --  to determine the smallest value that takes the most operations,
   --  we run the program until the first time the value in register 0 repeats
   --  on line 28
   --
   --  see Part_2 below for more information on why we do this

      Machine : Register := [others => 0];

      Found_Values : Value_Sets.Set;

      Last_Singleton : Value := Value'Last;
      --  the last value generated will take the most instructions
      --  neat, huh?

   begin

      IO.Put_Line ("Re-doing part 2, but unoptimized: this will take a while");

      Machine (IP) := 0;

      loop

         Machine := Executor_Fns (Program (Natural (Machine (IP))).O)
               (Program (Natural (Machine (IP))).P, Machine); --  argument

         Machine (IP) := @ + 1;

         if Machine (IP) = 28 then

            declare
               Terminating_Value : constant Value
                  := Machine (Program (Natural (Machine (IP))).P.A);
            begin

               if Found_Values.Contains (Terminating_Value) then
                  exit;
               else
                  Last_Singleton := Terminating_Value;
                  Found_Values.Insert (Terminating_Value);
                  if Natural (Found_Values.Length) rem 1000 = 0 then
                     IO.Put_Line (
                        "Found" & Found_Values.Length'Image
                        & " terminating values"
                     );
                  end if;
               end if;

            end;
         end if;

      end loop;

      return Last_Singleton;
   end Slower_Part_2;

   Broken_Part_2 : exception;

   function Part_2 return Value is
   --  here i guessed correctly at the beginning:
   --  the values the program generates eventually cycle
   --  however, it takes quite a while for them to cycle
   --  (more than 10,731 values in my case!)
   --  and if you run it through the machine code it can take a loooong time,
   --  so much so that i initially gave up on that approach
   --  and tried to analyze the formulas mathematically
   --
   --  for this to work on your part 2, you need to be sure that your program
   --  does essentially the same thing as this one. IT PROBABLY DOES,
   --  though i'm not 100% sure that the instruction lines correspond 100%
   --  nor that the registers correspond 100%
   --  it also assumes that certain constants are 2**24 and 256;
   --  this seems reasonable, but cross-check against your code if not

      R1, R3, R4 : Value := 0;

      Found_Values : Value_Sets.Set;

      Last_Singleton : Value := Value'Last;
      --  the last value generated will take the most instructions
      --  neat, huh?

   begin

      Outer_Loop : loop

         R1 := R3 or Value (Program (6).P.B);      -- line 6 should be a bori
         R3 := Value (Program (7).P.A);            -- line 7 should be a seti

         Inner_Loop : loop

            R4 := R1 rem Value (256);
            R3 := R3 + R4;
            R3 := R3 * Value (Program (11).P.B);   -- line 11 should be a muli

            if Value (256) > R1 then

               if Found_Values.Contains (R3) then
                  return Last_Singleton;

               else
                  Found_Values.Insert (R3);
                  Last_Singleton := R3;

               end if;

               if R3 = Value'Last then
                  exit Outer_Loop;

               else
                  exit Inner_Loop;

               end if;

            else
               R1 := R1 / Value (256);

            end if;

         end loop Inner_Loop;
      end loop Outer_Loop;

      raise Broken_Part_2;

   end Part_2;

begin
   Read_Program;
   IO.Put_Line ("Halts after fewest instructions for" & Part_1'Image);
   IO.Put_Line ("Halts after most instructions for" & Part_2'Image);
   --  uncomment if you're willing to wait a while
   --  IO.Put_Line ("Again, but slower:" & Slower_Part_2'Image);
end Day21;
