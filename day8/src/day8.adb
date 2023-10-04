--  Advent of Code 2018
--
--  John Perry
--
--  Day 8: Memory Maneuver
--
--  part 1: determine the sum of all nodes' metadata
--
--  part 2: determine the value of the root node
--          the value of a node is:
--             if it has no children, the sum of its metas
--             otherwise, the sum of its children's values
--                (do not include illegitimate childre)

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day8 is

   package IO renames Ada.Text_IO;

   --  SECTION
   --  Global types and variables

   package Int_Vectors is new Ada.Containers.Vectors
      (Index_Type => Positive, Element_Type => Natural);

   type Node is record
      Num_Children, Num_Metas : Natural;  --  header
      Metas : Int_Vectors.Vector;         --  meta values
      Children : Int_Vectors.Vector;      --  points to children
   end record;

   package Node_Vectors is new Ada.Containers.Vectors
      (Index_Type => Positive, Element_Type => Node);

   Nodes : Node_Vectors.Vector;

   --  SECTION
   --  I/O

   function Read_Node (S : String; Pos : in out Natural) return Positive is

      package Int_IO is new IO.Integer_IO (Num => Natural);

      Meta : Natural;
      Num_Children, Num_Metas : Natural;
      Children, Metas : Int_Vectors.Vector;

   begin

      --  read header
      Int_IO.Get (S (Pos .. S'Last), Num_Children, Pos);
      Int_IO.Get (S (Pos + 2 .. S'Last), Num_Metas, Pos);

      --  step
      Pos := Pos + 2;

      --  read children
      for Each in 1 .. Num_Children loop
         Children.Append (Read_Node (S, Pos));
      end loop;

      --  read metas
      for Each in 1 .. Num_Metas loop
         Int_IO.Get (S (Pos .. S'Last), Meta, Pos);
         Pos := Pos + 2;
         Metas.Append (Meta);
      end loop;

      -- append and return
      Nodes.Append (Node'(Num_Children, Num_Metas, Metas, Children));
      return Nodes.Last_Index;

   end Read_Node;

   procedure Read_Input is
      F : IO.File_Type;
   begin

      IO.Open (F, IO.In_File, "input.txt");

      declare
         S : constant String := IO.Get_Line (F);
         Pos : Natural := S'First;
         Root_Node : Natural := Read_Node (S, Pos);
      begin
         null;
      end;

   end Read_Input;

   --  SECTION
   --  Part 1

   function Part_1 return Natural is
      Result : Natural := 0;
   begin

      for H of Nodes loop
         Result := Result + H.Metas'Reduce ("+", 0);
      end loop;

      return Result;

   end Part_1;

   --  SECTION
   --  Part 2

   function Node_Value (Idx : Positive) return Natural is
   --  recursively compute the value of the node stored at the given index

      Result : Natural := 0;
      H : Node := Nodes (Idx);

   begin

      if H.Num_Children = 0 then
         Result := H.Metas'Reduce ("+", 0);

      else

         for M of H.Metas loop
            if M in  1 .. H.Num_Children then
               Result := @ + Node_Value (H.Children (M));
            end if;
         end loop;

      end if;

      return Result;

   end Node_Value;

   function Part_2 return Natural is
      (Node_Value (Nodes.Last_Index));

begin
   Read_Input;
   IO.Put_Line ("Sum of metas is" & Part_1'Image);
   IO.Put_Line ("Root node's value" & Part_2'Image);
end Day8;
