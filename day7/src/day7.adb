--  Advent of Code 2018
--
--  John Perry
--
--  Day 7: The Sum of Its Parts
--
--  part 1: determine the correct sequence of steps in the instructions
--
--  part 2: determine how long it will take to complete the steps
--          when you have 4 helpers, and each step has a time penalty

with Ada.Text_IO;

procedure Day7 is

   package IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;
   --  By the time I decided to try the example I had solved the problem,
   --  but I had to wait 5 minutes for too many wrong answers,
   --  so I went ahead and tried with the example to be certain

   --  SECTION
   --  Global types and variables

   type Step is new Character range 'A' .. (
      if Doing_Example then 'F' else 'Z'
   );

   type Dependencies is array (Step) of Boolean;

   Instructions : array (Step) of Dependencies
      := [others => [others => False]];

   --  SECTION
   --  I/O

   package Step_IO is new IO.Enumeration_IO (Enum => Step);

   procedure Print_Instructions is
   --  useful for debugging
   begin

      for C in Step loop

         Step_IO.Put (C); IO.Put_Line (":"); IO.Put ("   ");

         for D in Step loop
            if Instructions (C) (D) then
               Step_IO.Put (D); IO.Put (' ');
            end if;
         end loop;

         IO.New_Line (2);

      end loop;

   end Print_Instructions;

   procedure Read_Input is
   --  the ease of the I/O on this one was a relief
      F : IO.File_Type;
   begin

      IO.Open (
         F,
         IO.In_File,
         (if Doing_Example then "example.txt" else "input.txt")
      );

      while not IO.End_Of_File (F) loop
         declare
            S : constant String := IO.Get_Line (F);
         begin
            Instructions (Step (S (37))) (Step (S (6))) := True;
         end;
      end loop;

      IO.Close (F);

   end Read_Input;

   --  SECTION
   --  Part 1

   Steps : array (1 .. (if Doing_Example then 6 else 26)) of Step;
   --  the correct order of steps

   Completed : Dependencies := [others => False];
   --  whether the Step has been completed

   function Step_Is_Available (S : Step)  return Boolean is
      (for all D in Step => (if Instructions (S) (D) then Completed (D)));
   --  whether Step is available yet

   procedure Part_1 is
      Done : Natural := 0;
   begin

      while Done < Steps'Last loop

         Next_Step : for C in Step loop

            if not Completed (C) then

               if Step_Is_Available (C) then
                  Done := @ + 1;
                  Steps (Done) := C;
                  Completed (C) := True;
                  exit Next_Step;
               end if;

            end if;

         end loop Next_Step;

      end loop;

   end Part_1;

   --  SECTION
   --  Part 2

   Completed_At : array (Step) of Natural := [others => Natural'Last];
   --  the time at which a given step will be ready

   function Step_Is_Available_At_Time (
      S : Step;
      Current_Time : Natural
   ) return Boolean is
      (for all D in Step =>
         (if Instructions (S) (D) then Completed_At (D) <= Current_Time)
      );
   --  whether Step is available at Current_Time

   function Part_2 return Natural is

      Done : Natural := 0;
      --  how many steps are done
      Current_Time : Natural := 0;
      --  the current time
      Completed : Dependencies := [others => False];
      --  whether a step has been completed
      --  (I'm pretty sure I could replace this with some convoluted logic
      --  on Finish_Time, but I don't see the point)
      Time_Penalty : constant Natural := (if Doing_Example then 1 else 61);
      --  common amount of time it takes to complete a step
      --  (60 seconds + 1 for A)
      Finish_Time : array (1 .. (if Doing_Example then 2 else 5)) of Natural
         := [others => 0];
      --  when the worker is ready for another task

   begin

      while Done < Steps'Length loop

         for Helper in Finish_Time'Range loop

            if Finish_Time (Helper) <= Current_Time then

               Next_Step : for C in Step loop

                  if not Completed (C) then
                     if Step_Is_Available_At_Time (C, Current_Time) then

                        --  next step is useful for debugging
                        IO.Put_Line (
                           "Worker" & Helper'Image
                           & " starts " & C'Image
                           & " at" & Current_Time'Image
                           & " and will finish at"
                           & Natural'Image (
                              Current_Time + Time_Penalty
                                 + Step'Pos (C) - Step'Pos ('A')
                           )
                        );

                        Done := @ + 1;
                        Steps (Done) := C;
                        Completed (C) := True;
                        Finish_Time (Helper) := Current_Time + Time_Penalty
                           + Step'Pos (C) - Step'Pos ('A');
                        Completed_At (C) := Finish_Time (Helper);
                        exit Next_Step;

                     end if;
                  end if;

               end loop Next_Step;

            end if;

         end loop;

         Current_Time := @ + 1;

      end loop;

      return Finish_Time'Reduce (Natural'Max, 0);

   end Part_2;

begin

   Read_Input;
   Print_Instructions; --  useful for debugging

   Part_1;
   for C of Steps loop --  useful for debugging
      IO.Put (C'Image);
   end loop;
   IO.New_Line;

   IO.Put_Line ("with 5 helpers it will take" & Part_2'Image & " seconds");

end Day7;
