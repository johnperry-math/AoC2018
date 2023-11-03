--  Advent of Code 2018
--
--  John Perry
--
--  Day 24: Immune Simulator 20XX
--
--  part 1: How many units does an infection have after beating  a reindeer's
--          immune system?
--
--  part 2: What's the smallest boost the immune system needs in order to
--          turn the tables?

pragma Ada_2022;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Generic_Array_Sort;

procedure Day24 is

   package IO renames Ada.Text_IO;
   package Natural_IO is new IO.Integer_IO (Num => Natural);
   package Positive_IO is new IO.Integer_IO (Num => Positive);

   Doing_Example : constant Boolean := False;

   --  SECTION
   --  Global constants and variables

   --  SUBSECTION
   --  unit attacks

   type Attack_Types is (
      Bludgeoning,
      Cold,
      Fire,
      Radiation,
      Slashing
   );

   package Attack_IO is new IO.Enumeration_IO (Enum => Attack_Types);

   type Attack_Reaction is (Weakness, Immunity, Neither);

   type Attack_Reactions is array (Attack_Types) of Attack_Reaction;

   --  SUBSECTION
   --  groups and armies

   type Group is record
      Units : Natural;
      Hit_Points : Positive;
      Damage : Positive;
      Attacks : Attack_Types;
      Initiative : Positive;
      Reactions : Attack_Reactions;
   end record;

   function Effective_Power (G : Group) return Natural is
   (G.Units * G.Damage);

   package Group_Vectors is new Ada.Containers.Vectors (
      Index_Type => Positive, Element_Type => Group
   );

   Original_Immune, Original_Infection,
      Immune_System, Infection : Group_Vectors.Vector;

   --  SECTION
   --  I/O

   procedure Get_Reactions (
      G : in out Group;
      S : String;
      Pos : in out Positive;
      Which : Attack_Reaction
   ) is
   --  Reads from S, starting at position Pos,
   --  the reactions of type Which, storing them in G
   --
   --  it is incumbent on the user to call this
   --  with a valid value of Pos that

      A : Attack_Types;

   begin

      loop
         Attack_IO.Get (S (Pos .. S'Last), A, Pos);
         G.Reactions (A) := Which;
         Pos := @ + 1;
         exit when S (Pos) /= ',';
         Pos := @ + 2;
      end loop;

   end Get_Reactions;

   procedure Read_Group (S : String; Which : in out Group_Vectors.Vector) is
   --  Reads from S the information needed to build
   --  a group to insert into Which, then inserts it

      G : Group;
      Pos : Positive := S'First;

   begin

      Positive_IO.Get (S (Pos .. S'Last), G.Units, Pos);
      Pos := @ + 18;

      Positive_IO.Get (S (Pos .. S'Last), G.Hit_Points, Pos);
      Pos := @ + 13;

      --  reactions

      G.Reactions := [ others => Neither ];     --  good default

      if S (Pos) = '(' then   -- there might not be any!

         Pos := @ + 1;

         while S (Pos) /= ')' loop

            if S (Pos) = 'i' then
               Pos := @ + 10;
               Get_Reactions (G, S (Pos .. S'Last), Pos, Immunity);
            else
               Pos := @ + 8;
               Get_Reactions (G, S (Pos .. S'Last), Pos, Weakness);
            end if;

            if S (Pos) /= ')' then
               Pos := @ + 2;
            end if;

         end loop;

         Pos := @ + 27;

      else
         Pos := @ + 25;

      end if;

      Positive_IO.Get (S (Pos .. S'Last), G.Damage, Pos);
      Pos := @ + 2;

      Attack_IO.Get (S (Pos .. S'Last), G.Attacks, Pos);
      Pos := @ + 22;

      Positive_IO.Get (S (Pos .. S'Last), G.Initiative, Pos);
      Which.Append (G);

   end Read_Group;

   procedure Read_Input is
      F : IO.File_Type;
   begin

      IO.Open (
         F,
         IO.In_File,
         (if Doing_Example then "example.txt" else "input.txt")
      );

      --  immune system
      IO.Skip_Line (F);
      Immune : loop
         declare
            S : constant String := IO.Get_Line (F);
         begin
            exit Immune when S'Length = 0;
            Read_Group (S, Immune_System);
         end;
      end loop Immune;

      --  infection
      IO.Skip_Line (F);
      Infect : loop
         declare
            S : constant String := IO.Get_Line (F);
         begin
            Read_Group (S, Infection);
            exit Infect when IO.End_Of_File (F);
         end;
      end loop Infect;

   end Read_Input;

   procedure Put_Group (G : Group) is
   begin
      IO.Put ("[ ");
      Natural_IO.Put (G.Units, 0);
      IO.Put (" units each with ");
      Positive_IO.Put (G.Hit_Points, 0);
      IO.Put (" hit points (");
      for A in Attack_Types loop
         if G.Reactions (A) = Immunity then
            IO.Put (" immune to ");
            Attack_IO.Put (A);
         elsif G.Reactions (A) = Weakness then
            IO.Put (" weak to ");
            Attack_IO.Put (A);
         end if;
      end loop;
      IO.Put (") with an attack that does ");
      Positive_IO.Put (G.Damage, 0);
      IO.Put (" ");
      Attack_IO.Put (G.Attacks);
      IO.Put (" damage at initiative ");
      Positive_IO.Put (G.Initiative, 0);
      IO.Put (" ]");
   end Put_Group;

   procedure Put_Armies is
   begin

      IO.Put_Line ("Immune System:");
      for I of Immune_System loop
         Put_Group (I);
         IO.New_Line;
      end loop;

      IO.Put_Line ("Infection:");
      for I of Infection loop
         Put_Group (I);
         IO.New_Line;
      end loop;

   end Put_Armies;

   --  SECTION
   --  Parts 1 and 2

   --  SUBSECTION
   --  support functions

   function Still_Standing (Army : Group_Vectors.Vector) return Boolean is
   --  true iff any army units are left
   (
      for some G of Army => G.Units > 0
   );

   function Damage_Inflicted (Attacker, Defender : Group) return Natural is
   --  the amount of damage that Attacker would inflict on Defender
   (
      case Defender.Reactions (Attacker.Attacks) is
      when Neither => Effective_Power (Attacker),
      when Weakness => 2 * Effective_Power (Attacker),
      when Immunity => 0
   );

   --  SUBSECTION
   --  targets!

   type Target (Chosen : Boolean := False) is record
      case Chosen is
      when True =>
         Index : Positive;
         Damage : Positive;
      when False => null;
      end case;
   end record;

   type Target_Array is array (Positive range <>) of Target;
   --  the idea is that if the ith army targets the opponent with index j,
   --  then Target_Array (i) = k

   --  SUBSUBSECTION
   --  targeting:

   type Sort_Array is array (Positive range <>) of Positive;
   --  the idea is that if the ith army gets to target an opponent on
   --  the jth round of targeting, then Sort_Array (j) = i

   function Generate_Target_Order (Attackers : Group_Vectors.Vector)
      return Sort_Array
   is
      --  sorting the Sort_Array
      --  this HAS to be done after every turn, as the number of units changes

      Length : constant Positive := Positive (Attackers.Length);

      Sorted_Attackers : Sort_Array (1 .. Length)
         := [ for Each in 1 .. Length => Each ];

      function Sort_Attackers (Left, Right : Natural) return Boolean is
      (
         Effective_Power (Attackers (Left)) >
            Effective_Power (Attackers (Right))
         or else (
            Effective_Power (Attackers (Left)) =
               Effective_Power (Attackers (Right)) and then
            Attackers (Left).Initiative > Attackers (Right).Initiative
         )
      );

      procedure Sort_Attackers is new Ada.Containers.Generic_Array_Sort (
         Index_Type => Positive,
         Element_Type => Positive,
         Array_Type => Sort_Array,
         "<" => Sort_Attackers
      );

   begin

      --  as with so many tasks, this required more set up than work ;-)
      Sort_Attackers (Sorted_Attackers);
      return Sorted_Attackers;

   end Generate_Target_Order;

   procedure Sort_Out_Targets (
      Attackers, Defenders : Group_Vectors.Vector;
      Targets : in out Target_Array
   ) is
   --  each group chooses a target per puzzle rules

      Damage : Natural;
      --  how much damage the current attacker would inflict
      --  on its chosen defender

      Sorted_Attackers : constant Sort_Array
         := Generate_Target_Order (Attackers);

   begin

      Targets := [ others => (Chosen => False) ];

      for I of Sorted_Attackers when Attackers (I).Units > 0 loop
         for J in 1 .. Positive (Defenders.Length) loop

            if Defenders (J).Units > 0 and then (
               for all T of Targets => (not T.Chosen or else T.Index /= J)
            )
            then

               Damage := Damage_Inflicted (Attackers (I), Defenders (J));

               if
                  Damage > 0 and then (

                  not Targets (I).Chosen or else

                  Targets (I).Damage < Damage or else (

                     Targets (I).Damage = Damage and then
                     Effective_Power (Defenders (J)) >
                        Effective_Power (Defenders (Targets (I).Index))

                  ) or else (

                     Targets (I).Damage = Damage and then
                     Effective_Power (Defenders (J)) =
                        Effective_Power (Defenders (Targets (I).Index))
                     and then
                        Defenders (J).Initiative >
                           Defenders (Targets (I).Index).Initiative

                  )
               )
               then

                  Targets (I) := (
                     Chosen => True, Index => J, Damage => Damage
                  );

               end if;
            end if;

         end loop;
      end loop;

   end Sort_Out_Targets;

   --  SUBSUBSECTION
   --  doing battle!

   type Melee (From_Immune : Boolean := True) is record
      Attacker : Positive;
   end record;
   --  From_Immune is true when the attacker is from the immune system

   type Melee_Array is array (Positive range <>) of Melee;

   function "<" (Left, Right : Melee) return Boolean is

      Left_Initiative : constant Positive := (
         if Left.From_Immune then Immune_System (Left.Attacker).Initiative
         else Infection (Left.Attacker).Initiative
      );

      Right_Initiative : constant Positive := (
         if Right.From_Immune then Immune_System (Right.Attacker).Initiative
         else Infection (Right.Attacker).Initiative
      );

   begin
      return Left_Initiative > Right_Initiative;
   end "<";

   procedure Devise_Order_Of_Battle is new Ada.Containers.Generic_Array_Sort (
      Index_Type => Positive,
      Element_Type => Melee,
      Array_Type => Melee_Array
   );

   procedure Attack_Target (Attacker : Group; Defender : in out Group) is
   --  Attacker attacks Defender, which loses units

      Total_Defense : constant Natural := Defender.Units * Defender.Hit_Points;

      Potential_Damage : constant Natural := (
         case Defender.Reactions (Attacker.Attacks) is
         when Neither => Effective_Power (Attacker),
         when Weakness => 2 * Effective_Power (Attacker),
         when Immunity => 0     --  this should never occur, of course...
      );

      Survivor : constant Natural := (
         if (Total_Defense - Potential_Damage) rem Defender.Hit_Points <= 0
         then 0
         else 1
      );

   begin

      Defender.Units := Integer'Max (
         0,
         (Total_Defense - Potential_Damage) / Defender.Hit_Points + Survivor
      );

   end Attack_Target;

   procedure Attack_Targets (Immune_Targets, Infection_Targets : Target_Array)
   is
   --  handles attacks for both armies, in order of initiative

      Immune_Length : constant Positive := Positive (Immune_Targets'Length);
      Infection_Length : constant Positive
         := Positive (Infection_Targets'Length);
      Order_Of_Battle : Melee_Array (1 .. Immune_Length + Infection_Length);

   begin

      --  set up the order of battle
      --  this could be done only once,
      --  but the program isn't slow, so i won't bother
      for I in 1 .. Immune_Length loop
         Order_Of_Battle (I) := (From_Immune => True, Attacker => I);
      end loop;
      for I in 1 .. Infection_Length loop
         Order_Of_Battle (Immune_Length + I)
            := (From_Immune => False, Attacker => I);
      end loop;
      Devise_Order_Of_Battle (Order_Of_Battle);

      for Faceoff of Order_Of_Battle loop

         if Faceoff.From_Immune then
            if Immune_System (Faceoff.Attacker).Units > 0 and then
               Immune_Targets (Faceoff.Attacker).Chosen
            then
               Attack_Target (
                  Immune_System (Faceoff.Attacker),
                  Infection (Immune_Targets (Faceoff.Attacker).Index)
               );
            end if;

         else
            if Infection (Faceoff.Attacker).Units > 0 and then
               Infection_Targets (Faceoff.Attacker).Chosen
            then
               Attack_Target (
                  Infection (Faceoff.Attacker),
                  Immune_System (Infection_Targets (Faceoff.Attacker).Index)
               );
            end if;
         end if;

      end loop;

   end Attack_Targets;

   function Remaining_Armies return Natural is
   --  returns the number of armies remaining in BOTH armies
   --  at end of puzzle, this should of course count armies in ONE army

      Result : Natural := 0;

   begin

      for G of Immune_System loop
         Result := @ + G.Units;
      end loop;

      for G of Infection loop
         Result := @ + G.Units;
      end loop;

      return Result;

   end Remaining_Armies;

   function Part_1 return Natural is

      use type Group_Vectors.Vector;

      Old_Immune, Old_Infection : Group_Vectors.Vector;

      Immune_Targets : Target_Array (1 .. Positive (Immune_System.Length));
      Infection_Targets : Target_Array (1 .. Positive (Infection.Length));

   begin

      while Still_Standing (Immune_System) and then Still_Standing (Infection)
      loop

         Old_Immune := Immune_System.Copy;
         Old_Infection := Infection.Copy;

         Sort_Out_Targets (Immune_System, Infection, Immune_Targets);
         Sort_Out_Targets (Infection, Immune_System, Infection_Targets);

         Attack_Targets (Immune_Targets, Infection_Targets);

         --  prevent steady state which occurs when remaining armies are immune
         --  to each other's attacks
         if Old_Immune = Immune_System and then Old_Infection = Infection then
            exit;
         end if;

      end loop;

      return Remaining_Armies;

   end Part_1;

   function Part_2 return Natural is
   --  the space SHOULD be linear, so we use a binary search algorithm
   --  i have read that some of the inputs were not in fact linear,
   --  which i can believe! but in my case, it was
   --
   --  in the case of a non-linear space, change this to proceed from
   --  the bottom up: answers seem to come quickly all the same

      Lower : Natural := 0;
      Upper : Natural := 3000;
      Boost : Natural := (Upper + Lower) / 2;

      Result, Armies_Remaining : Natural := 0;

   begin

      loop

         --  setup
         Immune_System := Original_Immune.Copy;
         Infection := Original_Infection.Copy;

         IO.Put_Line ("trying" & Boost'Image);
         for Each of Immune_System loop
            Each.Damage := @ + Boost;
         end loop;

         Result := Part_1;

         if (for some I of Infection => I.Units > 0) then
            IO.Put_Line ("Infection wins :-(");
            Lower := Boost;
            Boost := (Boost + Upper) / 2;

         else
            IO.Put_Line ("Reindeer wins :-)");
            Armies_Remaining := Result;
            Upper := Boost;
            Boost := (Boost + Lower) / 2;
         end if;

         exit when Upper - Lower <= 1;

      end loop;

      return Armies_Remaining;

   end Part_2;

begin

   Read_Input;
   Original_Immune := Immune_System.Copy;
   Original_Infection := Infection.Copy;

   IO.Put_Line ("Initial state:");
   Put_Armies;
   IO.New_Line (2);

   IO.Put_Line ("Armies remaining:" & Part_1'Image);
   Put_Armies;
   IO.New_Line (2);

   IO.Put_Line ("Armies remaining after boost:" & Part_2'Image);
   Put_Armies;
   IO.New_Line (2);

end Day24;