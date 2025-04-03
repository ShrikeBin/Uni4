with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Random_Seeds; use Random_Seeds;
with Constants;
with Position_Type;
with Ada.Real_Time; use Ada.Real_Time;

procedure Travelers is

-- Timing
  Start_Time : Time := Clock;  -- global starting time

-- Random seeds for the tasks' random number generators
  Seeds : Seed_Array_Type(1..Constants.Nr_Of_Travelers) := Make_Seeds(Constants.Nr_Of_Travelers);

  -- Protected type for each position on the board
   protected type Position_Mutex is
      function Is_Occupied return Boolean;
      procedure Set;
      procedure Clear;
   private
      Occupied : Boolean := False;  -- False means unoccupied, True means occupied
   end Position_Mutex;

   protected body Position_Mutex is
      function Is_Occupied return Boolean is
      begin
         return Occupied;
      end Is_Occupied;

      procedure Set is
      begin
         Occupied := True;
      end Set;

      procedure Clear is
      begin
         Occupied := False;
      end Clear;
   end Position_Mutex;

  -- Board of position mutexes
  type Board_Type is array (0..Constants.Board_Width-1, 0..Constants.Board_Height-1) of Position_Mutex;
  Board : Board_Type;

  -- elementary steps
  procedure Move_Down(Position: in out Position_Type.Position_Type) is
  begin
    Position.Y := (Position.Y + 1) mod Constants.Board_Height;
  end Move_Down;

  procedure Move_Up(Position: in out Position_Type.Position_Type) is
  begin
    Position.Y := (Position.Y + Constants.Board_Height - 1) mod Constants.Board_Height;
  end Move_Up;

  procedure Move_Right(Position: in out Position_Type.Position_Type) is
  begin
    Position.X := (Position.X + 1) mod Constants.Board_Width;
  end Move_Right;

  procedure Move_Left(Position: in out Position_Type.Position_Type) is
  begin
    Position.X := (Position.X + Constants.Board_Width - 1) mod Constants.Board_Width;
  end Move_Left;

  -- traces of travelers
  type Trace_Type is record 	      
    Time_Stamp:  Duration;	      
    Id : Integer;
    Position: Position_Type.Position_Type;      
    Symbol: Character;	      
  end record;	      

  type Trace_Array_type is array(0..Constants.Max_Steps) of Trace_Type;

  type Traces_Sequence_Type is record
    Last: Integer := -1;
    Trace_Array: Trace_Array_type;
  end record; 

  procedure Print_Trace(Trace: Trace_Type) is
    Symbol : String := (' ', Trace.Symbol);
  begin
    Put_Line(
        Duration'Image(Trace.Time_Stamp) & " " &
        Integer'Image(Trace.Id) & " " &
        Integer'Image(Trace.Position.X) & " " &
        Integer'Image(Trace.Position.Y) & " " &
        (' ', Trace.Symbol)
      );
  end Print_Trace;

  procedure Print_Traces(Traces: Traces_Sequence_Type) is
  begin
    for I in 0..Traces.Last loop
      Print_Trace(Traces.Trace_Array(I));
    end loop;
  end Print_Traces;

  -- task Printer collects and prints reports of traces
  task Printer is
    entry Report(Traces: Traces_Sequence_Type);
  end Printer;
  
  task body Printer is 
    All_Complete: Boolean := False;
  begin
    for I in 1..Constants.Nr_Of_Travelers loop
      accept Report(Traces: Traces_Sequence_Type) do
        Print_Traces(Traces);
      end Report;
    end loop;
    All_Complete := True;
  end Printer;

  -- travelers
  type Traveler_Type is record
    Id: Integer;
    Symbol: Character;
    Position: Position_Type.Position_Type;    
  end record;

  task type Traveler_Task_Type is	
    entry Init(Id: Integer; Seed: Integer; Symbol: Character);
    entry Start;
  end Traveler_Task_Type;	

  task body Traveler_Task_Type is
    G: Generator;
    MoveType : Integer;
    Traveler: Traveler_Type;
    Time_Stamp: Duration;
    Nr_of_Steps: Integer;
    Traces: Traces_Sequence_Type; 
    Timeout: Duration;

    procedure Store_Trace is
    begin  
      Traces.Last := Traces.Last + 1;
      Traces.Trace_Array(Traces.Last) := ( 
          Time_Stamp => Time_Stamp,
          Id => Traveler.Id,
          Position => Traveler.Position,
          Symbol => Traveler.Symbol
        );
    end Store_Trace;
    
    function Make_Step return Boolean is
      New_Position: Position_Type.Position_Type;
      Moved: Boolean := False;

      -- Convert a character to lower case
      function To_Lower(C: Character) return Character is
      begin
         case C is
               when 'A'..'Z' =>
                  return Character'Val(Character'Pos(C) + (Character'Pos('a') - Character'Pos('A')));
               when others =>
                  return C;
         end case;
      end To_Lower;

    begin
      New_Position := Traveler.Position;    
      case MoveType is
         when 1 => 
            Move_Left(New_Position);
         when 2 => 
            Move_Right(New_Position);
         when 3 => 
            Move_Up(New_Position);
         when 4 => 
            Move_Down(New_Position);
         when others => 
            Put_Line(" ?????????????? " & Integer'Image(MoveType));
            return False;
      end case;
      Timeout:= Constants.Deadlock_Delay;
      -- Try to acquire the new position
      loop
      -- If the position is not occupied, move the traveler
      if not Board(New_Position.X, New_Position.Y).Is_Occupied then
         Board(New_Position.X, New_Position.Y).Set;
         Board(Traveler.Position.X, Traveler.Position.Y).Clear;
         Traveler.Position := New_Position;
         return True;
      end if;
         delay 0.01;
         Timeout := Timeout - 0.01;
         if Timeout <= 0.0 then
            Traveler.Symbol := To_Lower(Traveler.Symbol);
            return False;  -- Timeout expired
         end if;
   end loop;
    end Make_Step;

  begin
    accept Init(Id: Integer; Seed: Integer; Symbol: Character) do
      Reset(G, Seed); 
      Traveler.Id := Id;
      Traveler.Symbol := Symbol;
      -- Random initial position:
      Traveler.Position := (
          X => Id,
          Y => Id          
        );
      -- Occupy initial position
      Board(Traveler.Position.X, Traveler.Position.Y).Set;

      
      Store_Trace; -- store starting position
      -- Number of steps to be made by the traveler  
      Nr_of_Steps := Constants.Min_Steps + 
                    Integer(Float(Constants.Max_Steps - Constants.Min_Steps) * Random(G));
      -- Time_Stamp of initialization
      Time_Stamp := To_Duration(Clock - Start_Time);
    end Init;
    
    -- determine the move
    if Traveler.Id mod 2 = 0 then
            -- Even ID: move Up(3) or Down(4)
            if Integer(Float'Floor(2.0 * Random(G))) = 1 then
               MoveType := 3;  -- Up
            else
               MoveType := 4;  -- Down
            end if;
         else
            -- Odd ID: move Left(1) or Right(2)
            if Integer(Float'Floor(2.0 * Random(G))) = 1 then
               MoveType := 1;  -- Left
            else
               MoveType := 2;  -- Right
            end if;
         end if;

    
    -- wait for initialisations of the remaining tasks:
    accept Start do
      null;
    end Start;

    for Step in 0..Nr_of_Steps loop
      delay Constants.Min_Delay + (Constants.Max_Delay - Constants.Min_Delay) * Duration(Random(G));
      if not Make_Step then
      exit;  -- Exit the loop if Make_Step returns false
      end if;
      Store_Trace;
      Time_Stamp := To_Duration(Clock - Start_Time);
    end loop;

    Printer.Report(Traces);
  end Traveler_Task_Type;

  -- Data for main task
  Travel_Tasks: array (0..Constants.Nr_Of_Travelers-1) of Traveler_Task_Type;
  Symbol: Character := 'A';
begin 
  -- Print the line with the parameters needed for display script:
  Put_Line(
      "-1 "&
      Integer'Image(Constants.Nr_Of_Travelers) &" "&
      Integer'Image(Constants.Board_Width) &" "&
      Integer'Image(Constants.Board_Height)      
    );

  -- init travelers tasks
  for I in Travel_Tasks'Range loop
    Travel_Tasks(I).Init(I, Seeds(I+1), Symbol);
    Symbol := Character'Succ(Symbol);
  end loop;

  -- start travelers tasks
  for I in Travel_Tasks'Range loop
    Travel_Tasks(I).Start;
  end loop;
end Travelers;