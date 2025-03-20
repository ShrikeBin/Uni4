with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Random_Seeds; use Random_Seeds;
with BoardMutex;
with Constants;
with Position_Type;
with Ada.Real_Time; use Ada.Real_Time;

procedure  Travelers is

-- Timing

  Start_Time : Time := Clock;  -- global startnig time

-- Random seeds for the tasks' random number generators
 
  Seeds : Seed_Array_Type(1..Constants.Nr_Of_Travelers) := Make_Seeds(Constants.Nr_Of_Travelers);

  -- elementary steps, in out means you can modify the input (in - read, out - modify)
  procedure Move_Down( Position: in out Position_Type.Position_Type ) is
  begin
    Position.Y := ( Position.Y + 1 ) mod Constants.Board_Height;
  end Move_Down;

  procedure Move_Up( Position: in out Position_Type.Position_Type ) is
  begin
    Position.Y := ( Position.Y + Constants.Board_Height - 1 ) mod Constants.Board_Height;
  end Move_Up;

  procedure Move_Right( Position: in out Position_Type.Position_Type ) is
  begin
    Position.X := ( Position.X + 1 ) mod Constants.Board_Width;
  end Move_Right;

  procedure Move_Left( Position: in out Position_Type.Position_Type ) is
  begin
    Position.X := ( Position.X + Constants.Board_Width - 1 ) mod Constants.Board_Width;
  end Move_Left;

  -- traces of travelers
  type Trace_Type is record 	      
    Time_Stamp:  Duration;	      
    Id : Integer;
    Position: Position_Type.Position_Type;      
    Symbol: Character;	      
  end record;	      

  type Trace_Array_type is  array(0 .. Constants.Max_Steps) of Trace_Type;

  type Traces_Sequence_Type is record
    Last: Integer := -1;
    Trace_Array: Trace_Array_type ;
  end record; 


  procedure Print_Trace( Trace : Trace_Type ) is
    Symbol : String := ( ' ', Trace.Symbol );
  begin
    Put_Line(
        Duration'Image( Trace.Time_Stamp ) & " " &
        Integer'Image( Trace.Id ) & " " &
        Integer'Image( Trace.Position.X ) & " " &
        Integer'Image( Trace.Position.Y ) & " " &
        ( ' ', Trace.Symbol ) -- print as string to avoid: '
      );
  end Print_Trace;

  procedure Print_Traces( Traces : Traces_Sequence_Type ) is
  begin
    for I in 0 .. Traces.Last loop
      Print_Trace( Traces.Trace_Array( I ) );
    end loop;
  end Print_Traces;

  -- task Printer collects and prints reports of traces
  task Printer is
    entry Report( Traces : Traces_Sequence_Type );
  end Printer;
  
  task body Printer is 
   All_Complete : Boolean := False;
  begin
    for I in 1 .. Constants.Nr_Of_Travelers loop -- range for TESTS !!!
        accept Report( Traces : Traces_Sequence_Type ) do
          Print_Traces( Traces );
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
    G : Generator;
    Traveler : Traveler_Type;
    Time_Stamp : Duration;
    Nr_of_Steps: Integer;
    Traces: Traces_Sequence_Type; 

    procedure Store_Trace is
    begin  
      Traces.Last := Traces.Last + 1;
      Traces.Trace_Array( Traces.Last ) := ( 
          Time_Stamp => Time_Stamp,
          Id => Traveler.Id,
          Position => Traveler.Position,
          Symbol => Traveler.Symbol
        );
    end Store_Trace;
    
    procedure Make_Step is
      N : Integer; 
      New_Position : Position_Type.Position_Type;
    begin
      N := Integer( Float'Floor(4.0 * Random(G)) );    
      case N is
         when 0 => 
            New_Position := Traveler.Position;
            Move_Up( New_Position );  -- Get the new position
         when 1 => 
            New_Position := Traveler.Position;
            Move_Down( New_Position );  -- Get the new position
         when 2 => 
            New_Position := Traveler.Position;
            Move_Left( New_Position );  -- Get the new position
         when 3 => 
            New_Position := Traveler.Position;
            Move_Right( New_Position );  -- Get the new position
         when others => 
            Put_Line( " ?????????????? " & Integer'Image( N ) );
            return;
      end case;
      -- Now check if the new position is occupied
      if not BoardMutex.Board.Is_Occupied(New_Position) then
         BoardMutex.Board.Move_Traveler(New_Position, Traveler.Id);
         Traveler.Position := New_Position; 
      end if;
    end Make_Step;

  begin
    accept Init(Id: Integer; Seed: Integer; Symbol: Character) do
      Reset(G, Seed); 
      Traveler.Id := Id;
      Traveler.Symbol := Symbol;
      -- Random initial position:
      Traveler.Position := (
          X => Integer( Float'Floor( Float( Constants.Board_Width )  * Random(G)  ) ),
          Y => Integer( Float'Floor( Float( Constants.Board_Height ) * Random(G) ) )          
        );
      Store_Trace; -- store starting position
      -- Number of steps to be made by the traveler  
      Nr_of_Steps := Constants.Min_Steps + Integer( Float(Constants.Max_Steps - Constants.Min_Steps) * Random(G));
      -- Time_Stamp of initialization
      Time_Stamp := To_Duration ( Clock - Start_Time ); -- reads global clock
    end Init;
    
    -- wait for initialisations of the remaining tasks:
    accept Start do
      null;
    end Start;

    for Step in 0 .. Nr_of_Steps loop
      delay Constants.Min_Delay+(Constants.Max_Delay-Constants.Min_Delay)*Duration(Random(G));
      -- do action ...
      Make_Step;
      Store_Trace;
      Time_Stamp := To_Duration ( Clock - Start_Time ); -- reads global clock
    end loop;
    Printer.Report( Traces );
  end Traveler_Task_Type;


-- Data for main task

  Travel_Tasks: array (0 .. Constants.Nr_Of_Travelers-1) of Traveler_Task_Type; -- for tests
  Symbol : Character := 'A';
begin 
  
  -- Prit the line with the parameters needed for display script:
  Put_Line(
      "-1 "&
      Integer'Image( Constants.Nr_Of_Travelers ) &" "&
      Integer'Image( Constants.Board_Width ) &" "&
      Integer'Image( Constants.Board_Height )      
    );

  -- init tarvelers tasks
  for I in Travel_Tasks'Range loop
    Travel_Tasks(I).Init( I, Seeds(I+1), Symbol );
    Symbol := Character'Succ( Symbol );
  end loop;

  -- start tarvelers tasks
  for I in Travel_Tasks'Range loop
    Travel_Tasks(I).Start;
  end loop;

end Travelers;

