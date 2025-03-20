-- board.adb
with Position_Type; -- Import Position_Type
with Constants; -- Import Constants
package body BoardMutex is
   protected body Board is
      occupied: array (0 .. Constants.Board_Width - 1, 0 .. Constants.Board_Height - 1) of Integer :=
         (others => (others => -1)); 

      procedure Move_Traveler(Position: in out Position_Type.Position_Type; Traveler_Id: Integer) is
      begin
         if occupied(Position.X, Position.Y) = -1 then
            occupied(Position.X, Position.Y) := Traveler_Id;
         else -- if more than max delay, shut down, still needs to be implemented
            delay Constants.Max_Delay;
            if occupied(Position.X, Position.Y) = -1 then
               occupied(Position.X, Position.Y) := Traveler_Id;
            end if;
         end if;
      end Move_Traveler;

      function Is_Occupied(Position: Position_Type.Position_Type) return Boolean is
      begin
         return occupied(Position.X, Position.Y) /= -1;
      end Is_Occupied;
   end Board;
end BoardMutex;
