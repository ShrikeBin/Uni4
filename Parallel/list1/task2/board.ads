-- board.ads
with Position_Type;
package Board is
   -- Declare the protected object and its methods
   protected Board is
      procedure Move_Traveler(Position : in out Position_Type.Position_Type; Traveler_Id : Integer);
      function Is_Occupied(Position : Position_Type.Position_Type) return Boolean;
   end Board;
end Board;
