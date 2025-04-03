with Constants;

package Position_Type is
   type Position_Type is record
      X: Integer range 0 .. Constants.Board_Width - 1;
      Y: Integer range 0 .. Constants.Board_Height - 1;
   end record;
end Position_Type;