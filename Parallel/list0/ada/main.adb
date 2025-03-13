with Ada.Text_IO;       
with Ada.Integer_Text_IO;  
use Ada.Text_IO;        
use Ada.Integer_Text_IO;

procedure Main is
    n : Integer;
    result : Integer;

    function Factorial (n : Integer) return Integer is
    begin
        if n = 0 then
            return 1;
        else
            return n * Factorial (n - 1);
        end if;
    end Factorial;

begin
   Put("Enter n: ");
   Get(Item => n);
   
   result := Factorial(n);

   Put_Line(Integer'Image(n) & "! is " & Integer'Image(result));
end Main;
