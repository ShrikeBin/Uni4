-- constants.ads
package Constants is

-- Travelers moving on the board

  Nr_Of_Travelers : constant Integer :=15;

  Min_Steps : constant Integer := 10 ;
  Max_Steps : constant Integer := 100 ;

  Min_Delay : constant Duration := 0.01;
  Max_Delay : constant Duration := 0.05;
  Deadlock_Delay : constant Duration := 0.1;

-- 2D Board with torus topology

  Board_Width  : constant Integer := 15;
  Board_Height : constant Integer := 15;

end Constants;
