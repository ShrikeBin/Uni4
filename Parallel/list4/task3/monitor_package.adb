-- Copyright (C) 2006 M. Ben-Ari. See copyright.txt
package body Monitor_Package is

  task body Monitor is
  begin
    loop
      select
        accept Enter;
      or
        terminate;
      end select;

      select
        accept Leave;
      or
        terminate;
      end select;
    end loop;
  end Monitor;

  task body Condition is
    Pending : Integer := 0; -- number of pending processes in Wait
  begin
    loop
      select
        when Pending = 0 =>
          accept Signal do
            Monitor.Leave;
          end Signal;
      or
        accept Pre_Wait do
          Pending := Pending + 1;
        end Pre_Wait;
      or
        accept Wait do
          loop
            select
              accept Signal;
                Pending := Pending - 1;
              exit;
            or
              accept Waiting(P: out Boolean) do
                P := True;
              end Waiting;
            or
              accept Pre_Wait do
                Pending := Pending + 1;
              end Pre_Wait;
            or
              terminate;
            end select;
          end loop;
        end Wait;
      or
        accept Waiting(P: out Boolean) do
          P := Pending /= 0;
        end Waiting;
      or
        terminate;
      end select;
    end loop;
  end Condition;

  function Non_Empty(Cond: Condition) return Boolean is
    P: Boolean;
  begin
    Cond.Waiting(P);
    return P;
  end Non_Empty;

  procedure Wait(Cond: in out Condition) is
  begin
    Cond.Pre_Wait;
    Monitor.Leave;
    Cond.Wait;
  end Wait;

end Monitor_Package;