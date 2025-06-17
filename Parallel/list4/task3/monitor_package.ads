-- Copyright (C) 2006 M. Ben-Ari. See copyright.txt
package Monitor_Package is

  task Monitor is
    entry Enter;
    entry Leave;
  end Monitor;

  task type Condition is
    entry Pre_Wait;
    entry Signal;
    entry Wait;
    entry Waiting(P: out Boolean);
  end Condition;

  function Non_Empty(Cond: Condition) return Boolean;

  procedure Wait(Cond: in out Condition);

end Monitor_Package;