program fact;


{ calcule la factorielle d'un nombre,
c'est à dire {{ n! = 1*2*3*...*n }
function f(var n:integer):integer;
var
  res : integer;
  i : integer;
begin
  res:=1;
  i:=n;
  while i>=1 do
  begin
    res := res * i;
    i:=i-1;
  end;
  f:=res;
end;


var n : integer;
begin
  n := 6;
  writeln(f(n));
end.
