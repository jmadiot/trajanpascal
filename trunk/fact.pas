program fact;

var n : integer;
r : integer;

{ calcule la factorielle d'un nombre,
c'est à dire {{ n! = 1*2*3*...*n }
procedure f(n : integer; var res : integer);
var
  i : integer;
begin
  res:=1;
  i:=n;
  while i>=1 do
  begin
    res := res * i;
    i:=i-1;
  end;
end;


begin
  n := 6;
  f(n, r);
  writeln(r);
end.
