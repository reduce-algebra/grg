off echo$
% This file is the part of GRG 3.2 (C) 1997  V.V.Zhytnikov
lisp$
begin
scalar psl,low,lis,cas,ok;
psl := getd 'dskin;
low := getd '!c!a!r;
if psl then lis := "PSL" else lis:= "CSL";
if low then cas := "Lower" else cas := "Upper";
prin2 "This REDUCE is based on ";
prin2 lis;
prin2 " and is ";
prin2 cas;
prin2 "-Cased.";
terpri();
if low then <<
  prin2 "Use lower-case symbols for built-in constans and functions:";
  terpri();
  prin2 "   e  i  pi  sin  cos  log  ..."; >>
else <<
  prin2 "Use upper-case symbols for built-in constans and functions:";
  terpri();
  prin2 "   E  I  PI  SIN  COS  LOG  ..."; >>;
terpri();
terpri();
end$
algebraic$
end;
