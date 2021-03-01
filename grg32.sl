%==========================================================================%
%   GRG 3.2 Startup File                 (C) 1988-2000  Vadim V. Zhytnikov %
%==========================================================================%
% This file is distributed without any warranty. You may modify it but you %
% are not allowed to remove author's name and/or distribute modified file. %
%==========================================================================%

(global '(![version!]))

(setq ![version!] "This is GRG 3.2 release 6 (July 16, 2000) ..." )

% Loading modules ...
(evload '(
  grgdecl
  grggeom
  grggrav
  grginit
  grgclass
  grgcomm
  grgcoper
  grgmain
  grgmater
  grgprin
  grgproc
  grgtrans
  grgcfg
))
(matrix nil)

(progn (terpri) (prin2 "Type ``grg;'' to start GRG ...") (terpri))

%==========================================================================%

