%==========================================================================%
%   GRG 3.2 Compilation [PSL]              (C) 1988-96  Vadim V. Zhytnikov %
%==========================================================================%
% This file is distributed without any warranty. You may modify it but you %
% are not allowed to remove author's name and/or distribute modified file. %
%==========================================================================%

% Set here amount of required free BPS or nil ...
(setq free!-bps!-size 45000)


(progn
  (terpri)
  (prin2 "Compiling GRG 3.2, wait few minutes.")                  (terpri)
  (prin2 "After several `*** Init code length is #'")             (terpri)
  (prin2 "messages the compilation should be completed.")         (terpri)
  (prin2 "Watch possible error messages preceded by `*****' ...") (terpri)
  (terpri)
  (wrs (open "grgcomp.log" 'output))
)


(de compile!-file!> (bin src)
  (prog (wcc)
    (setq wcc (wrs nil))
    (prin2 "Compiling `") (prin2 bin) (prin2 "' ...") (terpri)
    (wrs wcc)
    (terpri) (prin2 "### Compiling `") (prin2 bin) (prin2 "' ...") (terpri)
    (setq !*comp t)
    (faslout bin)
    (dskin src)
    (faslend)
    (setq !*comp nil)
    ))


% Loading compiler ...
(load compiler)

% Do we need symget.dat ?
% (cond
%   ((and (getd 'filep) (filep "$reduce/util/symget.dat"))
%     (dskin "$reduce/util/symget.dat") ))


% Enlarging BPS if necessary ...
(cond
  ((and free!-bps!-size (getd 'set!-bps!-size) (getd 'free!-bps)
        (lessp (free!-bps) free!-bps!-size))
     (set!-bps!-size free!-bps!-size)))

(dskin  "grgmacro.sl" )
(dskin  "grgdecl.sl"  )

(compile!-file!>  "grg"       "grg.sl"      )
(compile!-file!>  "grg32"     "grg32.sl"    )
(compile!-file!>  "grgdecl"   "grgdecl.sl"  )
(compile!-file!>  "grggeom"   "grggeom.sl"  )
(compile!-file!>  "grggrav"   "grggrav.sl"  )
(compile!-file!>  "grginit"   "grginit.sl"  )
(compile!-file!>  "grgclass"  "grgclass.sl" )
(compile!-file!>  "grgcomm"   "grgcomm.sl"  )
(compile!-file!>  "grgcoper"  "grgcoper.sl" )
(compile!-file!>  "grgmain"   "grgmain.sl"  )
(compile!-file!>  "grgmater"  "grgmater.sl" )
(compile!-file!>  "grgprin"   "grgprin.sl"  )
(compile!-file!>  "grgproc"   "grgproc.sl"  )
(compile!-file!>  "grgtrans"  "grgtrans.sl" )
(compile!-file!>  "grgcfg"    "grgcfg.sl"   )

(progn
  (terpri) (prin2 "### All done.") (terpri)
  (wrs nil)
  (terpri)
  (prin2 "GRG has been compiled.")                            (terpri)
  (prin2 "Move all created grg*.b files in the $reduce/fasl") (terpri)
  (prin2 "directory or keep them in your working directory.") (terpri)
)

(bye)

%==========================================================================%
