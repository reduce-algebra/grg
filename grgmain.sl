%==========================================================================%
%   GRGmain.sl                                         Main GRG Functions  %
%==========================================================================%
%   GRG 3.2 Standard Lisp Source Code     (C) 1988-2000 Vadim V. Zhytnikov %
%==========================================================================%
% This file is distributed without any warranty. You may modify it but you %
% are not allowed to remove author's name and/or distribute modified file. %
%==========================================================================%

%-------  Reduce Entry Points used in GRG --------------------------------
%
%   reval  aeval
%
%   writepri :  varpri    -  Reduce 3.3, 3.4, 3.4.1, 3.5
%               assgnpri  -  Reduce 3.6, 3.7
%
%   on  off  !~on  !~off
%
%   operator  remopr  depend  nodepend
%
%   order  factor  remfac
%
%   forall  !~let  let   match   !~clear  clear  let00  match00
%
%   seprp  printprompt
%
%-------------------------------------------------------------------------


%----- Main Function and Sturtup Procedures ------------------------------

% Really Main function. Just puts GRG> into ERRORSET ...
(de grg nil (errorset '(grg!>) nil nil))
%(de grg nil (grg!>)) % May be helpful for debuggin ...


(de grg!> nil
  (proc (w wtasknum)
       (setq wtasknum 0)
    % Banner ...
       (terpri) (prin2 ![version!]) (terpri)(terpri)
    % Initial settings which can be overridden later in `grg.cfg' ...
       (setq !*gc nil)
       (setq !*raise nil)
       (setq ![origlower!] !*lower)
       (setq !*lower nil)
       (setq ![lower!] (islowercase!>))
       (setq ![fldtuned!] nil)
       (setq ![erst1!] nil)
       (setq ![erst2!] nil)
       (cond ((null ![wf!])
         (setq ![wf!] '(!a !b !c !d !e !f !g !h))
         (setq ![wi!] '(!i !j !k !l !m !n !o !p !q))
         (setq ![wh!] '(!x !y !z !u !v !w !r !s !t))
         (setq ![ws!] '(!A !B !C !D !E !F !G !H !M !N !P !Q))
         (makeloop!> ![wf!])
         (makeloop!> ![wh!])
         (makeloop!> ![wi!])
         (makeloop!> ![ws!]) ))
       (tuneos!>) % trying set [dirsep] and [syscall]
    % First Init of GRG switches ...
       (initflags!>)
    % Trying to get standard input directory from environment ...
       (cond ((getd 'getenv) (progn
	 (setq w (errorset '(getenv "grg") nil nil))
	 (cond ((equal w '(nil)) (setq w (errorset '(getenv "GRG") nil nil))))
	 (cond ((atom w) (setq w nil))
	       (t (setq ![grgdir!] (cdr (reverse (explode
                    (setq ![grgdir1!] (car w)))))))))))
       (cond ((and ![dirsep!] ![grgdir!])
         (setq ![grgdir!] (cons ![dirsep!] ![grgdir!]))))
       (cond (![grgdir1!] (progn
	 (prin2 "System directory: ")
         (prin2 ![grgdir1!]) (terpri))))
    % Input `grg.cfg' file ...
       (ingrgsys!>)
       (setq ![flaghis!] nil)
       (saveflago!>)
    % Initial Settings Printing ...
       (showcase!>)
       (sdimsgn!>)
    % Absolute initial settings after `grg.cfg' ...
       (setq ![ttime!] (time)) % Overall time
       (setq ![tgctime!] (gctime)) % GC time
       (setq ![dim0!] ![dim!])
       (setq ![sgn0!] ![sgn!])
       (initial0!>)
    % Main Loop ...
    (loop!>
      (terpri)
      (cond ((eqn wtasknum 0) (setq w '!1))
	    (t                (setq w (asker!>
                                 '(
                                    "    Quit GRG       - 0"
	                            "    Start Task     - 1"
				    "    Exit to REDUCE - 2"
                                  )
                                 '( !0 !1 !2 ) ))
                              (terpri)  ))
      (setq promptstring!* "<- ")
      (cond ((iscsl!>) (setpchar promptstring!*)))
      (xrprompt!>)
      (setq wtasknum (add1 wtasknum))
      (setq w (cond
                ((eq w '!0) '(grgquit!>))
                ((eq w '!1) '(proceed!>))
                ((eq w '!2) '(grgexit!>))
                (t nil)))
      (cond (w (progn
        (setq w (errorset!> w ![erst1!] ![erst2!]))
        (cond
          ((atom w) (progn (terpri) (erm!> w) (terpri)))
	  ((equal w '(!!exit!!)) (return nil))
          )))))))


(de xrprompt!> nil
  (cond ((or (getd 'x!-pr!!) (getenv "redfront"))
    (setq promptstring!* (compress
      (append
        (list2 '!" (int2id 1))
        (append
          (reverse (cdr (reverse (cdr (explode promptstring!*)))))
          (list2 (int2id 2) '!"))))))))


% In `grg.cfg' file ...
(de ingrgsys!> nil
  (prog (w cn)
    (setq !*lower t)
    (setq !*raise t)
    (setq cn (grgopeninput!> "grg.cfg"))
    (cond ((atom cn)
      (setq !*lower nil)
      (setq !*raise nil)
      (return nil)))
    (rds (car cn))
    lab1
    (setq w (errorset '(read) nil nil))
    (cond ((atom w) (progn (erm!> 8802) (go lab2))))
    (cond ((equal w '(nil)) (go lab2)))
    (setq w (errorset (car w) nil nil))
    (cond ((atom w) (progn (erm!> 8802) (go lab2))))
    (go lab1)
    lab2
    (rds nil)
    (close (car cn))
    (setq !*lower nil)
    (setq !*raise nil)
    ))

% First init of switches in the session ...
(de initflags!> nil
  (progn
    (gprinreset!>)
    (cond
      ((and (fancyexist!>) (fancyloaded!>) (fancyon!>))
        (tunefancy!> t)))
    (cond
      ((and (getd 'x!-pr!!) (fancyexist!>))
        (on0!> '(fancy))))
    (setq ![flaghis!] nil)
    (foreach!> x in ![flagnil!] do (set x nil))
    (foreach!> x in ![flagt!]   do (set x t))
))

% Saves the initial setting of output mode switch ...
(de saveflago!> nil
  (prog (w)
    (setq w (cond
      (!*latex      'latex     )
      ((fancyon!>)  'fancy   )
      (!*grg        'grg     )
      (!*reduce     'reduce  )
      (!*maple      'maple   )
      (!*math       'math    )
      (!*macsyma    'macsyma ) ))
    (setq ![iflago!] w)))


%----- Main Loop ---------------------------------------------------------

% Start new Task ...
(de proceed!> nil
  (progn
    (initial0!>)
    (rund!>) ))

% Continue old Task ...
(de continue!> nil
  (prog2
    (setq ![er!] nil)
    (rund!>)))


%----- Some General Commands ---------------------------------------------

(de copyrzw!> nil
  (progn
    (terpri)
    (prin2 ![version!]) (terpri)
    (prin2 "(C) 1988-96  Vadim V. Zhytnikov ")
    (terpri) (terpri)))

% The System ; command.
% Temporary exit to OS ...
(de grgsystem!> (lst)
  (cond
    ((null lst) % System;
      (cond
        ((eqn ![syscall!] 1) % Via system ...
	  (progn
            (setq lst (errorset '(system) nil nil))
	    (cond ((atom lst) (prog2 (setq ![er!] 1104) !!er!!)))))
        ((eqn ![syscall!] 2) % via quit ...
	  (quit))
        (t (msg!> 1102))))  % Not supported
    ((and (stringp(car lst)) (null(cdr lst))) % System "...";
      (cond
        ((or (eqn ![syscall!] 1) (eqn ![syscall!] 2)) % Trying system ...
	  (progn
            (setq lst (errorset (list 'system (car lst)) nil nil))
	    (cond ((atom lst) (prog2 (setq ![er!] 1104) !!er!!)))))
        (t (msg!> 1102))))  % Not supported
    (t (prog2 (setq ![er!] 1103) !!er!!))))

% The Quit; Command and related operations ...
(de grgquit!> nil
  (progn
    (closeunload!>)
    (grgstat!>)
    (closewrite!>)
    (bye) ))

(de grgexit!> nil
  (prog nil
    (closeunload!>)
    (grgstat!>)
    (closewrite!>)
    (setq !*raise t)
    (setq !*lower ![origlower!])
    (prin2 "Exiting. Type ``grg;'' to restart GRG ...")(terpri)
    (return '!!exit!!)
    ))

% Statistics printing ...
(de grgstat!> nil
  (prog (wt wgt)
    (setq wt  (difference (time) ![ttime!]))
    (setq wgt (difference (gctime) ![tgctime!]))
    (cond ((iscsl!>) (setq wt (plus wt wgt))))
    (cond ((not(eqn wt 0)) (setq wgt (quotient (times 100 wgt) wt)))
          (t               (setq wgt 0)))
    (terpri)
    (prin2 "Overall Session time: ")
    (prtime!> wt)
    (cond ((zerop wt) (prog2 (terpri) (return nil))))
    (prin2 " (")
    (prin2 wgt)
    (prin2 "%GC)")
    (terpri)))


%------  Messages -------------------------------------------------------

% Error messages ...
(de erm!> (w)
  (proc (lst wm)
    (cond ((null w) (return nil)))
    (closewrite!>)
    (setq lst '(
(1000 . "ERROR: User interrupt.")
(1100 . "ERROR: Incorrect parameter of the command.")
(1101 . "ERROR: Coordinates already exist.")
(1103 . "ERROR: String is expected as a parameter.")
(1104 . "ERROR: Command failed.")
(2001 . "ERROR: Missing parameter or closing bracket in [,].")
(2002 . "ERROR: First parameter of _| must be a vector.")
(2003 . "ERROR: Second parameter of _| must be a form.")
(20021 . "ERROR: First parameter of | must be a vector.")
(20031 . "ERROR: Second parameter of | must be a scalar.")
(2004 . "ERROR: Exterior differential of a vector is impossible.")
(2005 . "ERROR: Parameters of /\ must be exterior forms.")
(2006 . "ERROR: Parameters of [,] must be vectors.")
(2007 . "ERROR: Dualization of a vector is impossible.")
(2008 . "ERROR: Form or vector is invalid in ^.")
(2009 . "ERROR: Zero denominator.")
(2010 . "ERROR: At lest one parameter of * must be a scalar.")
(2011 . "ERROR: Division on form or vector is impossible.")
(2012 . "ERROR: Terms of different type in A+B or A-B.")
(2013 . "ERROR: X must be a coordinate in @ X.")
(2014 . "ERROR: Missing operation.")
(2015 . "ERROR: Missing parameter of unary operation.")
(2016 . "ERROR: Missing parameter of operation.")
(2017 . "ERROR: Missing summand.")
(2018 . "ERROR: Unrecognized identifier.")
(20181 . "ERROR: Unrecognized variable.")
(2019 . "ERROR: String in expression.")
(2020 . "ERROR: Incorrect parameters list.")
(2021 . "ERROR: Incorrect function or missing operation.")
(2022 . "ERROR: Unrecognized function.")
(2023 . "ERROR: Form or vector as an argument of function is invalid.")
(20231 . "ERROR: Form or vector valued index is invalid.")
(2030 . "ERROR: Vector or 1-form are expected in scalar product.")
(2100 . "ERROR: Wrong type of expression.")
(2101 . "ERROR: Wrong identifier of object.")
(2102 . "ERROR: Incorrect indices.")
(21022 . "ERROR: Index out of range.")
(21023 . "ERROR: Number of indices is less than expected.")
(21024 . "ERROR: Number of indices is more than expected.")
(2103 . "ERROR: Incorrect Sum() or Prod() expression.")
(21031 . "ERROR: Wrong iteration variable specification.")
(2104 . "ERROR: Strange variable.")
(2105 . "ERROR: Wrong number of parameters.")
(2106 . "ERROR: Wrong parameter's value.")
(2108 . "ERROR: Incorrect min or max in iteration specification.")
(2110 . "ERROR: ~~ can be used as expr+~~ or expr-~~ only.")
(2113 . "ERROR: No Solutions are defined.")
(2114 . "ERROR: There is no Solution with this number.")
(2115 . "ERROR: 0 = 0 relation is invalid here.")
(2200 . "ERROR: Incorrect tensorial assignment.")
(2201 . "ERROR: Incorrect Coordinates or Constants declaration.")
(2202 . "ERROR: Wrong commas.")
(2203 . "ERROR: Coordinates does not match Dimension.")
(2204 . "ERROR: Incorrect assignment.")
(2205 . "ERROR: Repeated index in LHS.")
(2206 . "ERROR: Incorrect indices in tensorial assignment.")
(2207 . "ERROR: Wrong number of indices.")
(22071 . "ERROR: Unrecognized object.")
(2208 . "ERROR: Equation is expected at RHS.")
(2209 . "ERROR: Types of RHS and LHS differ.")
(2300 . "ERROR: Incorrect Solve command.")
(2301 . "ERROR: Solve failed.")
(2303 . "ERROR: Non equation in Solve.")
(2304 . "ERROR: Empty or trivial equations in Solve.")
(2400 . "ERROR: Incorrect boolean expression.")
(2410 . "ERROR: Unknown object name.")
(2420 . "ERROR: Unknown switch.")
(2500 . "ERROR: Incorrect Lie derivative.")
(2501 . "ERROR: Vector is expected in Lie derivative.")
(2502 . "ERROR: Cannot calculate Lie derivative of noncovariant object.")
(2600 . "ERROR: Incorrect covariant differential Dc.")
(2602 . "ERROR: Cannot calculate Dc of noncovariant object.")
(2603 . "ERROR: Wrong specification of alternative connection in Dc.")
(2700 . "ERROR: Incorrect covariant derivative Dfc.")
(2701 . "ERROR: Vector is expected in covariant derivative.")
(2702 . "ERROR: Cannot calculate Dfc of noncovariant object.")
(2703 . "ERROR: Wrong specification of alternative connection in Dfc.")
(2704 . "ERROR: Dfc of form or vector is impossible.")
(3000 . "ERROR: Object already exists.")
(3001 . "ERROR: Wrong type of indices for connection 1-form.")
(3002 . "ERROR: Connection must be 1-form valued.")
(4000 . "ERROR: Zero volume element.")
(4001 . "ERROR: Rotation Matrix isn't specified.")
(5012 . "ERROR: Incorrect Functions declaration.")
(5013 . "ERROR: Identifier already used.")
(50130 . "ERROR: This Identifier can't be used in GRG.")
(5016 . "ERROR: Incorrect function dependence list.")
(5100 . "ERROR: Generic Functions are not supported.")
(5101 . "ERROR: Incorrect Generic Function declaration.")
(6030 . "ERROR: Unrecognized object.")
(6042 . "ERROR: Incorrect command.")
(6043 . "ERROR: Unrecognized way of calculation.")
(6044 . "ERROR: Incorrect compound command structure.")
(6046 . "ERROR: Too few data.")
(6100 . "ERROR: Bad bracket count.")
(6200 . "ERROR: Incorrect Asy/Sy/Cy expression.")
%(6201 . "ERROR: Limits does not supported.")
%(6202 . "ERROR: Incorrect Limit expression.")
%(6203 . "ERROR: Form or Vector as a limiting point in Lim.")
(6204 . "ERROR: Incorrect SUB() expression.")
(6205 . "ERROR: Form or Vector in SUB().")
(6301 . "ERROR: Incorrect file name.")
(6321 . "ERROR: Can't open the file.")
(6402 . "ERROR: Unrecognized switch.")
(6500 . "ERROR: On TORSION is required.")
(6501 . "ERROR: On NONMETR is required.")
(6502 . "ERROR: On TORSION and On NONMETR is required.")
(6503 . "ERROR: On TORSION or On NONMETR is required.")
(6504 . "ERROR: Off NONMETR is required.")
(6505 . "ERROR: Off TORSION is required.")
(6506 . "ERROR: Off TORSION and Off NONMETR is required.")
(65002 . "ERROR: dim>2 is required.")
(650022 . "ERROR: dim=2 is required.")
(65003 . "ERROR: dim>3 is required.")
(65004 . "ERROR: dim>4 is required.")
(65005 . "ERROR: dim>5 is required.")
(6702 . "ERROR: Velocity is null.")
(6800 . "ERROR: Singular Metric or Inverse Metric.")
(6802 . "ERROR: Singular Frame or Vector Frame.")
(7200 . "ERROR: The file has format unknown for Load/Show.")
(7301 . "ERROR: Please specify Coordinates first.")
(7302 . "ERROR: Please specify Affine Parameter first.")
(7720 . "ERROR: File contains erroneous data.")
(7804 . "ERROR: Standard null metric is required for spinors.")
(78040 . "ERROR: dim=4 is required for spinors.")
(78041 . "ERROR: Standard null metric is required for spinorial rotation.")
(7805 . "ERROR: dim=4 is required.")
(7806 . "ERROR: Default diagonal metric is required.")
(7900 . "ERROR: The file contains other Dimension and/or Signature.")
(7910 . "ERROR: Signature -,+,+,+ or +,-,-,- is required for Null Metric.")
(8100 . "ERROR: Bad package name.")
(8102 . "ERROR: Cannot load the package.")
(8200 . "ERROR: Incorrect If( ) expression.")
(8201 . "ERROR: Non numeric argument in a relation.")
(8375 . "ERROR: Incorrect New Coordinates declaration.")
(8377 . "ERROR: Singular coordinates transformation.")
(8389 . "ERROR: Form or vector in old coordinates dependence list.")
(8388 . "ERROR: Recursive old coordinates dependence.")
(8400 . "ERROR: Singular Basis.")
(8401 . "ERROR: Singular Vector Basis.")
(8500 . "ERROR: Incorrect matrix.")
(8501 . "ERROR: The matrix isn't spinorial rotation.")
(8502 . "ERROR: The matrix isn't frame rotation.")
(8504 . "ERROR: Singular Matrix.")
(8600 . "ERROR: Incorrect New Object declaration.")
(8601 . "ERROR: Wrong type specification in the declaration.")
(8602 . "ERROR: Wrong indices specification in the declaration.")
(8604 . "ERROR: Identifier of new object contains digits or ~.")
(8606 . "ERROR: Wrong symmetry specification.")
(8709 . "ERROR: Incorrect Let command.")
(8710 . "ERROR: Zero is invalid in Let or Clear.")
(8711 . "ERROR: Form or vector in Let or Clear.")
(8712 . "ERROR: Incorrect For All command.")
(8713 . "ERROR: Incorrect For All command.")
(8714 . "ERROR: Incorrect parameters list in For All.")
(8800 . "ERROR: Dimension must be 2 or greater.")
(8801 . "ERROR: Dimension does not match Signature.")
(88011 . "ERROR: Incorrect Dimension declaration.")
(88012 . "ERROR: Dimension declaration must be first in session.")
(8802 . "ERROR: Incorrect data in the `grg.cfg' file.")
(8803 . "ERROR: Transformation was not properly completed.")
(9002 . "ERROR: Incorrect Signature in `grg.cfg' file.")
(9100 . "ERROR: Cannot classify form of vector.")
(9101 . "ERROR: Do not know how to classify this object.")
(9602 . "ERROR: Double ; delimiter.")
(9901 . "ERROR: Unexpected end of file.")
(9913 . "ERROR: Can't transform spinorial index to holonomic.")
(9999 . "ERROR: Cannot handle *SQ form in the expression.")
)) (while!> lst
      (cond ((eqn w (caar lst)) (setq wm (cdar lst))))
      (setq lst (cdr lst)))
    (cond (wm (prin2 wm) (terpri))
          (t  (prin2 "ERROR: ") (prin2 w) (terpri)
              (lowmemwarn!>) ))
    % If Batch mode then quitting ...
    (cond (!*batch
      (prinN2 "GRG is in Batch mode. Quitting ...")
      (terpri)
      (grgquit!>)))
))

% Messages ...
(de msg!> (w)
  (proc (lst wm)
    (cond ((null w) (return nil)))
    (setq lst '(
(100  . "WARNING: Macro tensor is not allowed in Find command.")
(1102 . "Command System; is not supported.")
(2104 . "WARNING: min > max in iteration.")
(2109 . "WARNING: Summation or iteration variable is already in use.")
(2112 . "WARNING: Manipulation with enumerating index is ignored.")
(2302 . "WARNING: No solutions found.")
(50131 . "WARNING: This Function can be used without declaration.")
(6700 . "WARNING: Null congruence is not actually null.")
(6701 . "WARNING: Null congruence is not geodesic.")
(6702 . "WARNING: Frenkel condition is not satisfied.")
(6801 . "Assuming Default Metric.")
(6803 . "Assuming Default Holonomic Frame.")
(6805 . "Assuming Default comoving Velocity.")
(6820 . "WARNING: Metric already exists.")
(7012 . "Basis can not be erased in anholonomic mode.")
(7630 . "WARNING: Coordinates have been redefined.")
(7631 . "WARNING: Loaded constants conflict with coordinates.")
(7632 . "WARNING: Loaded constants conflict with functions.")
(7633 . "WARNING: Loaded functions conflict with coordinates.")
(7634 . "WARNING: Loaded functions conflict with constants.")
(7635 . "WARNING: Loaded coordinates conflict with constants.")
(7637 . "WARNING: Loaded coordinates conflict with functions.")
(8101 . "WARNING: Package already loaded.")
(8391 . "Keeping Frame holonomic.")
(8392 . "Keeping Vector Frame holonomic.")
(8603 . "WARNING: Identifier already used.")
(8607 . "Same indices in different symmetry groups.")
(8701 . "WARNING: Unable to Forget built-in object.")
(88033 . "No guaranty for correct operation of the system. Better quit now!")
(8901 . "Fetching the file from System directory.")
(8902 . "Fetching `grg.cfg' file from System directory.")
(9001 . "WARNING: Velocity is not normalized.")
(9100 . "WARNING: Quite old REDUCE. All letters will be in lower case.")
(9101  . "WARNING: LaTeX output mode is not supported since GRG unable")
(91011 . "WARNING: to load `fmprint' package. Check that you have `fmprint.b'")
(91012 . "WARNING: file and copy it into your current directory or into the")
(91013 . "WARNING: directory where REDUCE usually looks for binary packages")
(91014 . "WARNING: (e.g. `$reduce/fasl/').")
)) (while!> lst
      (cond ((eqn w (caar lst)) (setq wm (cdar lst))))
      (setq lst (cdr lst)))
   (cond (wm (prin2 wm) (terpri))
         (t  (prin2 "WARNING: ") (prin2 w) (terpri)))
))


(de doub!> (w)
  (progn (closewrite!>)
         (prin2 w)
         (prin2 " - ? ")
         (terpri)))

(de doubs!> (w)
  (progn (closewrite!>)
         (prin1 w)
         (prin2 " - ? ")
         (terpri)))

(de doubl!> (lst)
  (progn (closewrite!>)
         (gprinreset!>)
         (gprin!> "`")
         (gprinwb!> lst)
         (gprin!> "'")(gprin!> " - ?")
         (gterpri!>)))

(de doubo!> (wi)
  (progn (closewrite!>)
         (gprinreset!>)
         (gprinwb!> (txt!> wi)) (gprin!> " - ?")
         (gterpri!>)))


% Warning about low memory ...
(de lowmemwarn!> nil
  (prog (wt wgt)
    (setq wt  (difference (time) ![time!]))
    (setq wgt (difference (gctime) ![gctime!]))
    (cond ((iscsl!>) (setq wt (plus wt wgt))))
    (cond ((not(eqn wt 0)) (setq wgt (quotient (times 100 wgt) wt)))
          (t               (setq wgt 0)))
    (cond
      ((and (lessp wgt 100) (greaterp wgt 39)) (progn
        (prin2 "Garbage Collections constitute ")
        (prin2 wgt)
        (prin2 "% of the total CPU time.")
        (terpri)
        (cond
          ((greaterp wgt 59) (prin2 "ATTENTION: Memory is exhausted!"))
          (t                 (prin2 "WARNING: Free memory is low!")))
        (terpri)
        )))
    ))

%------- Names of Built-In Objects --------------------------------------

% This gives the list - Name of an Object ...
(de txt!> (wi) % wi - internal variable
  (proc (w)
    (cond ((or (flagp wi '!+abbr)  (flagp wi '!+macros2))
            (return (idtxt!> wi))))
    (setq w ![datl!])
    (while!> w
      (cond ((eq wi (cadar w)) (return (lowertxt!> (caar w))))
            (t (setq w (cdr w)))))))

(de thetxt!> (wi) % wi - internal variable
  (proc(w)
    (cond ((or (flagp wi '!+abbr)  (flagp wi '!+macros2))
            (return (cons '!T!h!e (idtxt!> wi))))) % word!!!
    (setq w ![datl!])
    (while!> w
      (cond ((eq wi (cadar w)) (return (lowertxt!> (caar w))))
            (t (setq w (cdr w)))))))

(de lowertxt!> (w)
  (proc (wr wn)
    (while!> w
      (cond (wn (setq wr (cons (lowertxt0!> (car w) t) wr)))
	    (t  (setq wr (cons (lowertxt0!> (car w) nil) wr))
		(setq wn t)))
      (setq w (cdr w)))
    (return(reversip wr))))

(de lowertxt0!> (w wc)
  (cond ((not(idp w)) w)
	((get w '!=printas) (get w '!=printas))
	(t (proc (we wr)
	     (setq we (explode w))
	     (while!> we
	       (cond
                 ((liter (car we))
		    (cond (wc (setq wr (cons (tolc!>(car we)) wr)))
			  (t  (setq wr (cons (touc!>(car we)) wr))
			      (setq wc t))))
		 (t (setq wr (cons (car we) wr))))
	       (setq we (cdr we)))
	     (return(incom!>(reversip wr)))))))

% The name for a new Object created by user ...
(de idtxt!> (wi)
  (prog (w)
    (setq w (cdr (explode2 wi)))
    (return (ncons (incom!> w)))))

% Prints Object's name via GPRIN> ...
(de pn!> (wi) (gprils!> (txt!> wi)))
(de pn0!> (wi) (gprils0!> (txt!> wi)))
(de pn0dot!> (wi) (gprils0dot!> (txt!> wi)))
(de thepn!> (wi) (gprils!> (thetxt!> wi)))
(de thepn0!> (wi) (gprils0!> (thetxt!> wi)))


%-------   Functions for manipulation with whole data boxes  -----------

% Here:  LST  - the Box list;   WN - internal variable;
%        WI   - NIL at the beginning, the index list is collected here;
%        IDXL - IDXL list at the beginning;
%        FUN  - function  (FUN  W WI WN) here
%               W - element, WI - its indices, WN - intern. variable


% Apply FUN to each element in the LST ...
(de allcom!> (lst wn wi idxl fun)
  (cond((null idxl) (apply fun (list lst (reverse wi) wn)))
       (t(proc (wc) (setq wc -1)
           (while!> lst
             (setq wc (add1 wc))
             (allcom!> (car lst) wn (cons wc wi) (cdr idxl) fun)
             (setq lst(cdr lst)))))))

% Apply FUN to each element in LST and collect result ...
(de allcoll!> (lst wn wi idxl fun)
  (cond((null idxl) (apply fun (list lst (reverse wi) wn)))
       (t(proc (wc w) (setq wc -1)
           (while!> lst
             (setq wc (add1 wc))
             (setq w (cons
               (allcoll!> (car lst) wn (cons wc wi) (cdr idxl) fun) w))
             (setq lst(cdr lst)))
           (return (reverse w))
           ))))

%--------- Tracing messages ----------------------------------------------

% Sometning has/have been calculated ...
(de trsc!> (w wy)
 (cond(!*trace
  (prog (wm)
    (gprinreset!>)
    (setq ![gptab!] 2)
    (pn!> w)
    (cond
      ((null wy)
         (gprils0!> (cond
            ((flagp w '!+pl) '("calculated."))
            (t               '("calculated."))  )))
            %((flagp w '!+pl) '("have" "been" "calculated."))
            %(t               '("has" "been" "calculated."))  )))
      (t (gprils0!> (cond
            ((flagp w '!+pl) '("calculated"))
            (t               '("calculated"))  ))
            %((flagp w '!+pl) '("have" "been" "calculated"))
            %(t               '("has" "been" "calculated"))  ))
         (cond (wy (gprin!> '! ) (gprils0dot!> (lowertxt!> wy))))))
    (gprin!> '! )
    (gptime!>)))))
%(de trsc!> (w wy)
% (cond(!*trace
%  (progn (gprinreset!>)
%	 (setq ![gptab!] 2)
%         (pn!> w)
%         (gprils0!> (cond
%            ((flagp w '!+pl) '("have" "been" "calculated"))
%            (t               '("has" "been" "calculated")) ))
%         (cond(wy(prog2 (gprin!> '! ) (gprils0!> (lowertxt!> wy)))))
%         (gprin!> ". ")
%         (gptime!>)))))

% Done ...
(de done!> nil
 (cond(!*trace
  (progn (gprinreset!>)
         (gprils0!> '("Done: "))
         (gptime!>)))))

% Too few data ...
(de tfd!> (w)
 (progn (gprinreset!>)
	(setq ![gptab!] 2)
        (gprils!> '("Too" "few" "data" "for" "calculation" "of"))
        %(pn0!> w)(gprin!> ".")(gterpri!>)))
        (pn0dot!> w)(gterpri!>)))

% Failed to calculate ...
(de trsf!> (w)
 (progn (gprinreset!>)
	(setq ![gptab!] 2)
        (gprils!> '("Cannot" "calculate"))
        %(pn0!> w)(gprin!> ".")(gterpri!>)))
        (pn0dot!> w)(gterpri!>)))

% Already exists ...
(de aexp!> (w)
  (progn (gprinreset!>)
	 (setq ![gptab!] 2)
         (gprils!> '("Value" "of"))
         (pn!> w)
         (gprils0!> '("is" "known" "already."))
         (gterpri!>)))

% The value indefinite ...
(de abse!> (w)
  (progn (gprinreset!>)
	 (setq ![gptab!] 2)
         (gprils!> '("Value" "of"))
         (pn!> w)
         (gprils0!> '("is" "indefinite."))
         (gterpri!>)))

% Something can't be calculated ...
(de cantcalc!> (w)
  (progn (gprinreset!>)
	 (setq ![gptab!] 2)
         (thepn!> w)
         (gprin!> '("can't" "be" "calculated."))
         (gterpri!>)))

% Something can't be calculated by way WY ...
(de cantway!> (w wy)
  (progn (gprinreset!>)
	 (setq ![gptab!] 2)
         (thepn!> w)
         (gprin!> '("can't" "be" "calculated"))
         %(gprils0!> (lowertxt!> wy))(gprin!> ".")(gterpri!>) ))
         (gprils0dot!> (lowertxt!> wy))(gterpri!>) ))


%------ Initial Settings for a New Task ----------------------------------

% All system parameters resetting ...
(de initial0!> nil
  (progn
     (setq ![mtype!] nil)
     (setq ![mitype!] nil)
     (setq ![dtype!] nil)
     (setq ![ditype!] nil)
     (setq ![ftype!] nil)
     (setq ![fitype!] nil)
     (setq ![dim!] ![dim0!])
     (setq ![sgn!] ![sgn0!])
     (tunedim!>)
     (setq ![echo!] nil)
     (resetsubs!>) % Reset substitutions (before declarations!) ...
     (rempf!> ![rpfl!] '(1 2)) % Clear all declarations ...
     (setq ![gfun!] nil)
     % Clear all data values ...
     (foreach!> x in ![datl!] do
           (cond((atom(cadr x)) (prog2
              (set (cadr x) nil)
              (cond ((flagp (cadr x) '!+abbr) (forget1!>(cadr x))))))))
     (foreach!> x in ![abbr!] do
           (prog2 (set x nil) (forget1!> x)))
     (resetflags!>)  % Resetting switches ...
     (closeallo!>)   % Cloasing all files ...
     % Restoring default values of system variables ...
     (foreach!> x in
          '( ![solveq!] ![er!] ![wri!] ![chain!] ![unl!]
             ![pause!] ![fromf!]  ![loa!] ![umod!] ![way!]
             ![x!] ![ocord!] ![xb!] ![dfx!] ![dex!] ![lsrs!]
             ![xv!] ![ccb!] ![xf!] ![ccbi!] ![lwri!] ![lunl!]
             ![l!] ![la!] ![li!] ![dbas!] )
       do (set x nil))
     (setq ![lline!] 0)
     (gprinreset!>)
     (setq ![time!] (time))
     (setq ![gctime!] (gctime))
))

% This closes really all output files ...
(de closeallo!> nil
  (prog2
    (closeunload!>)
    (closewrite!>)  ))

% This closes global Write output ...
(de closewrite!> nil
  (progn
    (cond(![wri!] (close ![wri!])))
    (setq ![wri!] nil)
    (wrs nil)))

% This close global Unload ...
(de closeunload!> nil
  (progn
    (cond(![unl!](progn
      (wrs ![unl!])
      (print t)
      (wrs ![wri!])
      (close ![unl!]))))
    (setq ![unl!] nil)
    ))


% Resets all switches to initial values ...
(de resetflags!> nil
  (proc (w ww)
    (cond(![iflago!]
      (setq ![flaghis!]
        (append ![flaghis!]
          (ncons(cons ![iflago!] t))))))
    (while!> ![flaghis!]
      (setq w (car ![flaghis!]))
      (setq ww (makeswvar!> (car w)))
      (cond((not(equal (eval ww) (cdr w)))
        (cond ((flagp (car w) 'switch) % Reduce ...
		(cond ((cdr w) (eval (list 'on  (car w))))
		      (t       (eval (list 'off (car w)))))
                (onoff1!> (car w) (cdr w)))
              (t(onoff1!> (car w) (cdr w)))))) % GRG ...
      (setq ![flaghis!] (cdr ![flaghis!]))
    (cond((null ![iflago!]) (offallo!>)))
    )))

% Resets all substitutions ...
(de resetsubs!> nil
  (proc (w)
    (while!> ![sublist!]
      (setq w (car ![sublist!]))
      (errorset (list (car w) (list 'quote (cadr w)))
		![erst1!] ![erst2!])
      (setq ![sublist!] (cdr ![sublist!])))))

% Removes all Cord, Const and Fun declarations ...
(de rempf!> (lst wt)
  (proc (w x)
   (cond ((member 2 wt)
     (foreach!> xx in ![cord!] do (nodepend (cons xx ![apar!])))))
   (cond((member 1 wt)
     (foreach!> xx in ![fun!] do (prog2
       (cond((setq w(get xx '!=depend))(nodepend w)))
       (remopr xx)
       )) ))
   (while!> lst
    (setq x (car lst))
    (cond((setq w(eval(caar x)))
          (progn
           (cond((cadr x)
                 (foreach!> y in (cadr x) do (remflag w y))))
           (cond((cddr x)
                 (foreach!> y in (cddr x) do
                  (foreach!> z in w do (remprop z y))))))))
    (setq lst(cdr lst)))
  ))

%------ Tuning for dimension --------------------------------------------

(de tunedim!> nil
  (prog (w wa)
    (setq ![dim1!] (sub1 ![dim!]))
    (setq ![sigprod!] (sigprod!>))
    (put '!d!i!m   '!=sysconst ![dim!])
    (put '!s!i!g!n '!=sysconst ![sigprod!])
    (put '!s!g!n!t '!=sysconst ![sigprod!])
    (setq wa (ncons(cons 'a (dimlist1!> 1))))
    (put '!#!e!p!s '!=sidxl wa)
    (put '!#!e!p!s!i '!=sidxl wa)
    (put '!#!e!p!s!h '!=sidxl wa)
    (put '!#!e!p!s!i!h '!=sidxl wa)
    (put '!#!e!p!s '!=idxl (mks1!> ![dim1!] nil))
    (put '!#!e!p!s!i '!=idxl (mks1!> ![dim1!] t))
    (put '!#!e!p!s!h '!=idxl (mks1!> ![dim1!] 0))
    (put '!#!e!p!s!i!h '!=idxl (mks1!> ![dim1!] 1))
    (cond ((eqn ![sigprod!] -1) (put '!#!s!d!e!t!G '!=tex "\sqrt{-g}") )
          (t                    (put '!#!s!d!e!t!G '!=tex "\sqrt{g}")  ))
    ))


%------ Metric and Frame Type -------------------------------------------

% Determines Frame Type ...
%  [FTYPE]  NIL - unknown, 1 - holonomic, 2 - diag, 3 - general
(de ftype!> nil (ftype0!> !#!T '![ftype!]))
(de fitype!> nil (ftype0!> !#!D '![fitype!]))
(de ftype0!> (w wt)
  (cond
    (w (prog (wc wcc wod wnu) % wod - off diag, wnu - non unit
	 (cond (![umod!] (set wt 3) (return nil)))
         (fordim!> i do
	   (fordim!> j do
	     (progn
	       (setq wc (exprtype!>
                 (setq wcc (getfdx!> (getel1!> w i) j))))
	       (cond ((and (not(eqn i j)) wc) % off diag
                        (setq wod t)))
	       (cond ((and (eqn i j) (not(equal wcc 1))) % not unit
                        (setq wnu t))
               ))))
	 (cond ((and (null wod) (null wnu)) (set wt 1))
	       ((null wod) (set wt 2))
	       ( t (set wt 3)))))
    (t (set wt nil))))

% Determines Metric Type ...
%  [MTYPE]  NIL - unknown, 1 - null, 2 - diag, 3 - general
%  [DTYPE]  NIL - unknown, 1 - constant, 2 - general
(de mtype!> nil (mtype0!> !#!G '![mtype!] '![dtype!]))
(de mitype!> nil (mtype0!> !#!G!I '![mitype!] '![ditype!]))
(de mtype0!> (w wt wd)
  (cond
    (w (prog (wc wod wnc) % wod - off diag, wnc - non const
	 (cond
           ((and (equal ![sgn!] '(-1 1 1 1)) (equal w ![nullm!]))
	      (set wt 1) (set wd 1) (return t))
           ((and (equal ![sgn!] '(1 -1 -1 -1)) (equal w ![nullm1!]))
	      (set wt 1) (set wd 1) (return t)))
         (fordim!> i do
	   (fordim!> j do
	     (cond ((geq j i)
	       (progn
	         (setq wc (exprtype!> (getel2!> w i j)))
	         (cond ((and (not(eqn i j)) wc) % off diag
                         (setq wod t)))
	         (cond ((eqn wc 2) % non const
                         (setq wnc t)))
		 )))))
	 (cond ((not wnc) (set wd 1))
	       (t (set wd 2)))
	 (cond ((not wod) (set wt 2))
	       (t (set wt 3)))
	 (return t)))
    (t (set wt nil))))

% Determines expression type:
% NIL - zero, 1 - constant, 2 - general
(de exprtype!> (w)
  (cond ((null w) nil)
	(t (exprtype1!> w))))

(de exprtype1!> (w)
  (cond
    ((atom w)
      (cond
	((numberp w) 1)
        ((get w '!=cord) 2)
        ((get w '!=depend) (exprtype1!> (cons nil (cdr(get w '!=depend)))))
	(t 1)))
    (t(proc nil
	(setq w (cdr w))
	(while!> w
	  (cond ((eqn 2 (exprtype1!> (car w))) (return 2)))
	  (setq w (cdr w)))
	(return 1)))))


% [FTYPE]  NIL - unknown, 1 - holonomic, 2 - diag, 3 - general

% Frame holomonic ?
(de fholop!> nil
  (cond ((and ![ftype!] (eqn ![ftype!] 1)) t)
	(t nil)))
% Inverse Frame holomonic ?
(de ifholop!> nil
  (cond ((and ![fitype!] (eqn ![fitype!] 1)) t)
	(t nil)))

% This crucial predicate defines Holonomic Regime.
% In this case frame indixes are not differnt from
% holonomic ones. This is important in coordinates
% transformations and in the Dc/Lie covar. operations.
(de holonomicp!> nil
  (and !*holonomic                   % holonomic is on
       (not ![umod!])                % not if basis mode
       (or (null !#!T) (fholop!>))    % t is holonomic or absent
       (or (null !#!D) (ifholop!>)))) % d is holonomic or absent

% Frame diagonal ?
(de fdiagp!> nil
  (cond ((and ![ftype!] (eqn ![ftype!] 2)) t)
	(t nil)))
% Inverse Frame diagonal ?
(de ifdiagp!> nil
  (cond ((and ![fitype!] (eqn ![fitype!] 2)) t)
	(t nil)))


% [MTYPE]  NIL - unknown, 1 - null, 2 - diag, 3 - general
% [DTYPE]  NIL - unknown, 1 - constant, 2 - general

% Metric diagonal or null?
(de motop!> nil
  (cond ((and ![mtype!] (leq ![mtype!] 2)) t)
	(t nil)))
% Inverse Metric diagonal or null?
(de imotop!> nil
  (cond ((and ![mitype!] (leq ![mitype!] 2)) t)
	(t nil)))

% Null Metric ?
(de mnullp!> nil
  (cond ((and ![mtype!] (eqn ![mtype!] 1)) t)
	(t nil)))
(de imnullp!> nil
  (cond ((and ![mitype!] (eqn ![mtype!] 1)) t)
	(t nil)))

% Maps `diagonal' index to its adjacent ...
(de ai!> (wa)
  (cond ((eqn ![mtype!] 1)
          (cond ((eqn wa 1) 0)
                ((eqn wa 0) 1)
                ((eqn wa 2) 3)
                ((eqn wa 3) 2)))
         (t wa)))

% `Diagonal' element of Metric/Inverse Metric ...
(de diagm!>  (w) (getmetr!>  w (ai!> w)))
(de diagmi!> (w) (getimetr!> w (ai!> w)))

% Predicat of +--- version in the spinorial regime ...
(de pmmm!> nil (eqn (car ![sgn!]) 1))
(de mppp!> nil (eqn (car ![sgn!]) -1))



%------ Restrictors for Constrained Data Types and Ways ------------------

% Only dim=4 ...
(de ttt4!> nil
  (cond ((not(eqn ![dim!] 4)) 7805)
	(t nil)))

% We need affine parameter ...
(de tttapar!> nil
  (cond ((null ![apar!]) 7302)
	(t nil)))

% Need Torsion ...
(de tttq!> nil
  (cond ((null !*torsion) 6500)
        (t nil)))

% Need Nonmetricity ...
(de tttn!> nil
  (cond ((null !*nonmetr) 6501)
        (t nil)))

% Need Torsion or Nonmetricity ...
(de tttqorn!> nil
  (cond ((not(or !*torsion !*nonmetr)) 6503)
	(t nil)))

% Need Torsion and Nonmetricity ...
(de tttqandn!> nil
  (cond ((not(and !*torsion !*nonmetr)) 6502)
	(t nil)))

% Need Torsion but not Nonmetr ...
(de tttqnotn!> nil
  (cond ((not !*torsion) 6500)
	(!*nonmetr       6504)
	(t nil)))

% Need Off Nonmetr ...
(de tttnotn!> nil
  (cond (!*nonmetr       6504)
	(t nil)))

% Need Nonmetr but not Torsion ...
(de tttnnotq!> nil
  (cond ((not !*nonmetr) 6501)
	(!*torsion       6505)
	(t nil)))

% No Torsion and No Nonmetricity ...
(de tttnotqn!> nil
  (cond ((or !*nonmetr !*torsion) 6506)
	(t nil)))

% We need default diagonal metric ...
(de tttdiag!> nil
  (cond ((or (null !#!G) (null ![mtype!]) (null ![dtype!])) 7806)
	((not(eqn ![mtype!] 2)) 7806)
	((not(eqn ![dtype!] 1)) 7806)
	(t nil)))

% Spinorial restrictor ...
(de sp!> nil
  (cond ((not(eqn ![dim!] 4)) 78040)
        ((null !#!G) 7804)
	((null ![mtype!]) 7804)
        ((not(eqn ![mtype!] 1)) 7804)
	((and !#!G!I (not(eqn ![mitype!] 1))) 7804)
        (t nil)))

% Spinorial but NONMETR must be Off ...
(de sp!-n!> nil
  (cond (!*nonmetr 6504)
        ((not(eqn ![dim!] 4)) 78040)
        ((null !#!G) 7804)
	((null ![mtype!]) 7804)
        ((not(eqn ![mtype!] 1)) 7804)
	((and !#!G!I (not(eqn ![mitype!] 1))) 7804)
        (t nil)))

% dim>n restrictors ...
(de deq2!> nil (cond ((not(eqn ![dim!] 2)) 650022) (t nil)))
(de dg2!> nil (cond ((not(greaterp ![dim!] 2)) 65002) (t nil)))
(de dg3!> nil (cond ((not(greaterp ![dim!] 3)) 65003) (t nil)))
(de dg4!> nil (cond ((not(greaterp ![dim!] 4)) 65004) (t nil)))
(de dg5!> nil (cond ((not(greaterp ![dim!] 5)) 65005) (t nil)))


% Check consrtains for one object WI ...
(de constrp!> (wi)
  (cond ((null (setq wi (get wi '!=constr))) nil)
	(t (constrp1!> wi))))

(de constrp1!> (w) % w - list of constraints ...
  (cond ((null w) nil)
        ((eval(car w)) (eval(car w)))
        (t (constrp1!>(cdr w)))))

% Check constrains for list of objects ...
(de constrpl!> (lst)
  (cond ((null lst) nil)
	(t(prog (w)
	    (setq w (constrp!>(car lst)))
	    (cond (w (progn  (setq ![er!] w)
                             (doubo!>(car lst))
			     (return !!er!!)))
		  (t (return (constrpl!> (cdr lst)))))))))


%------ Main Data Calculation Algorithm ----------------------------------

% Main Data Calculation Recursive Algorithm. Returns:
%   !!ER!! - Some error in the process of calculation.
%    NIL   - Cannot calculate. Too few data or no any ways.
%     T    - Done.
(de request!> (nam)
  (cond
    ((eval nam) t)             % already exists ...
    ((memq nam ![chain!]) nil) % already in the chain ...
    ((constrp!> nam)           % constrained object ...
      (progn (doubo!> nam) (erm!>(constrp!> nam)) nil))
    (t(proc (w wa wy w1w)
	% trying to find method for calculation ...
        (cond((not ![way!]) (progn   % choosing way  ...
                 (setq w (get nam '!=way))
                 (cond ((null w) (return nil))) % no any way ...
		 (setq wa (mainway!> w))
                 (cond ((null wa) (setq wa (firstgoodway!> w))))
		 (cond ((null wa) (return nil))) % no any appropriate way ...
		 (setq w1w (car wa))
		 (setq wy  (caddr wa))
		 (setq w   (cdddr wa)) ))
             (t(progn                % alternative way ...
                 (setq w (get nam '!=way))
                 (cond((null w)  % no any ways for this object ...
                   (progn (setq ![er!] 6043) (doubl!> ![way!])
                          (setq ![way!] nil) (return !!er!!))))
                 (setq w (getthisway!> ![way!] w))
                 (cond
		   ((eq w !!er!!) (return !!er!!))
                   ((null w)  % unknown way ...
                     (progn (setq ![er!] 6043) (doubl!> ![way!])
                            (setq ![way!] nil) (return !!er!!))))
                 (cond((setq wa (constrp1!>(ncons(cadr w)))) % constr.way ...
                   (progn (cantway!> nam ![way!]) (setq ![way!] nil)
                          (setq ![er!] wa) (return !!er!!))))
                 (setq ![way!] nil)
                 (setq w1w (car w))
                 (setq wy  (caddr w))
                 (setq w   (cdddr w)))))
        % now: w - reqired data list, w1w - way name,
        %      wy - calculating call for ways
        (setq ![chain!] (cons nam ![chain!]))
        (while!> w  % request for data required for calculation ...
          (cond((and (pairp(car w)) (eval(caar w)))
                 (setq w (appmem!>(cdar w)(cdr w))) ) % new group ...
               ((and (pairp(car w)) (null(eval(caar w))))
                 (setq w (cdr w)) ))                  % skip group ...
          (tohead (or(null w)(pairp(car w))))
          (setq wa (request!>(car w)))
          (cond
            ((eq wa !!er!!) (return !!er!!))
            ((not wa) (progn (trsf!>(car w)) (return nil))))
          (setq w (cdr w)))
        (setq w (eval wy)) % calculation ...
	(cond((eq w !!er!!)(return !!er!!)))
        (trsc!> nam w1w) % successful calculation ...
	(return t)))))

% Seek main way if awailable ...
(de mainway!> (wl)
  (cond ((null wl) nil)
        ((and (not(eval(cadar wl))) (mainwayp!>(cdddar wl)))
           (car wl))
        (t (mainway!> (cdr wl)))))

(de mainwayp!> (w)
  (proc (wt wc)
    (while!> w
      (setq wc (car w))
      (cond
	((and (pairp wc) (eq (car wc) t))
	  (cond ((eval(cadr wc)) (setq wt t))
		(t (return nil)))))
      (setq w (cdr w)))
    (return wt)))

% Seek first appropriate way ...
(de firstgoodway!> (wl)
  (cond ((null wl) nil)
        ((not(eval(cadar wl))) (car wl))
        (t (firstgoodway!> (cdr wl)))))

% Get This Way from List ...
(de getthisway!> (wy wl)
  (prog (w)
    (cond ((or (eqs!> wy '(by standard way))       % word!!!
	       (eqs!> wy '(using standard way)))   % word!!!
	     (setq wy nil)))
    (setq w (getthisway1!> wy wl))  % searching by name of the way ...
    (cond (w (return w))
	  ((memqs!> (car wy) '(from using)) (setq wy (cdr wy))) % word!!!
	  (t (return nil)))
    (cond ((null wy) (return nil)))
    (setq wy (dgood!> wy))
    (cond ((null wy) (return nil))
	  ((and (eq wy !!er!!) (eqn ![er!] 6030))
	    (prog2 (setq ![er!] nil) (return nil)))
	  ((eq wy !!er!!) (return !!er!!))
	  ((cdr wy) (return nil)))
    (return (getthisway2!> (car wy) wl))  % searching by data name ...
    ))

(de getthisway1!> (wy wl)
  (cond ((null wl) wl)
	((eqs!> wy (caar wl)) (car wl))
	(t (getthisway1!> wy (cdr wl)))))

(de getthisway2!> (wy wl)
  (cond ((null wl) wl)
	((memq!> wy (cdddar wl)) (car wl))
	(t (getthisway2!> wy (cdr wl)))))

(de memq!> (w lst)
  (cond ((null lst) nil)
	((or (eq w (car lst))
	     (and (pairp(car lst)) (memq w (car lst))))  t)
	(t (memq!> w (cdr lst)))))

% Tries to calculate all data in the list LST if AUTO is On.
% ERR interrupt is can not do it.
(de require!> (lst)
  (cond((null lst) nil)
    (t (prog (wa)
    (cond(!*auto
           (foreach!> x in lst do (progn
             (setq ![chain!] nil)
             (setq wa (request!> x))
             (cond((eq wa !!er!!)
                     (prog2 (trsf!> x) (err!> ![er!])))
                  ((null wa) (cantfd!> x)))))))
    (foreach!> x in lst do
      (cond((null(eval x)) (cantfd!> x)))))) ))

% Tries to calculate X if AUTO is On.
% ERR interrupt is can not do it.
(de require1!> (x)
  (prog (wa)
    (cond(!*auto (progn
             (setq ![chain!] nil)
             (setq wa (request!> x))
             (cond((eq wa !!er!!)
                     (prog2 (trsf!> x) (err!> ![er!])))
                  ((or(null wa)(null(eval x))) (cantfd!> x)) ))))))

(de cantfd!> (w) (prog2 (trsf!> w) (err!> 6046)))

%------- Commands translation -------------------------------------

% General Command translation with Compound Mechanism ...
% Command -> List Of Commands -> List of Evaluations
(de instrs!> (lst)
  (prog nil
    (setq lst (composin!> lst)) % compound command maybe ...
    (cond (!*showcommands (showcommands!> lst))) % print the result
    (cond ((eq lst !!er!!) (prog2 (erm!> 6044) (return lst))))
    (setq lst (mapcar lst (function instr!>)))
    (cond ((memq !!er!! lst) (return !!er!!)))
    (return lst)))

% One Command translation ...
% Command text -> Evaluation
(de instr!> (lst)
  (proc (w wa)
    (cond ((null lst) (return '(nil next!>))))
    (setq wa lst)
    (setq w ![instr!])
    (while!> lst
      (setq w (assocf!> (car lst) w))
      (cond
        ((null w) (setq lst nil))
        ((eq(car w)(quote !!))
          (cond((cdr lst)(setq lst nil))
               (t(return(cons t(cdr w))))))
        ((eq(car w)(quote !!!!))
          (return(cons nil(cons(cadr w)(cons(cdr lst)(cddr w))))))
        (t(setq lst(cdr lst))))
      (exitif(null lst)))
    (cond((and(null(cdr wa))(stringp(car wa))) % in ...
           (return(list nil 'from!> wa)))
         ((memqs!> 'for wa)   % word!!!        % print ...
           (return(list nil 'printi!> wa)))
         ((memq '!= wa)                        % assign ...
           (return(list nil 'seti!> wa)))
         (t(return(list nil 'printi!> wa)))    % print ... ?
         )
    (closewrite!>)
    (gprinreset!>)
    (gprin!>  "Unknown command - '")
    (gprinwb!>  wa)(gprin!> "'.")(gterpri!>)
    (return !!er!! )))

% Print list of commands ...
(de showcommands!> (lst)
  (cond ((null lst) (gprinreset!>))
        ((eq lst !!er!!) !!er!!)
        (t(progn
	  (gprinreset!>)(setq ![gptab!] 4)
          (gprin!> "  ")
          (gprinwb!> (car lst))
          (gprin!> ";")
          (gterpri!>)
          (showcommands!> (cdr lst))))))

% Conpound command -> commands list ...
(de composin!> (lst)
  (cond
    % Comments in command ...
    ((and (not(eq (car lst) '!%)) (memq '!% lst)) (proc (w)
      (while!> (not (eq (car lst) '!%))
        (setq w (cons (car lst) w))
        (setq lst (cdr lst)))
      (return (cons lst (composin!> (reverse w))))))
    % Re prefix ...
    ((or (eq (car lst) '!R!E) (eq (car lst) '!R!e) (eq (car lst) '!r!e))
      (cond ((and (cdr lst) (eq (cadr lst) '!-))
             (composin!> (append '(erase and) (cddr lst))))
            (t (ncons lst))))
    % Not Composite Command ...
    ((or (not (memqs!> (car lst) ![icompos!])) % compound version forbidden
         (not (or (memq '!& lst)(memq '!, lst)(memqs!> 'and lst)))) % word!!!
       (ncons lst))
    % Composite Command Itself ...
    (t(proc (w wa wb wc wd)
        (setq lst (memll!> lst '(!& !, and))) % word!!!
        (cond ((eq lst !!er!!) (prog2(setq ![er!] 6044)(return !!er!!))))
	% Select Left Commands ...
        (while!> lst
          (setq w (inspar!> (car lst)))
          (cond ((eq w !!er!!) (return !!er!!)))
          (setq wa (cons (car w) wa))
          (setq lst (cdr lst))
          (exitif (cdr w)))
        (cond ((cdr w) (setq lst (cons (cdr w) lst))))
	 % Select Paremeters ...
        (while!> (and lst (not(insp!>(car lst))))
          (setq w (parway!> (car lst)))
          (cond ((null(car w)) (return !!er!!)))
          (setq wb (cons (car w) wb))
          (setq lst (cdr lst))
          (exitif (cdr w)))
        (setq wc (cdr w))
	% Right Commands ...
        (while!> lst
          (cond ((not(insp!>(car lst))) (return !!er!!)))
          (setq wd (cons (car lst) wd))
          (setq lst (cdr lst)))
        (cond (wd  % it after right comm
          (setq wd (cons (wiplit!>(car wd)) (cdr wd)))))
        (setq wa (reverse wa))
        (setq wb (reverse wb))
        (setq wd (reverse wd))
	% WA - Left Commands
	% WB - Parameters
	% WC - Way
	% WD - Right commands
        (setq w nil)
        (cond ((null wb) (return wa)))
        (while!> wa
	  (cond ((and (wucp!>(car wa)) wc)
	    (setq w (cons (append (car wa) wc) w))))
          (setq w (append w
            (foreach!> x in wb collect
              (append (car wa) (append x
                                      (cond ((wucp!>(car wa)) nil)
                                            (t wc)))))))
	  (cond
	    ((and (ucp!>(car wa)) wc)
               (setq w (append w '((ends)))))  % word!!!
	    ((and (wcp!>(car wa)) wc)
               (setq w (append w '((endw)))))) % word!!!
          (setq wa (cdr wa)))
        (while!> wd
          (setq w (append w
            (foreach!> x in wb collect (append (car wd) x))))
          (setq wd (cdr wd)))
        (return w)
        ))))

% Command predicate ...
(de insp!> (lst)
  (memqs!> (car lst) ![icompos!]))

% Write, Unload commands predicate ...
(de wucp!> (w)
  (memqs!> (car w) '(write save unload))) % word!!!
(de wcp!> (w)
  (memqs!> (car w) '(write)))             % word!!!
(de ucp!> (w)
  (memqs!> (car w) '(save unload)))       % word!!!

% Way predicate ...
(de bftp!> (w)
  (memqs!> w '( by from using to with in !> ))) % word!!!

% LST -> ( Command . Parameters ) ...
(de inspar!> (lst)
  (proc (w wa)
    (cond ((not(memqs!> (car lst) ![icompos!])) (return !!er!!)))
    (setq w ![instr!])
    (while!> lst
      (setq w (assocf!> (car lst) w))
      (cond ((null w) (return !!er!!)))
      (exitif (or (eq (car w) '!!) (eq (car w) '!!!!)))
      (setq wa (cons (car lst) wa))
      (setq lst (cdr lst)))
    (cond ((null lst) (return !!er!!)))
    (return(cons(reverse(cons(car lst) wa))(cdr lst)))))

% LST -> ( Parameters . Way ) ...
(de parway!> (lst)
  (proc (wa)
    (while!> (and lst (not(bftp!>(car lst)))) % by from ...
      (setq wa(cons(car lst) wa))
      (setq lst(cdr lst)))
    (return(cons(reverse wa)lst))))

% Split LST by WI=( and , & ) delimiters ...
(de memll!> (lst wi)
   (proc(wa wb)
      (setq lst(cons(car wi) lst))
      (while!> lst
        (setq lst(cdr lst))
        (while!>(and lst(not(memqs!>(car lst)wi)))
                (setq wa(cons(car lst)wa))
                (setq lst(cdr lst)))
            (cond
               ((null wa)(return !!er!!))
               (t(prog2 (setq wb(cons(reversip wa)wb))
                        (setq wa nil)))))
      (return(reversip wb))))

% Cut It etc in the end of LST ...
(de wiplit!> (lst)
  (cond ((null(cdr lst))
          (cond ((memqs!> (car lst) '(it them)) nil)     % word!!!
                (t                              lst)))
        (t (cons (car lst) (wiplit!> (cdr lst))))))


%-------- Commands execution --------------------------------------------

% Execute command from the terminal ...
(de rund!> nil
  (proc nil
    (setq ![firsti!] t)
    (loop!>
      (cond ((eq (runcom!> nil) !!stop!!) (return !!stop!!)))
      (setq ![firsti!] nil))
    ))

% This is main command executer. Work with the ERRORSET
% and catches possible internal REDUCE errors. The RUNCOM>
% is called only in tree places:
% (1) in main function RUND> as the basic commands' loop (RUNCOM> NIL)
% (2) in the In command for each command COM from file   (RUNCOM> COM)
% (3) in the Pause commad as another command loop        (RUNCOM> NIL)
% If WA=NIL then the command is requested from the terminal
% otherwise the WA is executed.
(de runcom!> (wa)
  (proc (wp wq wr w wc wx)
    (cond (wa % command from file must be printed
      (progn (setq wx t)
             (setq w wa) % print commands
	     (gprinreset!>) (setq ![gptab!] 3)
	     (gprin!> '!<!-! )
             (gprinwb!> w)
             (gprin!> ";")
             (gterpri!>)
             (gprinreset!>)  )))
    (loop!>
      (cond (wa (setq wa (instrs!> wa)))) % command translation
      labela
      (cond
        ((or (null wa) (eq wa !!er!!)) % take command from terminal if
          (progn
             (cond ((eq wa !!er!!) (closewrite!>)))
             (setq wp t)
             (setq wq t)
             (cond (wx (prin2 "Please enter correct command:")
		       (terpri)))
             (cond ( (eq (loop!> % getting a correct command
                            (cond ((iscsl!>) (printprompt promptstring!*)))
                            (setq wa (listok!> '( !; )))
                            (cond ((and (not (eq wa !!er!!))
                                        (not (eq (bc!> wa) !!er!!)))
                                     (progn
                                       (setq wp nil) % success
                                       (setq wa (car(mklevel!> wa)))))
                                  (t (progn
                                       (erm!> ![er!])
                                       (setq ![er!] nil))))
                            (tohead wp) % failure => loop again
                            (return nil)
                           ) !!er!!)
                       (return !!er!!)))))
         (t (setq wq nil)))
      (tohead wq) % we have not a command so loop again
      % here we have translated list of commands and we
      % are going to execute them
      (while!> wa % commands list evaluation (execution)
	(setq ![lsrs!] nil) % these are the values that not bad to
        (setq ![ivs!] nil)  % clear before each commands execution
        (cond % coordinates must be specified!
          ((and (null ![cord!]) (not (flagp (cadar wa) '!+unloc)))
	      (erm!> 7301) (setq wa !!er!!) (go labela)))
%       (tohead % coordinates must be specified!
%          (and (null ![cord!])
%               (not (flagp (cadar wa) '!+unloc))
%               (progn (erm!> 7301) (setq wa (cdr wa)) t) ))
	% execution ...
	(setq wc (cadar wa))
        (setq wr
          (list 'apply
            (list2 'function (cadar wa))
	    (list2 'quote
              (cond
                ((caar wa) (mapcar (cddar wa) 'eval))
                (t (cons (caddar wa)
                         (mapcar (cdddar wa) 'eval)))))))
        (setq wr (errorset!> wr ![erst1!] ![erst2!]))
	(cond ((atom wr) (algterpri!>)
                         (setq ![er!] wr)
                         (setq wr !!er!!))
	      (t (setq wr (car wr))))
        (cond ((eq wr !!stop!!) (return !!stop!!))
              ((eq wr !!er!!)
                 (progn (erm!> ![er!])
                        (setq wa nil)
                        (setq ![er!] nil))))
        (exitif (null wa)) % error, so exit it
        (setq wa (cdr wa)))
    (tohead (eq wr !!er!!)) % making the cycle in the case of the error.
      %(cond ((not(eq wc 'comment!>)) (setq ![firsti!] nil)))
      (cond ((and (not(eq wc 'comment!>)) (not(eq wc 'grgout!>)))
              (setq ![firsti!] nil)))
      (return wr))))

% Brackets count ...
(de bc!> (lst)
  (proc (wc) (setq wc 0)
    (while!> lst
      (cond((eq(car lst) '!()(setq wc(add1 wc)))
           ((eq(car lst) '!))(setq wc(sub1 wc))))
      (cond((lessp wc 0)(prog2(setq ![er!] 6100)(return !!er!!))))
      (setq lst(cdr lst)))
    (cond((not(eqn wc 0))
      (prog2(setq ![er!] 6100)(return !!er!!)))) ))


%==========  End of GRGmain.sl  ==========================================%

