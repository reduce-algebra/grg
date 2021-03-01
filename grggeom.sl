%==========================================================================%
%   GRGgeom.sl                                                   Geometry  %
%==========================================================================%
%   GRG 3.2 Standard Lisp Source Code       (C) 1988-96 Vadim V. Zhytnikov %
%==========================================================================%
% This file is distributed without any warranty. You may modify it but you %
% are not allowed to remove author's name and/or distribute modified file. %
%==========================================================================%


%------ Coordinate --------------------------------------------------------

% Macro 2 for Coordinates ...
(de x!> (wm)  (getel1!> ![cord!] wm))

%------ Dimension ---------------------------------------------------------

% Macro 3 for dim ...
(de dim!> nil  ![dim!])

%------ Delta symbols -----------------------------------------------------

(de delta!> (wa wb) (cond ((equal wa wb) 1) (t nil)))

%------ Epsilon tensors  05.96 --------------------------------------------

(de epsilon!> (u)
  (cond
    ((issame!> u) nil)
    (t(proc (wt wp w ww wc)
	(setq w u)
        (loop!>
	  (setq wp nil)
	  (setq ww (ncons (car w)))
	  (setq w (cdr w))
	  (while!> w
	    (setq wc (car w))
	    (cond ((lessp wc (car ww))
		     (setq ww (cons (car ww)
				    (cons wc
					  (cdr ww))))
		     (setq wt (not wt))
		     (setq wp t))
		  (t (setq ww (cons wc ww))))
	    (setq w (cdr w)))
	  (cond ((null wp) (return (cond (wt -1) (t 1)))))
	  (setq w (reversip ww)))))))

(de issame!> (w)
  (cond ((null w) nil)
	((memq (car w) (cdr w)) t)
	(t (issame!> (cdr w)))))

(dm epsilf!> (w) (list 'epsilf0!> (list 'quote (cdr w))))
(de epsilf0!> (w)
  (prog2
    (setq w (epsilon!> w))
    (cond (w (list 'times w (car !#!s!d!e!t!G)))
	  (t nil))))

(dm epsiuf!> (w) (list 'epsiuf0!> (list 'quote (cdr w))))
(de epsiuf0!> (w)
  (prog2
    (setq w (epsilon!> w))
    (cond (w (list 'quotient (list 'times w ![sigprod!]) (car !#!s!d!e!t!G)))
	  (t nil))))

(dm epsilh!> (w) (list 'epsilh0!> (list 'quote (cdr w))))
(de epsilh0!> (w)
  (prog2
    (setq w (epsilon!> w))
    (cond (w (list 'times w (list 'sqrt
                              (list 'times ![sigprod!] (car !#!d!e!t!g)))))
	  (t nil))))

(dm epsiuh!> (w) (list 'epsiuh0!> (list 'quote (cdr w))))
(de epsiuh0!> (w)
  (prog2
    (setq w (epsilon!> w))
    (cond (w (list 'quotient (list 'times w ![sigprod!])
                             (list 'sqrt
                               (list 'times ![sigprod!] (car !#!d!e!t!g)))))
	  (t nil))))

(de epss!> (wa wb)
  (cond ((equal wa wb)  nil)
	((eqn wa 0)       1)
	((eqn wa 1)      -1)
	(t              nil)))


%------ Basis and Inverse Basis  27.02.91, 05.96 --------------------------

% Basis ...
(de base!> nil
  (setq !#!b (copy !#!T)))

(de base1!> nil  % 05.96
  (prog (w) (setq !#!b (mkt!> 1))
    (setq w (aeval (list 'tp (list 'quotient 1 (mkmtetr!> !#!e)))))
    (mktetrm!> (cdr w) !#!b)
    (return t)))

% Inverse Basis ...
(de ibase!> nil
  (prog (w)
    (setq w (evalform!>(dfprod!> !#!b)))
    (cond ((null w) (prog2 (setq ![er!] 8400) (return !!er!!))))
    (setq !#!e (mkt!> 1))
    (setq w (aeval (list 'tp (list 'quotient 1 (mkmtetr!> !#!b)))))
    (mktetrm!> (cdr w) !#!e)
    (return t)))


%------ Sigma Matrix -------------------------------------------------------

(de sigma!> (wm wa wb)
  (prog (w)
    (setq w
      (cond
        ((and (eqn wm 0) (eqn wa 1) (eqn wb 1)) 1)
        ((and (eqn wm 1) (eqn wa 0) (eqn wb 0)) 1)
        ((and (eqn wm 2) (eqn wa 1) (eqn wb 0)) 1)
        ((and (eqn wm 3) (eqn wa 0) (eqn wb 1)) 1)
	(t nil)))
    (cond (w (setq w (car ![sgn!]))))
    (return w)))

(de sigmai!> (wm wa wb)
  (prog (w)
    (setq w
      (cond
        ((and (eqn wm 0) (eqn wa 1) (eqn wb 1)) 1)
        ((and (eqn wm 1) (eqn wa 0) (eqn wb 0)) 1)
        ((and (eqn wm 2) (eqn wa 1) (eqn wb 0)) 1)
        ((and (eqn wm 3) (eqn wa 0) (eqn wb 1)) 1)
	(t nil)))
    (return w)))

%------ Signature ----------------------------------------------------------

% Signum ...
(de signum!> (w)  (cond ((lessp w 0) -1) (t 1)))

% Signum of Product of Signature, i.e. Signum of the Metric ...
(de sigprod!> nil (signum!> (eval (cons 'times ![sgn!]))))

% Macros 2 Signature diagonal ...
(de diagonal!> (w)  (getel1!> ![sgn!] w))

(de pmsgn!> nil (pm!> 1))
(de mpsgn!> nil (mp!> 1))

%------ S - forms ----------------------------------------------------------

(de makesforms!> nil
  (prog nil
    (setq !#!S (mkt!> 2))
    (fordim!> x do (fordim!> y do (cond ((lessp x y)
      (putel!> (evalform!> (dfprod2!> (getframe!> x)
				      (getframe!> y)))
	       !#!S (list2 x y))))))
    (return t)))

%------ Metric -------------------------------------------------------------

(de imetr1!> nil  % 05.96
  (prog (w)
    (cond ((zerop (nz!>(eval!> (list 'det (setq w (mats!> !#!G))))))
      (setq ![er!] 6800) (return !!er!!) ))
    (setq !#!G!I (mkt!> 2))
    (rmats!> !#!G!I (aeval (list 'quotient 1 w)))
    (mitype!>)
    (return t)))

(de metr0!> nil  % 05.96
  (prog nil
    (msg!> 6801)
    (setq !#!G (mkt!> 2))
    (fordim!> i do
	  (putel!> (getel1!> ![sgn!] i) !#!G (list2 i i)))
    (mtype!>)
    (return t)))

(de metr1!> nil  % 05.96
  (prog (w)
    (cond ((zerop (nz!>(eval!> (list 'det (setq w (mats!> !#!G!I))))))
      (setq ![er!] 6800) (return !!er!!) ))
    (setq !#!G (mkt!> 2))
    (rmats!> !#!G (aeval (list 'quotient 1 w)))
    (mtype!>)
    (return t)))

(de nullmetric!> nil % 05.96
  (prog nil
    (cond
      (!#!G (msg!> 6820) (return t))
      ((equal ![sgn!] '(-1 1 1 1))
	 (setq !#!G  (copy ![nullm!]))
	 (setq ![mtype!] 1)
	 (setq ![dtype!] 1)
	 (return t))
      ((equal ![sgn!] '(1 -1 -1 -1))
	 (setq !#!G  (copy ![nullm1!]))
	 (setq ![mtype!] 1)
	 (setq ![dtype!] 1)
	 (return t))
      (t (setq ![er!] 7910) (return !!er!!)))))

(de detg1!> nil  % 05.96
  (prog (w)
    (cond ((zerop (nz!> (setq w (eval!> (list 'det (mats!> !#!G))))))
            (setq ![er!] 6800) (return !!er!!) ))
    (setq !#!d!e!t!G (ncons w))
    (return t)))

(de dethg1!> nil  % 05.96
  (prog (w)
    (cond ((zerop (nz!> (setq w (eval!> (list 'det (matsf!> 'gmetr!>))))))
            (setq ![er!] 6800) (return !!er!!) ))
    (setq !#!d!e!t!g (ncons w))
    (return t)))

(de sdetg1!> nil  % 05.96
  (prog (w)
    (cond ((zerop (nz!> (setq w (eval!> (list 'det (mats!> !#!G))))))
            (setq ![er!] 6800) (return !!er!!) ))
    (setq !#!s!d!e!t!G (ncons (evalalg!>
                    (list 'sqrt (list 'times ![sigprod!] w)))))
    (return t)))


%------ Volume -------------------------------------------------------------

(de vol0!> nil  % 05.96
  (prog (w)
    (fordim!> i do
      (cond ((eqn i 0) (setq w (getframe!> 0)))
            (t         (setq w (dfprod2!> w (getframe!> i))))))
    (setq w (evalform!> (fndfpr!> (car !#!s!d!e!t!G) w)))
    (cond ((null w) (setq ![er!] 4000) (return !!er!!)))
    (setq !#!V!O!L (ncons w))
    (return t)))


%------ Frame --------------------------------------------------------------

(de frame1!> nil  % 05.96
  (prog (w) (setq !#!T (mkt!> 1))
    (setq w (aeval (list 'tp (list 'quotient 1 (mkmtetr!> !#!D)))))
    (mktetrm!> (cdr w) !#!T)
    (ftype!>)
    (return t)))

(de iframe1!> nil  % 05.96
  (prog (w) (setq !#!D (mkt!> 1))
    (setq w (aeval (list 'tp (list 'quotient 1 (mkmtetr!> !#!T)))))
    (mktetrm!> (cdr w) !#!D)
    (fitype!>)
    (return t)))

(de frame0!> nil  % 05.96
  (prog nil
    (msg!> 6803)
    (setq !#!T (mkt1!>))
    (fordim!> i do (putel1!> (mkdx!> i) !#!T i))
    (ftype!>)
    (return t)))


%----- Macros Metric/Frame components -------------------------------------

% Components of Frame/Inverse Frame ... 05.96
(de ham!>  (wa wm)  % h^a_m
  (cond (![umod!] (vform1!> (getel1!> ![xv!] wm) (getel1!> !#!T wa)))
        (t        (getfdx!> (getel1!> !#!T wa) wm))))

(de hiam!> (wa wm)  % h_a^m
  (cond (![umod!] (vform1!> (getel1!> !#!D wa) (getel1!> ![xf!] wm)))
        (t        (getfdx!> (getel1!> !#!D wa) wm))))

(de gmetr!> (wi wk) % g_ik
  (cond((fholop!>)  % holonomic frame
         (getmetr!> wi wk))
       ((motop!>)   % `diagonal' metric
         (cons 'plus
           (foreach!> a in (dimlist!> 0) collect
             (mktimes!> (list (diagm!> a)
                              (ham!> a wi)
                              (ham!> (ai!> a) wk))))))
       (t(prog (w wc) % general case
           (fordim!> a do
             (fordim!> b do
               (cond ((setq wc (getmetr!> a b))
                 (setq w (cons (mktimes!> (list wc
                                                (ham!> a wi)
                                                (ham!> b wk)))
                               w))))))
           (cond (w (return (cons 'plus w))) (t (return nil)))))))

(de gmetr0!> (wi wk) % g_ik
  (cond((fholop!>)  % holonomic frame
         (getmetr!> wi wk))
       ((motop!>)   % `diagonal' metric
         (cons 'plus
           (foreach!> a in (dimlist!> 0) collect
             (mktimes!> (list (diagm!> a)
                              (ham0!> a wi)
                              (ham0!> (ai!> a) wk))))))
       (t(prog (w wc) % general case
           (fordim!> a do
             (fordim!> b do
               (cond ((setq wc (getmetr!> a b))
                 (setq w (cons (mktimes!> (list wc
                                                (ham0!> a wi)
                                                (ham0!> b wk)))
                               w))))))
           (cond (w (return (cons 'plus w))) (t (return nil)))))))

(de gimetr!> (wi wk) % g^ik
  (cond((ifholop!>)  % holonomic frame
         (getimetr!> wi wk))
       ((imotop!>)   % `diagonal' metric
         (cons 'plus
           (foreach!> a in (dimlist!> 0) collect
             (mktimes!> (list (diagmi!> a)
                              (hiam!> a wi)
                              (hiam!> (ai!> a)wk))))))
       (t(prog (w wc)
           (fordim!> a do
             (fordim!> b do
               (cond ((setq wc (getimetr!> a b))
                 (setq w (cons (mktimes!> (list wc
                                                (hiam!> a wi)
                                                (hiam!> b wk)))
                               w))))))
           (cond (w (return(cons 'plus w))) (t (return nil)))))))

(de huam!> (wa wm) % h^a^mu
  (cond ((imotop!>)
          (mktimes!> (list (diagmi!> wa) (hiam!> (ai!> wa) wm))))
        (t(cons 'plus
            (foreach!> b in (dimlist!> 0) collect
              (mktimes!> (list (getimetr!> wa b) (hiam!> b wm))))))))

(de hlam!> (wa wm) % h_a_mu
  (cond ((motop!>)
          (mktimes!> (list (diagm!> wa) (ham!> (ai!> wa) wm))))
        (t(cons 'plus
            (foreach!> b in (dimlist!> 0) collect
              (mktimes!> (list (getmetr!> wa b) (ham!> b wm))))))))

%---------- Spin Coefficients -------------------------------------------

(de spcoef!> (waa wb)
  (vform1!> (getiframe!> wb) (getel1!> !#!o!m!e!g!a!u waa)))

%----------  Line-element. 27.12.90, 05.96 ------------------------------

(de showlinel!> nil
  (proc (w wx wy wf wm)
    (setq wm "Cannot calculate Line-Element.")
    (setq ![chain!] nil)
    (setq w (request!> '!#!G))
    (cond((eq w !!er!!) (return w))
         ((null w) (progn (trsf!> '!#!G)(prin2 wm)(terpri)
                          (setq ![er!] 6046) (return !!er!!))))
    (setq ![chain!] nil)
    (setq w (request!> '!#!T))
    (cond((eq w !!er!!) (return w))
         ((null w) (progn (trsf!> '!#!T)(prin2 wm)(terpri)
                          (setq ![er!] 6046) (return !!er!!))))
    (gprinreset!>)
    (cond((not(and (fancyon!>) (not !*latex))) (terpri)))
    (cond((ifmodo!>) (gprin!> "ds2"))
	 (t(prog2
             (algpri!> " d" )
             (algpri!> '(expt !s 2) ))))
    (wriassign!> nil)
    (cond(!*math (gprin!> "(")))
    (fordim!> x  do (fordim!> y  do
      (cond((or(lessp x y)(eqn x y))(progn
        (setq w(eval!>(cond ((eqn x y) (gmetr0!> x x))
                           (t(list 'times 2 (gmetr0!> x y))))))
        (setq w (nz!> w))
        (cond((and(not(ifmodo!>))(numberp w)(lessp w 0)(not(eqn w -1)))
          (setq w (list 'minus (minus w)))))
        (cond((or (null w) (eqn w 0)) nil)
	     ((ifmodo!>)
	       (progn
		 (cond(wf (gprin!> "+")))
                 (setq wx (list2 '!dx (prepdx2!> x)))
                 (setq wy (list2 '!dx (prepdx2!> y)))
		 (ooprin!> (list 'times w wx wy))
		 (setq wf t)))
             (t(progn
                 (algpri!>(cond((eqn w -1) " - ")(wf " + ")(t " ")) )
                 (cond((not(memq w '(1 -1))) (progn
                    (cond((pairp w)(algpri!> "(" )))
                    (algpri!> (aeval w) )
                    (cond((pairp w)(algpri!> ")" ))) )))
                 (wridd!>)
                 (setq wx (prepdx2!> x))
                 (setq wy (prepdx2!> y))
                 (cond
                   ((eqn x y) (prog2
		     (cond((and ![umod!] (fancyon!>)) (progn
		       (algpri!> "(" )
		       (algpri!> wx )
		       (setq wx ")" ))))
                     (algpri!> (list 'expt wx 2) )))
                   (t(progn
                     (algpri!> wx )
                     (wridd!>)
                     (algpri!> wy ))))
                 (setq wf t)
                 )))  )))))
    (cond ((null wf) (alpri!> nil)))
    (cond (!*math (gprin!> ")")))
    (grgends!>)
    (grgterpri!>)
    (terpri)
))

(de prepdx2!> (wx)
  (cond
    (![umod!]
      (cond ((fancyon!>) (list 'expt '!#!#b wx))
            (t (compress (cons '!b (explode2 wx))))))
    (t (getel1!> ![cord!] wx))))

(de wridd!> nil
  (algpri!>
    (cond (![umod!] (cond ((fancyon!>) "\,")
			  (t           " ")))
	  (t        (cond ((fancyon!>) "\,d\,")
			  (t           " d "))))
    ))

%------ Spinorial S-forms 06.96 ------------------------------------------

(de ssform!> (wn w2 w3)
  (prog (w)
    (set wn (mkbox!> wn))
    (setq wn (eval wn))
    (setq w (evalform!> (chsignf!> (dfprod2!> (getframe!> 0)
					      (getframe!> w2)))))
    (putel1!> w wn 0)
    (setq w (evalform!> (fndfpr!> '(quotient 1 2) (dfsum!> (list2
	       (dfprod2!> (getframe!> 0) (getframe!> 1))
	       (chsignf!> (dfprod2!> (getframe!> w2) (getframe!> w3))))))))
    (putel1!> w wn 1)
    (setq w (evalform!> (dfprod2!> (getframe!> 1)
				   (getframe!> w3))))
    (putel1!> w wn 2)
    (return t)))

%------ Christoffel symbols  06.96 ---------------------------------------

(de chrt!> (wa)
  (list 'times '(quotient 1 2)
    (list 'quotient (list 'df (car !#!d!e!t!g) (getel1!> ![cord!] wa))
		    (car !#!d!e!t!g))))

(de chrf!> (wa wb wc)
  (list 'times '(quotient 1 2)
    (list 'plus
      (list 'df (gmetr!> wa wc) (getel1!> ![cord!] wb))
      (list 'df (gmetr!> wa wb) (getel1!> ![cord!] wc))
      (chsigna!> (list 'df (gmetr!> wb wc) (getel1!> ![cord!] wa))))))

(de chr!> (wa wb wc)
  (evalalg!> (getm!> '!#!C!H!R!F nil (list wa wb wc) '(3 nil nil))))


%------ Tensorial Solver 06.96 -------------------------------------------

% Genral solver for frame connection ...
% W - result, WT = t^a, WN = n_a_b (symmetric)
(de fsolver!> (wr wt wn)
  (prog (w ww wc)
    (setq ww (mkt!> 1))
    (setq w  (mkt!> 2))
    (set wr  (mkt!> 2))
    (setq wr (eval wr))
    % Creating t_a -> WT
    (cond (wt
      (fordim!> a do (putel1!> (getlo!> wt a) ww a))
      (setq wt ww)
      (setq ww nil)))
    % Solving for 2*omega_a_b -> W (antisymmetric iff n_a_b=0)
    (fordim!> a do (fordim!> b do
      (cond ((or (lessp a b) wn)
	(setq wc nil)
	(fordim!> c do (progn
	  % ( D_a _| D_b _| t_c ) T^c
	  (cond (wt
	    (setq wc (cons
	      (fndfpr!> (vform1!> (getiframe!> a)
			  (vform!> (getiframe!> b)
			    (getel1!> wt c)))
			(getframe!> c))
	      wc))))
	  % ( D_b _| n_a_c - D_a _| n_b_c ) T^c
	  (cond (wn
	    (setq wc (cons
	      (fndfpr!> (list 'difference
			  (vform1!> (getiframe!> b) (getel2s!> wn a c))
			  (vform1!> (getiframe!> a) (getel2s!> wn b c)))
			(getframe!> c))
	      wc))))))
	(cond (wt
	  % - D_a _| t_b
	  (setq wc (cons
	    (chsignf!> (vform!> (getiframe!> a) (getel1!> wt b)))
	    wc))
	  % D_b _| t_a
	  (setq wc (cons
	    (vform!> (getiframe!> b) (getel1!> wt a))
	    wc))))
	(cond (wn
	  % n_a_b
	  (setq wc (cons (getel2s!> wn a b) wc))))
	(setq wc (evalform!> (dfsum!> wc)))
	(putel!> wc w (list2 a b))))))
    % Now omega^a_b
    (fordim!> a do (fordim!> b do (progn
      (setq wc (evalform!>
        (cond
          ((imotop!>)
            (fndfpr!> (mktimes2!> '(quotient 1 2) (diagmi!> a))
	      (cond (wn (getel2!>  w (ai!> a) b))
		    (t  (getasy2!> w (ai!> a) b t)))))
          (t (dfsum!> (foreach!> c in (dimlist!> 0) collect
            (fndfpr!> (mktimes2!> '(quotient 1 2) (getimetr!> a c))
	      (cond (wn (getel2!>  w c b))
		    (t  (getasy2!> w c b t))))))))))
      (putel!> wc wr (list2 a b)) ))) ))


%------ Spinorial Solver  06.96 ------------------------------------------

% General spinorial solver ...
% WD = T - dotted, NIL - undotted
% WR - destination, WZ - Z_AA 3-form
(de ssolver!> (wr wz wd)
  (prog (wm00 wm10 wm20 wm01 wm11 wm21 w02 w12 w22 w03 w13 w23
         i0 i1 i2 i3 w)
    (set wr (mkbox!> wr))
    (setq wr (eval wr))
    (setq i0 0) (setq i1 1)
    (cond (wd (setq i2 3) (setq i3 2))  % undotted
          (t  (setq i2 2) (setq i3 3))) % dotted
    % #( Z_AA/\T^b )
    (setq wm00  (dfp2!> (not wd) (getel1!> wz 0) (getframe!> i0)))
    (setq wm10  (dfp2!> (not wd) (getel1!> wz 1) (getframe!> i0)))
    (setq wm20  (dfp2!> (not wd) (getel1!> wz 2) (getframe!> i0)))
    (setq wm01  (dfp2!> (not wd) (getel1!> wz 0) (getframe!> i1)))
    (setq wm11  (dfp2!> (not wd) (getel1!> wz 1) (getframe!> i1)))
    (setq wm21  (dfp2!> (not wd) (getel1!> wz 2) (getframe!> i1)))
    (setq w02  (dfp2!> wd (getel1!> wz 0) (getframe!> i2)))
    (setq w12  (dfp2!> wd (getel1!> wz 1) (getframe!> i2)))
    (setq w22  (dfp2!> wd (getel1!> wz 2) (getframe!> i2)))
    (setq w03  (dfp2!> wd (getel1!> wz 0) (getframe!> i3)))
    (setq w13  (dfp2!> wd (getel1!> wz 1) (getframe!> i3)))
    (setq w23  (dfp2!> wd (getel1!> wz 2) (getframe!> i3)))
    % omega_0
    (setq w (evalform!> (fndfpr!> 'i (dfsum!> (list
	      (fndfpr!>  w12 (getframe!> i0))
	      (fndfpr!> wm00 (getframe!> i1))
	      (fndfpr!> wm10 (getframe!> i2))
	      (fndfpr!>  w02 (getframe!> i3)))))))
    (putel1!> w wr 0)
    % omega_1
    (setq w (evalform!> (fndfpr!> '(quotient i 2) (dfsum!> (list
	      (fndfpr!> (list 'plus w22 wm11) (getframe!> i0))
	      (fndfpr!> (list 'plus w03 wm10) (getframe!> i1))
	      (fndfpr!> (list 'plus w13 wm20) (getframe!> i2))
	      (fndfpr!> (list 'plus w12 wm01) (getframe!> i3)))))))
    (putel1!> w wr 1)
    % omega_2
    (setq w (evalform!> (fndfpr!> 'i (dfsum!> (list
	      (fndfpr!> wm21 (getframe!> i0))
	      (fndfpr!>  w13 (getframe!> i1))
	      (fndfpr!>  w23 (getframe!> i2))
	      (fndfpr!> wm11 (getframe!> i3)))))))
    (putel1!> w wr 2)
    ))

(de dfp2!> (wd w1 w2)
  (eval!> (duald!>
    (cond
      ((and wd (not(pmmm!>)))  (dfprod2!> w1 w2))
      ((and (pmmm!>) (not wd)) (dfprod2!> w1 w2))
      (t                       (dfprod2!> w2 w1)) ))))

%-------------------------------------------------------------------------

% omega from dT with THETA and N ...
(de connec!> nil  % 09.96
  (prog (wt wn)
    % t = dT + TH
    (setq wt (mkt!> 1))
    (fordim!> a do
       (putel1!> (cond (!*torsion (dfsum!> (list
                                      (dex!>(getframe!> a))
				      (getel1!> !#!T!H!E!T!A a))))
		       (t         (dex!>(getframe!> a))))
                 wt a))
    % n = dG + N
    (setq wn (mkt!> 2))
    (fordim!> a do (fordim!> b do
      (cond ((leq a b)
    	(putel!> (cond (!*nonmetr (dfsum!> (list
                                      (dfun!>(getmetr!> a b))
				      (getel2!> !#!N a b))))
                       (t         (dfun!>(getmetr!> a b)) ))
                 wn (list2 a b))))))
    % solving ...
    (fsolver!> '!#!o!m!e!g!a wt wn)))

% Riem connection + wa
(de connecplus!> (wa)  % 09.96
  (prog (wt wn)
    % t = dT
    (setq wt (mkt!> 1))
    (fordim!> a do
       (putel1!> (dex!>(getframe!> a)) wt a))
    % n = dG
    (setq wn (mkt!> 2))
    (fordim!> a do (fordim!> b do
      (cond ((leq a b)
    	(putel!> (dfun!>(getmetr!> a b)) wn (list2 a b))))))
    % solving ...
    (cond (wa (fsolver!> '!#!o!m!e!g!a wt wn))
	  (t  (fsolver!> '!#!r!o!m!e!g!a wt wn)))
    % adding wa ...
    (cond (wa
     (fordim!> a do (fordim!> b do
       (putel!> (evalform!> (dfsum!> (list (getel2!> !#!o!m!e!g!a a b)
					   (getel2!> wa a b))))
		!#!o!m!e!g!a (list2 a b))))  ))
    ))

% K from THETA and N ...
(de conndef!> nil  % 09.96
  (prog (wt wn)
    % t = TH
    (setq wt (mkt!> 1))
    (fordim!> a do
       (putel1!> (getel1!> !#!T!H!E!T!A a) wt a))
    % n = N
    (setq wn (mkt!> 2))
    (fordim!> a do (fordim!> b do
      (cond ((leq a b)
    	(putel!> (getel2!> !#!N a b) wn (list2 a b))))))
    % solving ...
    (fsolver!> '!#!K wt wn)))

% KN from  N ...
(de nondef!> nil  % 09.96
  (prog (wt wn)
    (setq wt (mkt!> 1))
    % n = N
    (setq wn (mkt!> 2))
    (fordim!> a do (fordim!> b do
      (cond ((leq a b)
    	(putel!> (getel2!> !#!N a b) wn (list2 a b))))))
    % solving ...
    (fsolver!> '!#!K!N wt wn)))

% KQ from THETA ...
(de contor!> nil  % 09.96
  (prog (wt wn)
    % t = TH
    (setq wt (mkt!> 1))
    (fordim!> a do
       (putel1!> (getel1!> !#!T!H!E!T!A a) wt a))
    (setq wn (mkt!> 2))
    % solving ...
    (fsolver!> '!#!K!Q wt wn)))

% GAMMA from omega ...
(de gfromo!> nil
  (prog nil
     (setq !#!G!A!M!M!A (mkt!> 2))
     (fordim!> a do (fordim!> b do
       (putel!> (evalform!> (dfsum!> (list
		   (getm!> '!#!o!m!e!g!a nil (list2 a b) '(7 8))
		   (addgamma!> a b))))
		!#!G!A!M!M!A (list2 a b)))) ))

% RGAMMA from romega ...
(de rgfromro!> nil
  (prog nil
     (setq !#!R!G!A!M!M!A (mkt!> 2))
     (fordim!> a do (fordim!> b do
       (putel!> (evalform!> (dfsum!> (list
		   (getm!> '!#!r!o!m!e!g!a nil (list2 a b) '(7 8))
		   (addgamma!> a b))))
		!#!R!G!A!M!M!A (list2 a b)))) ))

(de addgamma!> (wm wn)
  (prog (w)
    (fordim!> ww do
       (setq w (cons (fndfpr!> (hiam!> ww wm) (dfun!>(ham!> ww wn))) w)))
    (return(dfsum!> w))))

% omega from GAMMA ...
(de ofromg!> nil
  (prog nil
     (setq !#!o!m!e!g!a (mkt!> 2))
     (fordim!> a do (fordim!> b do
       (putel!> (evalform!> (dfsum!> (list
		   (getm!> '!#!G!A!M!M!A nil (list2 a b) '(5 6))
		   (addomega!> a b))))
		!#!o!m!e!g!a (list2 a b)))) ))

(de addomega!> (wa wb)
  (prog (w)
    (fordim!> ww do
      (setq w (cons (fndfpr!> (ham!> wa ww) (dfun!>(hiam!> wb ww))) w)))
    (return(dfsum!> w))))

% N from K ...
(de nfromk!> (wk)
  (prog nil
    (setq !#!N (mkt!> 2))
    (fordim!> a do (fordim!> b do (cond ((leq a b)
       (putel!> (evalform!> (dfsum!> (list
		   (getm!> wk nil (list2 a b) '(2 nil))
		   (getm!> wk nil (list2 b a) '(2 nil))
		   )))
		!#!N (list2 a b)))) ))))

% THETA from K ...
(de qfromk!> (wk)
  (prog (w)
    (setq !#!T!H!E!T!A (mkt!> 1))
    (setq wk (eval wk))
    (fordim!> a do (progn
      (setq w nil)
      (fordim!> b do
	(setq w (cons (dfprod2!> (getframe!> b) (getel2!> wk a b)) w)))
      (putel1!> (evalform!> (dfsum!> w)) !#!T!H!E!T!A a)))))

% Torsion trace 1-form 08.01.91
(de qqq!> nil
  (prog (w)
    (fordim!> a  do
      (setq w (cons (vform!> (getiframe!> a)
                             (getel1!> !#!T!H!E!T!A a)) w)))
    (setq !#!Q!Q (ncons(evalform!>(chsign!> t (dfsum!> w)))))
    (return t)))

% Antisymmetric Torsion 3-form 10.96
(de qqqa!> nil
  (prog (w)
    (fordim!> a  do
      (setq w (cons (dfprod2!> (getlo!> !#!T a)
                               (getel1!> !#!T!H!E!T!A a)) w)))
    (setq !#!Q!Q!A (ncons (evalform!> (dfsum!> w))))
    (return t)))

% roumegau ...
(de ruconnec!> nil
  (ssolver!> '!#!r!o!m!e!g!a!u (mapcar !#!S!U 'dex!>) nil))
% romegad ...
(de rdconnec!> nil
  (ssolver!> '!#!r!o!m!e!g!a!d (mapcar !#!S!D 'dex!>) t))

% oumegau ...
(de uconnec!> nil
  (prog nil
    (ssolver!> '!#!o!m!e!g!a!u (mapcar !#!S!U 'dex!>) nil)
    (cond (!*torsion
      (for!> x (0 1 2) do
        (putel1!> (evalform!> (dfsum2!> (getel1!> !#!o!m!e!g!a!u x)
				        (getel1!> !#!K!U x)))
                  !#!o!m!e!g!a!u x))))))
% omegad ...
(de dconnec!> nil
  (prog nil
    (ssolver!> '!#!o!m!e!g!a!d (mapcar !#!S!D 'dex!>) t)
    (cond (!*torsion
      (for!> x (0 1 2) do
        (putel1!> (evalform!> (dfsum2!> (getel1!> !#!o!m!e!g!a!d x)
				        (getel1!> !#!K!D x)))
                  !#!o!m!e!g!a!d x))))))


% omegau from omega ...
(de oufromo!> (wu wo)
  (prog nil
    (set wu (mkbox!> wu))
    (setq wu (eval wu))
    (putel1!> (evalform!> (mpf!> (getel2!> wo 2 1))) wu 0)
    (putel1!> (evalform!> (fndfpr!> (pma!> '(quotient 1 2))
		(dfsum2!> (getel2!> wo 1 1) (getel2!> wo 3 3)))) wu 1)
    (putel1!> (evalform!> (pmf!> (getel2!> wo 3 0))) wu 2)
    ))

% omegad from omega ...
(de odfromo!> (wu wo)
  (prog nil
    (set wu (mkbox!> wu))
    (setq wu (eval wu))
    (putel1!> (evalform!> (mpf!> (getel2!> wo 3 1))) wu 0)
    (putel1!> (evalform!> (fndfpr!> (pma!> '(quotient 1 2))
		(dfsum2!> (getel2!> wo 1 1) (getel2!> wo 2 2)))) wu 1)
    (putel1!> (evalform!> (pmf!> (getel2!> wo 2 0))) wu 2)
    ))

% omega from omegau+omegad ...
(de ofromos!> (wo wu wd)
  (prog (w)
    (set wo (mkbox!> wo))
    (setq wo (eval wo))
    %
    (setq w (dfsum2!> (getel1!> wu 1) (getel1!> wd 1)))
    (putel!> (evalform!>(mpf!> w)) wo (list2 0 0))
    (putel!> (evalform!>(pmf!> w)) wo (list2 1 1))
    %
    (setq w (dfsum2!> (getel1!> wd 1) (chsign!> t (getel1!> wu 1))))
    (putel!> (evalform!>(pmf!> w)) wo (list2 2 2))
    (putel!> (evalform!>(mpf!> w)) wo (list2 3 3))
    %
    (setq w (evalform!>(pmf!>(getel1!> wd 2))))
    (putel!> w wo (list2 2 0))
    (putel!> w wo (list2 1 3))
    %
    (setq w (evalform!>(mpf!>(getel1!> wu 0))))
    (putel!> w wo (list2 2 1))
    (putel!> w wo (list2 0 3))
    %
    (setq w (evalform!>(pmf!>(getel1!> wu 2))))
    (putel!> w wo (list2 3 0))
    (putel!> w wo (list2 1 2))
    %
    (setq w (evalform!>(mpf!>(getel1!> wd 0))))
    (putel!> w wo (list2 3 1))
    (putel!> w wo (list2 0 2))
    ))

% complex conjugation ...
(de conj3!> (wr wss)
  (prog nil
    (set wr (mkbox!> wr))
    (setq wr (eval wr))
    (putel1!> (evalform!>(coform!>(getel1!> wss 0))) wr 0)
    (putel1!> (evalform!>(coform!>(getel1!> wss 1))) wr 1)
    (putel1!> (evalform!>(coform!>(getel1!> wss 2))) wr 2)
    ))

%--------------------------------------------------------------------------

% Curvature ...
(de curvature!> nil
  (prog (w)
    (setq !#!O!M!E!G!A (mkt!> 2))
    (fordim!> a do (fordim!> b do (progn
      (setq w (ncons (dex!> (getel2!> !#!o!m!e!g!a a b))))
      (fordim!> x do
	(setq w (cons (dfprod2!> (getel2!> !#!o!m!e!g!a a x)
				 (getel2!> !#!o!m!e!g!a x b) ) w)))
      (putel!> (evalform!> (dfsum!> w)) !#!O!M!E!G!A (list2 a b)))))))

% Spinor Curvature
(de scurvature!> (wr wo)
  (prog nil
    (set wr (mkbox!> wr))
    (setq wr (eval wr))
    (putel1!> (evalform!>(dfsum2!> (dex!>(getel1!> wo 0))
				   (fndfpr!> (pma!> 2) (dfprod2!>
				      (getel1!> wo 0)
				      (getel1!> wo 1) ))))  wr 0)
    (putel1!> (evalform!>(dfsum2!> (dex!>(getel1!> wo 1))
				   (fndfpr!> (pma!> 1) (dfprod2!>
				      (getel1!> wo 0)
				      (getel1!> wo 2) ))))  wr 1)
    (putel1!> (evalform!>(dfsum2!> (dex!>(getel1!> wo 2))
				   (fndfpr!> (pma!> 2) (dfprod2!>
				      (getel1!> wo 1)
				      (getel1!> wo 2) ))))  wr 2)
    ))

% Riemann Tensor ...
(de riemm!> nil
  (prog (w)
    (setq !#!R!I!M (mkt!> 4))
    (fordim!> wa do (fordim!> wb do
      (fordim!> wc do (fordim!> wd do (cond ((lessp wc wd)
	(setq w (vform1!> (getiframe!> wd)
		  (vform!> (getiframe!> wc)
                    (getel2!> !#!O!M!E!G!A wa wb))))
	(putel!> (evalalg!> w) !#!R!I!M (list wa wb wc wd))))))))))

% Ricci Tensor ...
(de ricci!> nil
  (prog (w)
    (setq !#!R!I!C (mkt!> 2))
    (fordim!> wa do (fordim!> wb do
      (cond
        ((and (null !*torsion) (null !*nonmetr) (greaterp wa wb)) nil)
	(t (progn
	     (setq w nil)
	     (fordim!> wx do
	       (setq w (cons (getrim!> wx wa wx wb) w)))
	     (putel!> (summa!> w) !#!R!I!C (list2 wa wb)))))))))

% Scalar Curvature ...
(de rscalar!> nil
  (prog (w)
    (fordim!> wa do (fordim!> wb do
      (setq w (cons (multa!> (getimetr!> wa wb)
			     (cond ((or !*torsion !*nonmetr)
					(getel2!> !#!R!I!C wa wb))
				   (t   (getel2s!> !#!R!I!C wa wb))) )
		    w))))
      (setq w (summa!> w))
      (setq !#!R!R (ncons w)) ))

% Einstein Tensor ...
(de gtensor!> nil
  (prog (w)
    (setq !#!G!T (mkt!> 2))
    (fordim!> wa do (fordim!> wb do
      (cond
        ((and (null !*torsion) (null !*nonmetr) (greaterp wa wb)) nil)
	(t (progn
	     (setq w (list2 (getel2!> !#!R!I!C wa wb)
			    (multa!> '(quotient -1 2)
			      (multa!> (getmetr!> wa wb)
				       (car !#!R!R)))))
	     (putel!> (summa!> w) !#!G!T (list2 wa wb)))))))))

%------- Curvature spinors -------------------------------------------------

% local aux functions ...
(de ousu!> (wa wb)
  (dualdi!> (dfprod2!> (getel1!> !#!O!M!E!G!A!U wa)
                       (getel1!> !#!S!U wb))))
(de ousd!> (wa wb)
  (dualdi!> (dfprod2!> (getel1!> !#!O!M!E!G!A!U wa)
                       (getel1!> !#!S!D wb))))
(de odsu!> (wa wb)
  (dualdi!> (dfprod2!> (getel1!> !#!O!M!E!G!A!D wa)
                       (getel1!> !#!S!U wb))))
(de odsd!> (wa wb)
  (dualdi!> (dfprod2!> (getel1!> !#!O!M!E!G!A!D wa)
                       (getel1!> !#!S!D wb))))

% Scalar curvature ...
(de rrsp!> nil
  (prog (wr)
    (cond
      (!*torsion
        (setq wr (summa!>  (list (ousu!> 2 0) (ousu!> 0 2)
			         (multa!> -2 (ousu!> 1 1)))))
        (setq wr (evalalg!>
          (cond (!*torsion (multa!> 2 (list 'plus wr (coalg!> wr))))
	        (t         (multa!> 4 wr))))) )
      (t
	(setq wr (evalalg!> (multa!> 8 (list 'difference
					 (ousu!> 0 2) (ousu!> 1 1))))) ))
    (setq !#!R!R (ncons wr))))

% Scalar deviation ...
(de rdsp!> nil
  (prog (wr)
    (setq wr (summa!>  (list (ousu!> 2 0) (ousu!> 0 2)
			     (multa!> -2 (ousu!> 1 1)))))
    (setq wr (evalalg!>
      (multa!> '(times -2 i) (list 'difference wr (coalg!> wr)))))
    (setq !#!R!D (ncons wr))))

% Weyl spinor ...
(de rwsp!> nil
  (progn
    (makebox!> '!#!R!W)
    (cond
      (!*torsion
        (putel1!> (evalalg!> (ousu!> 0 0))  !#!R!W 0)
        (putel1!> (evalalg!> (multa!> '(quotient 1 2)
                    (list 'plus  (ousu!> 0 1) (ousu!> 1 0))))  !#!R!W 1)
        (putel1!> (evalalg!> (list 'plus
                    (multa!> '(quotient 1 6)
                      (list 'plus  (ousu!> 2 0) (ousu!> 0 2)))
                    (multa!> '(quotient 2 3) (ousu!> 1 1))))   !#!R!W 2)
        (putel1!> (evalalg!> (multa!> '(quotient 1 2)
                    (list 'plus  (ousu!> 1 2) (ousu!> 2 1))))  !#!R!W 3)
        (putel1!> (evalalg!> (ousu!> 2 2))  !#!R!W 4)  )
      (t
        (putel1!> (evalalg!> (ousu!> 0 0))  !#!R!W 0)
        (putel1!> (evalalg!> (ousu!> 0 1))  !#!R!W 1)
        (putel1!> (evalalg!> (list 'plus
                    (multa!> '(quotient 1 3) (ousu!> 0 2))
                    (multa!> '(quotient 2 3) (ousu!> 1 1))))  !#!R!W 2)
        (putel1!> (evalalg!> (ousu!> 1 2))  !#!R!W 3)
        (putel1!> (evalalg!> (ousu!> 2 2))  !#!R!W 4)  )  )
    t))

% Ricanti spinor ...
(de rasp!> nil
  (progn
    (makebox!> '!#!R!A)
    (putel1!> (evalalg!> (multa!> (cond ((mppp!>) 1) (t -1))
                (list 'difference
                  (ousu!> 1 0) (ousu!> 0 1))))  !#!R!A 0)
    (putel1!> (evalalg!> (multa!> (cond ((mppp!>) '(quotient 1 2))
					(t        '(quotient -1 2)))
                (list 'difference
                  (ousu!> 2 0) (ousu!> 0 2))))  !#!R!A 1)
    (putel1!> (evalalg!> (multa!> (cond ((mppp!>) 1) (t -1))
                (list 'difference
                  (ousu!> 2 1) (ousu!> 1 2))))  !#!R!A 2)
    t))

% Traceless ricci spinor ...
(de rcsp!> nil
  (progn
    (makebox!> '!#!R!C)
    (for!> x (0 1 2) do (for!> y (0 1 2) do
      (cond ((leq x y)
        (putel!> (cond (!*torsion (evalalg!> (mpa!> (list 'difference
		                     (ousd!> x y) (odsu!> y x)))))
		       (t (evalalg!> (mpa!> (multa!> 2 (ousd!> x y))))))
		 !#!R!C (list2 x y))))))
    t))

% Traceless deviation spinor ...
(de rbsp!> nil
  (progn
    (makebox!> '!#!R!B)
    (for!> x (0 1 2) do (for!> y (0 1 2) do
      (cond ((leq x y)
        (putel!> (evalalg!> (mpa!> (multa!> 'i (list 'plus
		   (ousd!> x y) (odsu!> y x)))))
		 !#!R!B (list2 x y))))))
    t))

%----- NP formalism via macro 10.96 ---------------------------------------

(de psinp!> (w)
  (getel1!> !#!R!W w))

(de phinp!> (wa wb)
  (prog (w)
    (setq w (cond ((leq wa wb) (getel2!> !#!R!C wa wb))
		  (t  (coalg!> (getel2!> !#!R!C wb wa)))))
    (return (cond (w (list 'times (pma!> '(quotient 1 2)) w))
		  (t nil)))))

(de alphanp!>   nil (pma!>(spcoef!> 1 2)))
(de betanp!>    nil (pma!>(spcoef!> 1 3)))
(de gammanp!>   nil (pma!>(spcoef!> 1 0)))
(de epsilonnp!> nil (pma!>(spcoef!> 1 1)))
(de kappanp!>   nil (pma!>(spcoef!> 0 1)))
(de rhonp!>     nil (pma!>(spcoef!> 0 2)))
(de sigmanp!>   nil (pma!>(spcoef!> 0 3)))
(de taunp!>     nil (pma!>(spcoef!> 0 0)))
(de munp!>      nil (pma!>(spcoef!> 2 3)))
(de nunp!>      nil (pma!>(spcoef!> 2 0)))
(de lambdanp!>  nil (pma!>(spcoef!> 2 2)))
(de pinp!>      nil (pma!>(spcoef!> 2 1)))

(de dtop!>  nil (getiframe!> 0))
(de dddop!> nil (getiframe!> 1))
(de duop!>  nil (getiframe!> 3))
(de ddop!>  nil (getiframe!> 2))

%----- Geosedics. 10.96 ---------------------------------------------------

(de geodesics!> nil
  (prog (w)
    (setq !#!G!E!O!q (mkt!> 1))
    (fordim!> x do (progn
      (setq w (ncons (list 'df (getel1!> ![cord!] x) (car ![apar!]) 2)))
      (fordim!> y do (fordim!> z do
        (setq w (cons (list 'times (chr!> x y z)
			(list 'df (getel1!> ![cord!] y) (car ![apar!]))
			(list 'df (getel1!> ![cord!] z) (car ![apar!])))
		       w))))
      (putel1!> (equation!> (evalalg!> (cons 'plus w)) nil) !#!G!E!O!q x)))))

%----- Null Congruence. 10.96 ---------------------------------------------

(de ncnq!> nil
  (prog (w)
    (setq w (evalalg!> (vprod!> (car !#!K!V) (car !#!K!V))))
    (setq !#!N!C!o (ncons(equation!> w nil)))
    (cond (w (msg!> 6700)))))

% vec'w
(de getncv!> (w)
  (vform1!> (car !#!K!V) (getframe!> w)))
% vec.w
(de getncvlo!> (w)
  (vform1!> (car !#!K!V) (getlo!> !#!T w)))

% Riemann omega'a.b
(de rimomega!> (wa wb)
  (cond ((or !*torsion !*nonmetr) (getel2!> !#!r!o!m!e!g!a wa wb))
        (t                        (getel2!> !#!o!m!e!g!a wa wb))))

% Riemann omega'a.b.c
(de rimomegac!> (wa wb wc)
  (vform1!> (getiframe!> wc) (rimomega!> wa wb)))

(de ncgq!> nil
  (prog (w wc)
    (setq !#!G!C!o (mkt!> 1))
    (fordim!> x do (progn
      (setq w (ncons (vfun!> (car !#!K!V) (getncv!> x))))
      (fordim!> y do
	(setq w (cons (list 'times
			(vform1!> (car !#!K!V) (rimomega!> x y))
			(getncv!> y)) w)))
      (setq w (evalalg!> (cons 'plus w)))
      (cond (w (setq wc t)))
      (putel1!> (equation!> w nil) !#!G!C!o x)))
   (cond (wc (msg!> 6701)))))

% D.a ( vec.b ) = D.a | vec.b - omega'm.b.a vec.m
(de dcnc!> (wa wb)
  (prog (w)
    (setq w (ncons (vfun!> (getiframe!> wa) (getncvlo!> wb))))
    (fordim!> m do
      (setq w (cons (list 'times -1 (rimomegac!> m wb wa)
				    (getncvlo!> m))  w)))
    (setq w (evalalg!> (cons 'plus w)))
    (return w)))

% THETA
(de nctheta!> nil
  (prog (w)
    (fordim!> x do (fordim!> y do
      (setq w (cons (list 'times '(quotient 1 2)
				  (dcnc!> x y)
				  (getimetr!> x y)) w))))
   (setq w (evalalg!> (cons 'plus w)))
   (setq !#!t!h!e!t!a!O (ncons w)) ))

% omega^2
(de ncomega!> nil
  (prog (w wa wb)
    (fordim!> x do (fordim!> y do
      (fordim!> p do (fordim!> q do (progn
	(setq wa (getimetr!> x p))
	(setq wb (getimetr!> y q))
	(cond ((and wa wb)
          (setq w (cons (list 'times '(quotient 1 4) wa wb (dcnc!> p q)
			  (list 'difference (dcnc!> x y) (dcnc!> y x)))
                      w)))))))))
   (setq w (evalalg!> (cons 'plus w)))
   (setq !#!o!m!e!g!a!S!Q!O (ncons w)) ))

% sigma*~sigma
(de ncsigma!> nil
  (prog (w wa wb)
    (fordim!> x do (fordim!> y do
      (fordim!> p do (fordim!> q do (progn
	(setq wa (getimetr!> x p))
	(setq wb (getimetr!> y q))
	(cond ((and wa wb)
          (setq w (cons (list 'times '(quotient 1 4) wa wb (dcnc!> p q)
			  (list 'plus (dcnc!> x y) (dcnc!> y x)))
                      w)))))))))
   (setq w (cons 'plus w))
   (setq w (list 'difference w (list 'expt (car !#!t!h!e!t!a!O) 2)))
   (setq w (evalalg!> w))
   (setq !#!s!i!g!m!a!S!Q!O (ncons w)) ))

%----- Kinematics 10.96 ----------------------------------------------------

% UV = UUP'a D.a
(de uvfromuup!> nil
  (prog (w)
    (fordim!> x do
      (setq w (cons (fndfpr!> (getel1!> !#!U!U x) (getiframe!> x)) w)))
    (setq !#!U!V (ncons (evalform!> (dfsum!> w))))))

% UUp'a = UV _| T'a
(de uupfromuv!> nil
  (prog nil
    (setq !#!U!U (mkt!> 1))
    (fordim!> x do
      (putel1!> (evalalg!> (vform1!> (car !#!U!V) (getframe!> x)))
		!#!U!U x))
    ))

(de uudefault!> nil
  (prog nil
    (setq !#!U!U (mkt!> 1))
    (putel1!> 1	!#!U!U 0)
    (msg!> 6805)
    ))

% USQ = UUP'a UUP.a
(de usquare!> nil
  (prog (w)
    (fordim!> x do
      (setq w (cons (list 'times (getel1!> !#!U!U x)
                                 (getloa!> !#!U!U x)) w)))
    (setq w (evalalg!> (cons 'plus w)))
    (cond ((null w) (setq ![er!] 6702) (return !!er!!))
	  ((eqn (exprtype!> w) 2) (msg!> 9001)))
    (setq !#!U!S!Q (ncons w))))

% PRO'a.b
(de projector!> nil
  (prog (w)
    (setq !#!P!R (mkt!> 2))
    (cond ((null (car !#!U!S!Q)) (setq ![er!] 6702) (return !!er!!)))
    (setq w (list 'quotient 1 (car !#!U!S!Q)))
    (fordim!> a do (fordim!> b do
      (putel!> (evalalg!> (list 'difference (delta!> a b)
			    (list 'times w (getel1!> !#!U!U a)
					   (getloa!> !#!U!U b))))
	       !#!P!R (list2 a b))))))

(de dcuup!> (wa wb)
  (prog (w)
    (setq w (ncons (vfun!> (getiframe!> wa) (getel1!> !#!U!U wb))))
    (fordim!> wm do
      (setq w (cons (list 'times (getel1!> !#!U!U wm)
				 (rimomegac!> wb wm wa)) w)))
    (return (cons 'plus w))))

(de dcudown!> (wa wb)
  (prog (w)
    (setq w (ncons (vfun!> (getiframe!> wa) (getloa!> !#!U!U wb))))
    (fordim!> wm do
      (setq w (cons (list 'times -1 (getloa!> !#!U!U wm)
				    (rimomegac!> wm wb wa)) w)))
    (return (cons 'plus w))))

(de accelerat!> nil
  (prog (w)
     (setq !#!a!c!c!U (mkt!> 1))
     (fordim!> a do (progn
       (setq w nil)
       (fordim!> m do
	 (setq w (cons (list 'times (getel1!> !#!U!U m)
				    (dcuup!> m a)) w)))
       (putel1!> (evalalg!> (cons 'plus w)) !#!a!c!c!U a)))))

(de utheta!> nil
  (prog (w)
    (fordim!> m do (setq w (cons (dcuup!> m m) w)))
    (setq !#!t!h!e!t!a!U (ncons (evalalg!> (cons 'plus w))))))

(de uomega!> nil
  (prog (w)
    (setq !#!o!m!e!g!a!U (mkt!> 2))
    (fordim!> a do (fordim!> b do (cond ((lessp a b)
      (setq w nil)
      (fordim!> m do (fordim!> n do
	(setq w (cons (list 'times '(quotient 1 2)
			(getel2!> !#!P!R m a) (getel2!> !#!P!R n b)
			(list 'difference (dcudown!> m n)
					  (dcudown!> n m))) w))))
      (putel!> (evalalg!> (cons 'plus w)) !#!o!m!e!g!a!U (list2 a b))))))))

(de usigma!> nil
  (prog (w)
    (setq !#!s!i!g!m!a!U (mkt!> 2))
    (fordim!> a do (fordim!> b do (cond ((leq a b)
      (setq w (ncons (list 'times (list 'quotient -1 ![dim1!])
			    (car !#!t!h!e!t!a!U)
			    (getm!> '!#!P!R nil (list2 a b) '(2 nil)))))
      (fordim!> m do (fordim!> n do
	(setq w (cons (list 'times '(quotient 1 2)
			(getel2!> !#!P!R m a) (getel2!> !#!P!R n b)
			(list 'plus (dcudown!> m n)
				    (dcudown!> n m))) w))))
      (putel!> (evalalg!> (cons 'plus w)) !#!s!i!g!m!a!U (list2 a b))))))))


%------- Irreducible torsion components. 01.91 ---------------------------

% Local aux functions ...
(de qsu!> (wq wss)
   (dualdi!> (dfprod2!> (getel1!> !#!T!H!E!T!A wq) (getel1!> !#!S!U wss))))
(de qsd!> (wq wss)
   (dualdi!> (dfprod2!> (getel1!> !#!T!H!E!T!A wq) (getel1!> !#!S!D wss))))

% Tracelass torsion spinor ...
(de qcfromth!> nil
   (progn
     (makebox!> '!#!Q!C)
     (putel!> (evalalg!> (list 'times 1 (qsu!> 0 0)))
              !#!Q!C (list  0 0))
     (putel!> (evalalg!> (list 'times 1 '(quotient -1 3)
              (list 'plus (qsu!> 3 0) (list 'times -2 (qsu!> 0 1)))))
              !#!Q!C (list 1 0))
     (putel!> (evalalg!> (list 'times 1 (qsu!> 1 2)))
              !#!Q!C  (list 3 1))
     (putel!> (evalalg!> (list 'times 1 '(quotient 1 3)
              (list 'plus (qsu!> 0 2) (list 'times -2 (qsu!> 3 1)))))
              !#!Q!C (list 2 0))
     (putel!> (evalalg!> (list 'times -1 (qsu!> 3 2)))
              !#!Q!C  (list 3 0))
     (putel!> (evalalg!> (list 'times 1 '(quotient 1 3)
              (list 'plus (qsu!> 1 0) (list 'times -2 (qsu!> 2 1)))))
              !#!Q!C (list 1 1))
     (putel!> (evalalg!> (list 'times -1 (qsu!> 2 0)))
              !#!Q!C  (list 0 1))
     (putel!> (evalalg!> (list 'times 1 '(quotient -1 3)
              (list 'plus (qsu!> 2 2) (list 'times -2 (qsu!> 1 1)))))
              !#!Q!C (list 2 1))
     t))

% Torsion trace vector with spinors ...
(de qtfromthsp!> nil
   (progn
     (setq !#!Q!T (mkt!> 1))
     (putel1!> (evalalg!> (list 'times (car ![sgn!])
       (list 'plus (qsu!> 1 0) (qsu!> 2 1) (qsd!> 2 1) (qsd!> 0 2))))
       !#!Q!T 2)
     (putel1!> (evalalg!> (list 'times (car ![sgn!]) -1
       (list 'plus (qsu!> 3 1)(qsu!> 0 2)(qsd!> 1 0)(qsd!> 3 1))))
       !#!Q!T 3)
     (putel1!> (evalalg!> (list 'times (car ![sgn!]) -1
       (list 'plus
             (list 'times -1 (list 'plus (qsu!> 3 0) (qsu!> 0 1)))
             (qsd!> 2 0) (qsd!> 0 1))))
       !#!Q!T 0)
     (putel1!> (evalalg!> (list 'times (car ![sgn!]) -1
        (list 'plus
              (qsu!> 1 1) (qsu!> 2 2)
              (list 'times -1 (list 'plus (qsd!> 1 1) (qsd!> 3 2))))))
       !#!Q!T 1)
     t))

% Torsion pseudotrace vector with spinors ...
(de qpfromthsp!> nil
   (progn
     (setq !#!Q!P (mkt!> 1))
     (putel1!> (evalalg!> (list 'times (car ![sgn!]) 'i
       (list 'plus (qsu!> 3 0) (qsu!> 0 1) (qsd!> 2 0) (qsd!> 0 1))))
       !#!Q!P 0)
     (putel1!> (evalalg!>(list 'times (car ![sgn!]) '(minus i)
       (list 'plus (qsu!> 1 1) (qsu!> 2 2) (qsd!> 1 1) (qsd!> 3 2))))
       !#!Q!P 1)
     (putel1!> (evalalg!> (list 'times (car ![sgn!]) 'i
       (list 'plus (list 'times -1
                     (list 'plus (qsu!> 3 1) (qsu!> 0 2)))
                   (qsd!> 1 0) (qsd!> 3 1))))
       !#!Q!P 3)
     (putel1!> (evalalg!>(list 'times (car ![sgn!]) 'i
       (list 'plus (qsu!> 1 0) (qsu!> 2 1)
              (list 'times -1
                (list 'plus (qsd!> 2 1) (qsd!> 0 2))))))
       !#!Q!P 2)
     t))


%---- Undotted torsion 2-forms. 12.91 ------------------------------------

% wd - internal variable, fun - get function, wss - s-forms
(de trfr!> (wd fun wss)
  (prog (w wc)
    (set wd (mkt!> 1))
    (setq wd (eval wd))
    (for!> a (0 1 3) do (progn
      (setq w nil)
      (for!> b (0 1 2) do
        (setq w (cons (fndfpr!> (list 'times (cond ((eqn b 1) -2) (t 1))
                                             (apply fun (list a b)))
                                (getel1!> (eval wss) (si!> b))) w)) )
      (cond (w (putel1!> (evalform!> (dfsum!> w)) wd a)))))
    (return t)))

% local aux function ...
(de si!> (w)
  (cond ((eqn w 1) 1)
        ((eqn w 2) 0)
        ((eqn w 0) 2)))

% Get Traceless Torsion spinor ...
(de gcf!> (wa wb)
  (cond
    ((and (eqn wa 0) (eqn wb 0)) (getel2!> !#!Q!C 0 0))
    ((and (eqn wa 0) (eqn wb 1)) (getel2!> !#!Q!C 1 0))
    ((and (eqn wa 0) (eqn wb 2)) (getel2!> !#!Q!C 2 0))
    ((and (eqn wa 1) (eqn wb 0)) (getel2!> !#!Q!C 1 1))
    ((and (eqn wa 1) (eqn wb 1)) (getel2!> !#!Q!C 2 1))
    ((and (eqn wa 1) (eqn wb 2)) (getel2!> !#!Q!C 3 1))
    ((and (eqn wa 2) (eqn wb 0)) (list 'times -1 (getel2!> !#!Q!C 0 1)))
    ((and (eqn wa 2) (eqn wb 1)) (list 'times -1 (getel2!> !#!Q!C 1 1)))
    ((and (eqn wa 2) (eqn wb 2)) (list 'times -1 (getel2!> !#!Q!C 2 1)))
    ((and (eqn wa 3) (eqn wb 0)) (list 'times -1 (getel2!> !#!Q!C 1 0)))
    ((and (eqn wa 3) (eqn wb 1)) (list 'times -1 (getel2!> !#!Q!C 2 0)))
    ((and (eqn wa 3) (eqn wb 2)) (list 'times -1 (getel2!> !#!Q!C 3 0))) ))

% Get Torsion Trace spinor ...
(de gqf!> (wa wb)
  (gqpf!> wa wb (car ![sgn!]) !#!Q!T))

% Get Torsion Pseudotrace spinor ...
(de gpf!> (wa wb)
  (gqpf!> wa wb (cond ((mppp!>) 'i) (t '(minus i))) !#!Q!P))

(de gqpf!> (wa wb w lst)
  (cond
    ((and (eqn wa 0) (eqn wb 1))
      (list 'times (mkq!> w 6 nil) (getel1!> lst 0)))
    ((and (eqn wa 0) (eqn wb 2))
      (list 'times (mkq!> w 3 t) (getel1!> lst 3)))
    ((and (eqn wa 3) (eqn wb 0))
      (list 'times (mkq!> w 3 nil) (getel1!> lst 0)))
    ((and (eqn wa 3) (eqn wb 1))
      (list 'times (mkq!> w 6 t) (getel1!> lst 3)))
    ((and(eqn wa 2) (eqn wb 1))
      (list 'times (mkq!> w 6 nil) (getel1!> lst 2)))
    ((and (eqn wa 2) (eqn wb 2))
      (list 'times (mkq!> w 3 t) (getel1!> lst 1)))
    ((and (eqn wa 1) (eqn wb 0))
      (list 'times (mkq!> w 3 nil) (getel1!> lst 2)))
    ((and (eqn wa 1) (eqn wb 1))
      (list 'times (mkq!> w 6 t) (getel1!> lst 1))) ))

(de mkq!> (wd wn wb)
  (list 'quotient  (cond (wb (list 'minus wd)) (t wd))  wn))

(de qtfromqq!> nil
  (prog nil
    (makebox!> '!#!Q!T)
    (fordim!> a do
      (putel1!> (evalalg!> (vform1!> (getup!> !#!D a) (car !#!Q!Q)))
		!#!Q!T a))))

(de qpfromqqa!> nil
  (prog (w)
    (makebox!> '!#!Q!P)
    (setq w (dual!> (car !#!Q!Q!A)))
    (fordim!> a do
      (putel1!> (evalalg!> (vform1!> (getup!> !#!D a) w))
		!#!Q!P a))))

%------- Undotted Curvature 2-forms. 01.91 --------------------------------

% wd - internal variable, fun - get function, wss - s-forms
(de crfr!> (wd fun wss)
  (prog (w)
    (set wd (mkspace!> '((n . 2))))
    (for!> a (0 1 2) do (progn
      (setq w nil)
      (for!> b (0 1 2) do
        (setq w(cons(fndfpr!>(list 'times
                                     (cond((eqn b 1) '(minus 2))(t 1))
                                     (apply fun (list a b)))
                             (getel1!> (eval wss) (si!> b)))w)) )
      (cond(w(putel1!>(evalform!>(dfsum!> w)) (eval wd)  a)))))
    (return t)))

% Get Wayl spinor ...
(de gwf!> (wa wb)
   (getel1!> !#!R!W (plus wa wb)))

% Get Traceless Ricci spinor ...
(de gtf!> (wa wb)
   (list 'times (cond ((pmmm!>) '(quotient -1 2))
                      (t        '(quotient 1 2)))
                (getel2h!> !#!R!C wa wb)))

% Get Traceless Deviation spinor ...
(de gbf!> (wa wb)
   (list 'times (cond ((pmmm!>) '(quotient i 2))
                      (t        '(quotient (minus i) 2)))
                (getel2h!> !#!R!B wa wb)))

% Get Scalar Curvature spinor ...
(de gsf!> (wa wb)
   (cond((or(and(eqn wa 0)(eqn wb 2))(and(eqn wa 2)(eqn wb 0)))
           (list 'times '(quotient 1 12) (car !#!R!R)))
        ((and(eqn wa 1)(eqn wb 1))
           (list 'times '(quotient (minus 1) 24)(car !#!R!R)))
        (t nil)))

% Get Scalar Deviation spinor ...
(de gdf!> (wa wb)
   (cond((or(and(eqn wa 0)(eqn wb 2))(and(eqn wa 2)(eqn wb 0)))
           (list 'times '(quotient i 12)(car !#!R!D)))
        ((and(eqn wa 1)(eqn wb 1))
           (list 'times '(quotient (minus i) 24)(car !#!R!D)))
        (t nil)))

% Get Antisymmetric Ricci spinor ...
(de gaf!> (wa wb)
   (cond((and(eqn wa 0)(eqn wb 1))
           (list 'times (sgnm!>) '(quotient -1 2) (getel1!> !#!R!A 0)))
        ((and(eqn wa 0)(eqn wb 2))
           (list 'times (sgnm!>) -1 (getel1!> !#!R!A 1)))
        ((and(eqn wa 1)(eqn wb 0))
           (list 'times (sgnm!>) '(quotient 1 2) (getel1!> !#!R!A 0)))
        ((and(eqn wa 1)(eqn wb 2))
           (list 'times (sgnm!>) '(quotient -1 2)(getel1!> !#!R!A 2)))
        ((and(eqn wa 2)(eqn wb 0))
           (list 'times (sgnm!>) (getel1!> !#!R!A 1)))
        ((and(eqn wa 2)(eqn wb 1))
           (list 'times (sgnm!>) '(quotient 1 2) (getel1!> !#!R!A 2)))
        (t nil)))

% Signature ...
(de sgnm!> nil
  (cond ((pmmm!>) -1) (t 1)))

%=========== End of GRGgeom.sl ============================================%

