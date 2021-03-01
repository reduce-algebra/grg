%==========================================================================%
%   GRGclass.sl               Assignment, Macro Functions, Classification  %
%==========================================================================%
%   GRG 3.2 Standard Lisp Source Code       (C) 1988-96 Vadim V. Zhytnikov %
%==========================================================================%
% This file is distributed without any warranty. You may modify it but you %
% are not allowed to remove author's name and/or distribute modified file. %
%==========================================================================%


%-------  Assignment Command  09.91,03.94  -------------------------------

%
%  Assignment Command in forms
%   Tetrad T0=..., ...;
%   Tetrad T(j)=..., ...;
%   T(j)=..., ...;
%   T0=..., ...;
%
(de seti!> (lst)
  (prog (w wl wa wr was)
    (setq ![newabbr!] nil)
    (setq w (seek!> lst '(!=)))
    (cond ((or (null w) (null(car w)) (null(cdr w)))
       (prog2 (setq ![er!] 2204) (return !!er!!))))
    (setq wa (car w))
    (setq wl (length wa))
    (cond
      ((or (eqn wl 1)                      % t0 = ...
           (and(eqn wl 2)(pairp(car wa)))) % t(j) = ...
        (progn
          (setq wa(cond((eqn wl 1) (car wa))
                       (t          (cadr wa))))
          (cond((not(idp wa))
            (prog2(setq ![er!] 2204)(return !!er!!))))
          (setq was wa)
          (setq wa (explode2 wa))
          (cond((eqn wl 1)(setq wr(selid!> wa nil))))
          (setq wa(incomiv!> wa))
          (cond((not(flagp wa '!+ivar))
            (cond
              (wr(progn(doub!> was)(setq ![er!] 8604)(return !!er!!)))
              ((or(flagp wa '!+grgmac)(gettype!> wa))
                (progn(doub!> was)(setq ![er!] 3000)(return !!er!!)))
              (t(progn
                  (cond((flagp was '!+grg)(prog2(doub!> was)(msg!> 8603))))
                  (setq ![abbr!] (cons wa ![abbr!]))
		  (setq ![newabbr!] wa)
                  (global (ncons wa))
                  (flag (ncons wa) '!+ivar)
                  (flag (ncons wa) '!+abbr))))))
          (return(datr!> lst wa))))  %  --->  datr> ...
      ((atom(car wa))(prog2
                       (setq w(cons(car wa)(cdr w)))
                       (setq wa(cdr wa))))
      (t (prog2 (setq w(cons(cadr wa)(cons(car wa)(cdr w))))
                (setq wa(cddr wa)))))
    (setq wa(reverse wa))
    (setq was wa)
    (setq wa (assocf!> wa ![datl!]))
    (cond((or(null wa)(pairp(car wa)))
      (progn(setq ![er!] 6030)(doubl!> was)(return !!er!!))))
    (setq wa(car wa))
    (return(datr!> w wa))))          %  --->  datr> ...

% 03.94, 05.96 ... WN - Internal Variable, LST - Text
(de datr!> (lst wn)
  (proc (w wl wr ww)
    (cond ((null lst) (return nil))
	  ((setq w (constrp!> wn)) % constrained!
	    (progn (doubo!> wn) (setq ![er!] w) (return !!er!!))))
    (setq lst (memlistbr!> '!, lst))
    (cond ((eq lst !!er!!) (prog2 (setq ![er!] 2202) (return !!er!!))))
    (while!> lst
      (setq w (seek1!> (car lst) '!=))
      (cond((or (null w) (null(car w)) (null(cdr w)))
	(prog2 (setq ![er!] 2204) (return !!er!!))))
      (setq wl (reverse (car w)))
      (setq wr (cdr w))
      (cond((or (not(idp(car wl)))
		(and (cdr wl) (not(pairp(cadr wl))))
		(greaterp (length wl) 2))
	(prog2 (setq ![er!] 2204) (return !!er!!))))
      (setq ww
        (cond ((cdr wl) (transi!> wn wl wr))
	      (t (trans!> wn (car wl) wr))))
      (cond ((eq ww !!er!!) (cond (![newabbr!] (forget1!> ![newabbr!])))
                            (return !!er!!)))
      (cond
        ((eq wn '!#!G)  (mtype!>))
        ((eq wn '!#!G!I) (mitype!>))
        ((eq wn '!#!T)  (ftype!>))
        ((eq wn '!#!D)  (fitype!>)))
      (setq lst (cdr lst)) )))

% Normal Form ...

% 11.94 ... WN Internal var, WL Left, WR Right
(de trans!> (wn wl wr)
  (prog (wi wc)
    (cond((and (flagp wn '!+equ) (not(memq '!= wr)))
      (prog2 (setq ![er!] 2208) (return !!er!!))))
    (setq wi  (get wn '!=idxl))  % index types list
    (setq wc (transn!> wl wn wi)) % id = ... translation
    (cond ((eq wc !!er!!) (return !!er!!)))
    (return (trans0!> wn wc wr)) ))

% 11.94 ... WN Internal var, WL indices, WR Right
(de trans0!> (wn wc wr)
  (prog (wss wi wt we wnn)
    (setq wss (get wn '!=sidxl)) % symmetry list
    (setq wi  (get wn '!=idxl))  % index types list
    (setq wt  (gettype!> wn))  % expression type
    (cond((null(eval wn)) % prepare space for storing if not exists
      (prog2(setq wnn t)(set wn (mkbox!> wn)))))
    (cond (wc (setq wc (syaidx!> wc wss))))
    (cond((and wi (null wc)) (return nil)))
    (setq wr (cschtr!> wr (flagp wn '!+equ)))
    (setq ![extvar!] nil)
    (cond((flagp wn '!+equ) (setq we (translateeq!> wr))) % expr translation
	 (t                 (setq we (translate!> wr))))
    (cond ((equal we !!er!!)
             (cond (wnn (set wn nil)))
             (return !!er!!))
          ((null we)
	     (cond ((null wt) (put wn '!=type 0))))
          ((null wt)
             (setq wt (car we))
             (put wn '!=type wt))
          ((not(eqn wt (car we))) % incorrect expression type
             (cond(wnn(set wn nil)))
             (expects!> wt)
             (setq ![er!] 2100) (return !!er!!)))
    % storing of the data component
    (putel!> (cond(we(cdr we))(t nil)) (eval wn) (cond(wc wc)(t '(0))))
    (return t)))

% Perform Sign Changing [CS] and Complex Conjugations [CH] ...
(de cschtr!> (wr we)
  (cond((and ![ch!] ![cs!])
	 (cond (we (progn (setq wr (seek1!> wr '!=))
		     (list (csch0!>(reverse(car wr))) (csch0!>(cdr wr)))))
	       (t (csch0!> wr))))
       (![cs!]
	 (cond (we (progn (setq wr (seek1!> wr '!=))
		     (list (cs0!>(reverse(car wr))) (cs0!>(cdr wr)))))
	       (t (cs0!> wr))))
       (![ch!]
	 (cond (we (progn (setq wr (seek1!> wr '!=))
		     (list (ch0!>(reverse(car wr))) (ch0!>(cdr wr)))))
	       (t (ch0!> wr))))
       (t wr)))

% aux functions ...
(de cs0!> (w) (list2 '!- (ncons w)))
(de ch0!> (w) (list2 '!~ (ncons w)))
(de csch0!> (w) (list '!- '!~ (ncons w)))

% Message about wrong type of the expression ...
(de expects!> (wt)
  (progn
    (cond((eqn wt 0)  (prin2 "Algebraic expression"))
         ((eqn wt -1) (prin2 "Vector"))
         (t           (prin2 wt) (prin2 "-form")))
    (prin2 " is expected.")
    (terpri)))

% w - id = ...  wn - internal variable  wi - index types list
(de transn!> (w wn wi)
  (prog(wa wb wc wd wl wf)
    (setq wb(explode2 w))
    (setq wa(cdr(explode2 wn)))
    (setq wf(selid!> wb nil)) % wb - id  wf - indices
    (cond((not(equal wb wa))
      (progn(expid!> wa)(setq ![er!] 2101)(return !!er!!))))
    (cond((null wf)(cond((null wi)(return nil)) % scalar data ...
                        (t(prog2(setq ![er!] 2102)(return !!er!!))))))
    (setq wf (mapcar wf 'digorerr!>))
    (cond((memq !!er!! wf)
      (prog2(setq ![er!] 2102)(return !!er!!))))
    (cond ((eq (goodidxl!> wf wi) !!er!!) (return !!er!!)))
    (return wf)))

% aux fun ...
(de digorerr!> (w)
  (cond((digit w)(compress (ncons w)))
       (t !!er!!)))

% w is expected ...
(de expid!> (w)
  (progn (mapc w 'prin2)
         (prin2 " is expected.")
         (terpri)))

% Verifies correct range of indices ...
(de goodidxl!> (wb wi)
  (cond ((and (null wb) (null wi)) t)
        ((null wb) (setq ![er!] 21023) !!er!!)
        ((null wi) (setq ![er!] 21024) !!er!!)
        ((lessp (dimid!>(car wi) )(car wb)) (setq ![er!] 21022) !!er!!)
        (t (goodidxl!> (cdr wb) (cdr wi)))))

% Verifies correct range the index ...
(de goodid1!> (w wt)
  (cond((lessp(dimid!> wt)w) nil)
       (t t)))

% Tensorial Form  ...

% WN - Internal Variable  WL - Left  WR - Right
(de transi!> (wn wl wr)
  (proc (wt wi w wll wa wii)
    (setq wll(cons nil(get wn '!=idxl)))
    (setq wt (car wl))
    (setq wi (cadr wl))
    (setq wt (explode2 wt))
    (cond((not(equal wt(cdr(explode2 wn))))
           (progn(expid!>(cdr(explode2 wn)))
                 (setq ![er!] 2101)(return !!er!!))))
    (setq wi(memlist!> '!, wi))
    (cond((eq wi !!er!!) (prog2(setq ![er!] 2202)(return !!er!!))))
    (cond((not(eqn(length wi)(length(get wn '!=idxl))))
           (prog2 (cond (![newabbr!] (doubo!> ![newabbr!])
                                     (setq ![er!] 22071))
                        (t           (setq ![er!] 2207)))
                  (return !!er!!))))
    (setq wii nil)
    (while!> wi
      (setq wii
        (cons (prog2 (setq wll(cdr wll)) (sumintr!> (car wi) (car wll)))
              wii))
      (setq wi (cdr wi)))
    (setq wi (reverse wii))     % here now the list of indices in lhs
    (cond((memq !!er!! wi)(return !!er!!)))
    (setq ![extvar!] (mkextvars!> wi)) % prepare list of ext. vars.
    (cond((memq !!er!! ![extvar!]) (return !!er!!))
	 ((null ![extvar!])            % only numerical indices ...
            (return (trans0!> wn (mklitind!> wi) wr))))
    (cond((flagp wn '!+equ)(setq wr (pretranseq!> wr))) % pre translation
         (t                (setq wr (pretrans!> wr))))
    (cond((eq wr !!er!!)(return !!er!!)))
    (setq ![idl!] wi) (setq ![texpr!] wr)
    (setq w(cond((null(eval wn))(mkbox!> wn))
                (t(eval wn))))
    (setq w (errorset!> (list 'allcoll!> (list 'quote w)
                                      (list 'quote wn)
                                      nil
                                      (list 'quote (get wn '!=idxl))
                                      (list 'function 'transel!>)
                      ) ![erst1!] ![erst2!] ))
    (remsubindex!> ![idl!])(setq ![texpr!] nil)
    (cond((atom w)(prog2(setq ![er!] w)(return !!er!!)))
         (t(set wn(car w))))
    (return t)))

% Prepare List of Ext. vars ...
(de mkextvars!> (lst)
  (cond((null lst) nil)
       ((atom(car lst))(consmemer!>(car lst)(mkextvars!>(cdr lst))))
       (t(appmemer!>(car lst)(mkextvars!>(cdr lst))))))

(de appmemer!> (wa wb)
  (prog2 (while!> wa
           (setq wb (consmemer!> (car wa)wb))
           (setq wa (cdr wa)))
         wb))

(de consmemer!> (w lst)
  (cond((and(idp w)(memq w lst))
         (prog2(setq ![er!] 2205)(cons !!er!! lst)))
       ((idp w) (cons w lst))
       (t lst)))

(de mklitind!> (lst)
  (mapcar lst 'mklitind1!>))

(de mklitind1!> (w)
  (cond ((numberp w) w)
	(t (eval(cons 'plus w)))))

% Translate the element ...
(de transel!> (lst wi wn)
  (cond((and (syaidxp!> wi (get wn '!=sidxl))
             (coidxp!> wi ![idl!]) )
        (progn
          (putindex!> wi)
          (cond((flagp wn '!+equ)(setq lst(unievaluateeq!> ![texpr!])))
               (t                (setq lst(unievaluate!> ![texpr!]))))
          (remsubindex!> ![idl!])
          (cond((null(gettype!> wn))(put wn '!=type (car lst))))
          (cond((and lst(not(eqn(car lst)(gettype!> wn))))
            (prog2 (expects!>(gettype!> wn))
                   (err!> 2100))))
          (cond(lst(cdr lst))
               (t nil))))
       (t lst)))

% Summed index treatment if exists ...
(de sumintr!> (w wl)
  (cond((atom wl) % tetrad or holonomic index
         (cond((or(cdr w)(not(or(idp(car w))(numberp(car w)))))
                (prog2(setq ![er!] 2206) !!er!!))
              ((and(numberp(car w))(not(goodid1!>(car w)wl)))
                (prog2(setq ![er!] 21022) !!er!!))
              (t(car w))))
       ((null(cdr w))     % spinor or enumerating index
         (cond((not(or(idp(car w))(numberp(car w))))
                (prog2(setq ![er!] 2206) !!er!!))
              ((and(numberp(car w))(not(goodid1!>(car w)wl)))
                (prog2(setq ![er!] 21022) !!er!!))
              (t(car w))))
       (t(prog nil        % summed spinor index
           (setq w(memlist!> '!+ w))
           (cond((or(eq w !!er!!)(not(eqn(length w)(dimid!> wl))))
                  (prog2(setq ![er!] 2206) (return !!er!!))))
           (setq w (mapcar w 'auxfun1!>))
           (cond((memq !!er!! w)
                  (prog2(setq ![er!] 2206)(return !!er!!)))
                (t(return w)))))))

(de auxfun1!> (w)
  (cond((or (cdr w) (and (not(idp(car w))) (not(numberp(car w)))))
          !!er!!)
       ((and (numberp(car w)) (greaterp(car w)1)) !!er!!)
       (t(car w))))

% Compares current list of indices WI with concrete values in WL ...
(de coidxp!> (wi wl)
  (cond((and(null wi)(null wl)) t)
       (t(and (coidxp1!> (car wi)(car wl))
	      (coidxp!>  (cdr wi)(cdr wl))))))

(de coidxp1!> (wi wl)
  (cond((numberp wl)
         (cond((eqn wi wl)t)
              (t nil)))
       ((pairp wl)
	 (prog2 (setq wl (putindex2!> wl))
		(cond((or (lessp wi (car wl))
			  (lessp(length(cdr wl))(difference wi (car wl))))
			nil)
		     (t t))))
       (t t)))

% Preparing Ext. vars for translator ...
(de putindex!> (wi)
  (proc(w)
    (setq w ![idl!])
    (while!> wi
      (cond((numberp(car w)) nil)
           ((atom(car w))(put (car w) '!=subind (car wi)))
           (t(putindex1!> (car w) (car wi))))
      (setq w(cdr w)) (setq wi(cdr wi)))))

(de putindex1!> (wa wb)
  (proc nil
    (setq wa (putindex2!> wa))
    (setq wb (difference wb (car wa)))
    (setq wa (cdr wa))
    (setq wb (add1 wb))
    (while!> wa
      (put (car wa) '!=subind
        (cond((lessp(length wa)wb) 1)
             (t 0)))
      (setq wa(cdr wa)))))

(de putindex2!> (w)
  (proc (wn wr)
    (setq wn 0)
    (while!> w
      (cond
        ((numberp(car w)) (setq wn (plus wn (car w))))
	(t(setq wr (cons(car w)wr))))
      (setq w (cdr w)))
    (return(cons wn (reversip wr)))))

% Removing Ext. vars. after translation ...
(de remsubindex!> (w)
 (cond((null w) nil)
      ((pairp(car w))
        (prog2 (remsubindex!>(car w)) (remsubindex!>(cdr w))))
      ((idp(car w))(prog2
        (remprop (car w) '!=subind)
        (remsubindex!>(cdr w))))
      (t(remsubindex!>(cdr w)))))


%----- Macro Functions. 08.01.91, 05.96 -------------------------------

% Solution ...
(de getsoln!> (lst)
  (cond((cdr lst) (prog2(doub!> '!S!o!l)(err!> 2105)))
       ((null(car lst)) (getsoln1!> 0))
       ((not(zerop(caar lst))) (prog2(doub!> '!S!o!l)(err!> 2023)))
       ((not(numberp(cdar lst))) (prog2(doub!> '!S!o!l)(err!> 2106)))
       (t(getsoln1!> (cdar lst)))))

(de getsoln1!> (wn)
  (cond((null ![sol!]) (err!> 2113))
       (t(proc (w wnn)
	   (setq wnn wn)
	   (setq w ![sol!])
	   (while!> (and w (not(zerop wn)))
	     (setq w (cdr w))
	     (setq wn (sub1 wn)))
	   (cond((or(null w)(not(zerop wn)))
	     (prog2 (doub!> wnn) (err!> 2114))))
	   (return(cona1!> 0 (get1equ!>(car w))))))))

%----- Classify command 06.96 ------------------------------------------

(de classify!> (lst)
  (proc (w wc wi)
    (cond ((null lst) (return nil)))
    (cond ((eq (setq w (dgood!> lst)) !!er!!) (return !!er!!)))
    (setq w (altdata!> w))
    (while!> w
      (setq wc (car w))
      (cond
        ((not(zerop(get wc '!=type)))
            (setq ![er!] 9100) (doubo!> wc) (return !!er!!))
	((null(eval wc))
	    (abse!> wc) (go lab)))
      (setq wi (get wc '!=idxl))
      (cond
	((null wi) (cmsg!> wc) (scaltype!> (eval wc)))
	((eqn (length wi) 1)
	  (cond
	    ((eqn (dimid!> (car wi)) 2) (cmsg!> wc) (emtype!> (eval wc)))
	    ((eqn (dimid!> (car wi)) 4) (cmsg!> wc) (petrov!> (eval wc)))
            (t (setq ![er!] 9101) (doubo!> wc) (return !!er!!))))
	((eqn (length wi) 2)
	  (cond
	    ((and (eqn (dimid!> (car wi)) 2) (eqn (dimid!> (cadr wi)) 2))
	       (cmsg!> wc) (riccisclass!> (eval wc)))
	    ((and (eqn (dimid!> (car wi)) 1) (eqn (dimid!> (cadr wi)) 1))
	       (cmsg!> wc) (vectype!> (eval wc)))
            (t (setq ![er!] 9101) (doubo!> wc) (return !!er!!))))
	(t (setq ![er!] 9101) (doubo!> wc) (return !!er!!)))
      lab
      (setq w (cdr w)))))

(de cmsg!> (w)
  (progn (gprinreset!>)
         (gprils!> '("Classifying"))
         (pn0!> w)
         (gprils0!> '(":"))
         (gterpri!>)))

%----- Petrov classification. 08.01.91, 06.96 --------------------------

(de petrov!> (lst)
  (prog (w0 w1 w2 w3 w4 wc wr)
    (cond (!*trace
      (prin2 "Petrov classification ...") (terpri)
      (prin2 "  Using algorithm by F.W.Letniowski & R.G.McLenaghan") (terpri)
      (prin2 "    Gen. Rel. Grav. 20 (1988) 463-483") (terpri)))
    (setq w0 (aeval (nz!> (getel1!> lst 0 ))))
    (setq w1 (aeval (nz!> (getel1!> lst 1 ))))
    (setq w2 (aeval (nz!> (getel1!> lst 2 ))))
    (setq w3 (aeval (nz!> (getel1!> lst 3 ))))
    (setq w4 (aeval (nz!> (getel1!> lst 4 ))))
    (setq wc (plus (times 16 (to1!> w0))
		   (times 8  (to1!> w1))
		   (times 4  (to1!> w2))
		   (times 2  (to1!> w3))
		   (times 1  (to1!> w4)) ))
    (cond (!*trace
      (prin2 "Case ") (prin2 wc) (prin2 ": ")
      (foreach!> x in (list w0 w1 w2 w3 w4) do (progn
	(prin2 " ") (cond ((zerop x) (prin2 0)) (t (prin2 "N")))))
      (prin2 "  =>")
      (terpri) ))
    (setq wr
      (cond
        ((eqn wc 0)  (finis!>  "0"   ))
        ((eqn wc 1)  (finis!>  "N"   ))
        ((eqn wc 2)  (finis!>  "III" ))
        ((eqn wc 3)  (finis!>  "III" ))
        ((eqn wc 4)  (finis!>  "D"   ))
        ((eqn wc 5)  (finis!>  "II"  ))
        ((eqn wc 6)  (finis!>  "II"  ))

	((eqn wc 7)  (alter!> (list 'plus (list 'times  2 w3 w3)
				          (list 'times -3 w2 w4))
			      "2*W3^2-3*W2*W4" "D" "II"))

        ((eqn wc 8)  (finis!>  "III" ))
        ((eqn wc 9)  (finis!>  "I"   ))
        ((eqn wc 10) (finis!>  "I"   ))

	((eqn wc 11) (alter!> (list 'plus (list 'times 27 w4 w4 w1)
				          (list 'times 64 w3 w3 w3))
			      "27*W4^2*W1+64*W3^3" "II" "I"))

        ((eqn wc 12) (finis!>  "II"  ))

	((eqn wc 13) (alter!> (list 'plus (list 'times   w1 w1 w4)
				          (list 'times 2 w2 w2 w2))
			      "W1^2*W4+2*W2^3" "II" "I"))

	((eqn wc 14) (alter!> (list 'plus (list 'times   9 w2 w2)
				          (list 'times -16 w1 w3))
			      "9*W2^2-16*W1*W3" "II" "I"))

	((eqn wc 15) (scase15!> w0 w1 w2 w3 w4))

        ((eqn wc 16) (finis!> "N"   ))
        ((eqn wc 17) (finis!> "I"   ))
        ((eqn wc 18) (finis!> "I"   ))

	((eqn wc 19) (alter!> (list 'plus (list 'times     w0 w4 w4 w4)
				          (list 'times -27 w3 w3 w3 w3))
			      "W0*W4^3-27*W3^4" "II" "I"))

        ((eqn wc 20) (finis!> "II"  ))

	((eqn wc 21) (alter!> (list 'plus (list 'times  9 w2 w2)
				          (list 'times -1 w0 w4))
			      "9*W2^2-W0*W4" "D" "I"))

	((eqn wc 22) (alter!> (list 'plus (list 'times   w3 w3 w0)
				          (list 'times 2 w2 w2 w2))
			      "W3^2*W0+2*W2^3" "II" "I"))

	((eqn wc 23) (scase23!> w0 w1 w2 w3 w4))

        ((eqn wc 24) (finis!> "III" ))

	((eqn wc 25) (alter!> (list 'plus (list 'times     w4 w0 w0 w0)
				          (list 'times -27 w1 w1 w1 w1))
			      "W4*W0^3-27*W1^4" "II" "I"))

	((eqn wc 26) (alter!> (list 'plus (list 'times 27 w0 w0 w3)
				   (list 'times 64 w1 w1 w1))
			      "27*W0^2*W3+64*W1^3" "II" "I"))

	((eqn wc 27) (scase27!> w0 w1 w2 w3 w4))

	((eqn wc 28) (alter!> (list 'plus (list 'times  2 w1 w1)
				          (list 'times -3 w2 w0))
			      "2*W1^2-3*W2*W0" "D" "II"))

	((eqn wc 29) (scase29!> w0 w1 w2 w3 w4))
	((eqn wc 30) (scase30!> w0 w1 w2 w3 w4))
	((eqn wc 31) (scase31!> w0 w1 w2 w3 w4))

        ))
    (return wr)))

(de to1!> (w)
  (cond ((zerop w) 0)
        (t 1)))

(de finis!> (w)
  (progn
    (prin2 "Petrov type is ")
    (prin2 w)
    (prin2 ".")
    (terpri)
    w))

(de alter!> (w wp w0 w1)
  (prog2
    (setq w (aeval w))
    (cond ((zerop w) (iszero!> wp 2)      (finis!> w0))
	  (t         (isnonzero!> wp 2 w) (finis!> w1)))))

(de iszero!> (wp wl)
  (cond (!*trace
    (spaces wl)
    (prin2 wp)
    (prin2 " = 0 =>")
    (terpri))))

(de isnonzero!> (wp wl w)
  (cond (!*trace
    (spaces wl)
    (prin2 wp)
    (cond (!*showexpr
      (prin2 " = ") (terpri)
      (algpri!> "  ") (algpri!> w) (algterpri!>)
      (spaces (sub1 wl))))
    (prin2 " is nonzero =>")
    (terpri))))

(de zt!> (we wp wl)
  (cond ((zerop we) (prog2 (iszero!> wp wl)       t))
	(t          (prog2 (isnonzero!> wp wl we) nil))))

(de scase15!> (w0 w1 w2 w3 w4)
  (prog (wi wf1 wf2 wdh)
    (setq wi (aeval (list 'plus (list 'times  3 w2 w2)
				(list 'times -4 w1 w3))))
    (setq wf1 (aeval (list 'plus (list 'times  2 w2 w3)
		     		 (list 'times -3 w1 w4))))
    (cond
     ((zt!> wi "I=3*W2^2-4*W1*W3" 2)
	(cond
	  ((zt!> wf1 "F1=2*W2*W3-3*W1*W4" 4) (return(finis!> "III")))
	  (t                                 (return(finis!> "I")))))
     (t (cond
	  ((zt!> wf1 "F1=2*W2*W3-3*W1*W4" 4) (return(finis!> "I")))
	  (t (setq wf2 (aeval (list 'plus (list 'times  9 w2 w4)
	       	                          (list 'times -8 w3 w3))))
	     (cond
	       ((zt!> wf2 "F2=9*W2*W4-8*W3^2" 6) (return(finis!> "I")))
	       (t (setq wdh (aeval (list 'plus (list 'times 3 wf1 wf1)
					       (list 'times 2 wi  wf2))))
		  (cond
		    ((zt!> wdh "D^=3*F1^2+2*I*F2" 8)
                       (return(finis!> "II")))
		    (t (return(finis!> "I"))))))))))))

(de scase30!> (w0 w1 w2 w3 w4)
  (prog (wi wf1 wf2 wdh)
    (setq wi (aeval (list 'plus (list 'times  3 w2 w2)
				(list 'times -4 w1 w3))))
    (setq wf1 (aeval (list 'plus (list 'times  2 w2 w1)
		     		 (list 'times -3 w3 w0))))
    (cond
     ((zt!> wi "I=3*W2^2-4*W1*W3" 2)
	(cond
	  ((zt!> wf1 "F1=2*W2*W1-3*W3*W0" 4) (return(finis!> "III")))
	  (t                                 (return(finis!> "I")))))
     (t (cond
	  ((zt!> wf1 "F1=2*W2*W1-3*W3*W0" 4) (return(finis!> "I")))
	  (t (setq wf2 (aeval (list 'plus (list 'times  9 w2 w0)
	       	                          (list 'times -8 w1 w1))))
	     (cond
	       ((zt!> wf2 "F2=9*W2*W0-8*W1^2" 6) (return(finis!> "I")))
	       (t (setq wdh (aeval (list 'plus (list 'times 3 wf1 wf1)
					       (list 'times 2 wi  wf2))))
		  (cond
		    ((zt!> wdh "D^=3*F1^2+2*I*F2" 8)
                       (return(finis!> "II")))
		    (t (return(finis!> "I"))))))))))))

(de scase23!> (w0 w1 w2 w3 w4)
  (prog (wi wjh wf3 wdt)
    (setq wi (aeval (list 'plus (list 'times   w0 w4)
				(list 'times 3 w2 w2))))
    (setq wjh (aeval (list 'plus (list 'times  4 w2 w4)
		     		 (list 'times -3 w3 w3))))
    (cond
     ((zt!> wi "I=W0*W4+3*W2^2" 2)
	(cond
	  ((zt!> wjh "J^=4*W2*W4-3*W3^2" 4) (return(finis!> "III")))
	  (t                                (return(finis!> "I")))))
     (t (cond
	  ((zt!> wjh "J^=4*W2*W4-3*W3^2" 4) (return(finis!> "I")))
	  (t (setq wf3 (aeval (list 'plus (list 'times    w0 wjh)
	       	                          (list 'times -2 w2 wi ))))
	     (cond
	       ((zt!> wf3 "F3=W0*J^-2*W2*I" 6) (return(finis!> "I")))
	       (t (setq wdt (aeval (list 'plus (list 'times    w4 wi wi)
					       (list 'times -3 wjh wf3))))
		  (cond
		    ((zt!> wdt "D~=W4*I^2-3*J^*F3" 8)
                       (return(finis!> "II")))
		    (t (return(finis!> "I"))))))))))))

(de scase29!> (w0 w1 w2 w3 w4)
  (prog (wi wjh wf3 wdt)
    (setq wi (aeval (list 'plus (list 'times   w0 w4)
				(list 'times 3 w2 w2))))
    (setq wjh (aeval (list 'plus (list 'times  4 w2 w0)
		     		 (list 'times -3 w1 w1))))
    (cond
     ((zt!> wi "I=W0*W4+3*W2^2" 2)
	(cond
	  ((zt!> wjh "J^=4*W2*W0-3*W1^2" 4) (return(finis!> "III")))
	  (t                                (return(finis!> "I")))))
     (t (cond
	  ((zt!> wjh "J^=4*W2*W0-3*W1^2" 4) (return(finis!> "I")))
	  (t (setq wf3 (aeval (list 'plus (list 'times    w4 wjh)
	       	                          (list 'times -2 w2 wi ))))
	     (cond
	       ((zt!> wf3 "F3=W4*J^-2*W2*I" 6) (return(finis!> "I")))
	       (t (setq wdt (aeval (list 'plus (list 'times    w0 wi wi)
					       (list 'times -3 wjh wf3))))
		  (cond
		    ((zt!> wdt "D~=W0*I^2-3*J^*F3" 8)
                       (return(finis!> "II")))
		    (t (return(finis!> "I"))))))))))))

(de scase27!> (w0 w1 w2 w3 w4)
  (prog (wv wu ww wi wj wd)
    (setq wv (aeval (list 'plus (list 'times    w0 w3 w3)
			        (list 'times -1 w1 w1 w4))))
    (cond
      ((zt!> wv "V=W0*W3^3-W1^2*W4" 2)
        (setq wu (aeval (list 'plus (list 'times   w0 w4)
			            (list 'times 2 w1 w3))))
	(cond
	  ((zt!> wu "U=W0*W4+2*W1*W3" 4) (return(finis!> "D")))
	  (t
             (setq ww (aeval (list 'plus (list 'times     w0 w4)
	       		                 (list 'times -16 w1 w3))))
	     (cond
	       ((zt!> ww "W=W0*W4-16*W1*W3" 6) (return(finis!> "II")))
	       (t                              (return(finis!> "I")))))))
      (t
         (setq wi (aeval (list 'plus (list 'times    w0 w4)
			             (list 'times -4 w1 w3))))
         (setq wj (aeval (list 'plus (list 'times -1 w0 w3 w3)
			             (list 'times -1 w1 w1 w4))))
	 (cond
	   ((ZT!> WI "I=W0*W4-4*W1*W3" 4)
	     (cond
	       ((zt!> wj "J=-W0*W3^2-W1^2*W4" 6) (return(finis!> "III")))
	       (t                                (return(finis!> "I")))))
	   ((zt!> wj "J=-W0*W3^2-W1^2*W4" 6) (return(finis!> "I")))
	   (t
	      (setq wd (aeval (list 'plus (list 'times      wi wi wi)
			                  (list 'times -27 wj wj   ))))
	      (cond
		((zt!> wd "D=I^3-27*J^2" 8) (return(finis!> "II")))
		(t                          (return(finis!> "I"))))))))))

(de scase31!> (w0 w1 w2 w3 w4)
  (prog (wh wf we wa wi wq wj wg wz wss wd)
    (setq wh (aeval (list 'plus (list 'times    w0 w2 )
			        (list 'times -1 w1 w1 ))))
    (cond
      ((zt!> wh "H=W0*W2-W1^2" 2)
        (setq wf (aeval (list 'plus (list 'times    w0 w3 )
			            (list 'times -1 w1 w2 ))))
        (setq we (aeval (list 'plus (list 'times    w0 w4 )
			            (list 'times -1 w2 w2 ))))
	(cond
	  ((zt!> wf "F=W0*W3-W1*W2" 4)
	    (cond
	      ((zt!> we "E=W0*W4-W2^2" 6) (return(finis!> "N")))
	      (t                          (return(finis!> "I")))))
	  ((zt!> we "E=W0*W4-W2^2" 6)
            (setq wq (aeval (list 'plus (list 'times 37 w2 w2 )
			                (list 'times 27 w1 w3 ))))
	    (cond
	      ((zt!> wq "Q=37*W2^2+27*W1*W3" 8) (return(finis!> "II")))
	      (t                                (return(finis!> "I")))))
	  (t
             (setq wa (aeval (list 'plus (list 'times    w1 w3 )
			                 (list 'times -1 w2 w2 ))))
             (setq wi (aeval (list 'plus we (list 'times -4 wa ))))
	     (cond
	       ((zt!> wi "A=W1*W3-W2^2; I=E-4*A" 8) (return(finis!> "I")))
	       (t
		  (setq wj (aeval (list 'plus (list 'times    w4 wh )
			                      (list 'times -1 w3 wf )
			                      (list 'times    w2 wa ))))
                  (setq wd (aeval (list 'plus (list 'times     wi wi wi )
			                      (list 'times -27 wj wj ))))
		  (cond
		    ((zt!> wd "J=W4*H-W3*F+W2*A; D=I^3-27*J^2" 10)
			   (return(finis!> "II")))
		    (t     (return(finis!> "I")))))))))
      (t
         (setq wf (aeval (list 'plus (list 'times    w0 w3 )
			             (list 'times -1 w1 w2 ))))
         (setq we (aeval (list 'plus (list 'times    w0 w4 )
			             (list 'times -1 w2 w2 ))))
         (setq wa (aeval (list 'plus (list 'times    w1 w3 )
			             (list 'times -1 w2 w2 ))))
         (setq wi (aeval (list 'plus we (list 'times -4 wa ))))
	 (cond
	   ((zt!> wi "E=W0*W4-W2^2; A=W1*W3-W2^2; I=E-4*A" 4)
             (setq wf (aeval (list 'plus (list 'times    w0 w3 )
			                 (list 'times -1 w1 w2 ))))
	     (setq wj (aeval (list 'plus (list 'times    w4 wh )
			                 (list 'times -1 w3 wf )
			                 (list 'times    w2 wa ))))
	     (cond
	       ((zt!> wj "F=W0*W3-W1*W2; J=W4*H-W3*F+W2*A" 6)
                         (return(finis!> "III")))
	       (t        (return(finis!> "I")))))
	   (t
              (setq wf (aeval (list 'plus (list 'times    w0 w3 )
			                  (list 'times -1 w1 w2 ))))
	      (setq wg (aeval (list 'plus (list 'times    w0 wf )
			                  (list 'times -2 w1 wh ))))
	      (cond
		((zt!> wg "G=W0*F-2*W1*H" 6)
                  (setq wz (aeval (list 'plus (list 'times     w0 w0 wi )
			                      (list 'times -12 wh wh ))))
		  (cond
		    ((zt!> WZ "Z=W0^2*I-12*H^2" 8) (return(finis!> "D")))
		    (t
                       (setq wss (aeval (list 'plus (list 'times    w0 w0 wi )
			                            (list 'times -3 wh wh ))))
		       (cond
			 ((zt!> wss "S=W0^2*I-3*H^2" 10)
				(return(finis!> "II")))
			 (t     (return(finis!> "I")))))))
		(t
	           (setq wj (aeval (list 'plus (list 'times    w4 wh )
			                       (list 'times -1 w3 wf )
			                       (list 'times    w2 wa ))))
	           (cond
	             ((zt!> wj "J=W4*H-W3*F+W2*A" 8) (return(finis!> "I")))
	             (t
                        (setq wd (aeval (list 'plus (list 'times     wi wi wi )
			                            (list 'times -27 wj wj ))))
			(cond
			  ((zt!> wd "D=I^3-27*J^3" 10)
				 (return(finis!> "II")))
			  (t     (return(finis!> "I"))))))))))))))


%------- EM Type 06.96 ----------------------------------------------------

(de emtype!> (lst)
  (prog (w0 w1 w2 wc wr wd)
    (cond (!*trace
      (prin2 "EM strength classification ...") (terpri)))
    (setq w0 (aeval (nz!> (getel1!> lst 0 ))))
    (setq w1 (aeval (nz!> (getel1!> lst 1 ))))
    (setq w2 (aeval (nz!> (getel1!> lst 2 ))))
    (setq wc (plus (times 4 (to1!> w0))
		   (times 2  (to1!> w1))
		   (times 1  (to1!> w2)) ))
    (cond (!*trace
      (prin2 "Case ") (prin2 wc) (prin2 ": ")
      (foreach!> x in (list w0 w1 w2) do (progn
	(prin2 " ") (cond ((zerop x) (prin2 0)) (t (prin2 "N")))))
      (prin2 "  =>")
      (terpri) ))
    (setq wr
      (cond
	((eqn wc 0) (emfinis!> "0"))
	((eqn wc 1) (emfinis!> "N"))
	((eqn wc 2) (emfinis!> "I"))
	((eqn wc 3) (emfinis!> "I"))
	((eqn wc 4) (emfinis!> "N"))
	((eqn wc 5) (emfinis!> "I"))
	((eqn wc 6) (emfinis!> "I"))
	((eqn wc 7)
           (setq wd (aeval (list 'plus (list 'times    w0 w2)
				       (list 'times -1 w1 w1))))
           (cond
	     ((zt!> wd "D=F0*F2-F1^2" 2) (emfinis!> "N"))
	     (t                          (emfinis!> "I"))))))
    (return wr)))

(de emfinis!> (w)
  (progn
    (prin2 "EM type is ")
    (prin2 w)
    (prin2 ".")
    (terpri)
    w))

%------- Ricci spinor classification 06.96 --------------------------------

(de riccisclass!> (lst)
  (prog (f00 f01 f02 f11 f12 f22 w0 w1 w2 w3 w4 wc wr wpp wi6 ww
         wq ws1 ws2 ws3 ws4 ws5 ws6 ws7 wip wi7)
    (cond (!*trace
      (prin2 "Ricci Spinor classification ...") (terpri)
      (prin2 "  Using algorithm by G.C.Joly, M.A.H.McCallum & W.Seixas") (terpri)
      (prin2 "    Class. Quantum Grav. 7 (1990) 541-556") (terpri)
      (prin2 "    Class. Quantum Grav. 8 (1991) 1577-1585") (terpri)))
    (setq f00 (aeval (nz!> (getel2!> lst 0 0))))
    (setq f01 (aeval (nz!> (getel2!> lst 0 1))))
    (setq f02 (aeval (nz!> (getel2!> lst 0 2))))
    (setq f11 (aeval (nz!> (getel2!> lst 1 1))))
    (setq f12 (aeval (nz!> (getel2!> lst 1 2))))
    (setq f22 (aeval (nz!> (getel2!> lst 2 2))))
    (setq wc (mapcar (list f00 f01 f02 f11 f12 f22) 'to1!>))
    (cond (!*trace
      (prin2 "Case ")
      (foreach!> x in wc do (prin2 x))
      (prin2 " =>")
      (terpri) ))
    % Special cases ...
    (setq wr
      (cond
    	((equal wc '(0 0 0 0 0 0)) (rfin!> "0" "[(1111)]"))
    	((equal wc '(0 0 0 1 0 0)) (rfin!> "D" "[(11)(1,1)]"))
    	((equal wc '(0 0 1 0 0 0)) (rfin!> "D" "[11(1,1)]"))
    	((equal wc '(0 0 0 0 0 1)) (rfin!> "0" "[(112)]"))
    	((equal wc '(1 0 0 0 0 0)) (rfin!> "0" "[(112)]"))
    	((equal wc '(0 0 0 1 0 1)) (rfin!> "D" "[(11)2]"))
    	((equal wc '(1 0 0 1 0 0)) (rfin!> "D" "[(11)2]"))
    	((equal wc '(0 0 1 0 0 1)) (rfin!> "II" "[112]"))
    	((equal wc '(1 0 1 0 0 0)) (rfin!> "II" "[112]"))
    	((equal wc '(0 0 0 0 1 0)) (rfin!> "N" "[(13)]"))
    	((equal wc '(0 1 0 0 0 0)) (rfin!> "N" "[(13)]"))
    	((equal wc '(0 0 0 1 1 0)) (rfin!> "D" "[(11)2]"))
    	((equal wc '(0 1 0 1 0 0)) (rfin!> "D" "[(11)2]"))
    	((equal wc '(0 0 0 0 1 1)) (rfin!> "N" "[(13)]"))
    	((equal wc '(1 1 0 0 0 0)) (rfin!> "N" "[(13)]"))
    	((equal wc '(0 1 0 0 0 1)) (rfin!> "I" "[11ZZ~]"))
    	((equal wc '(1 0 0 0 1 0)) (rfin!> "I" "[11ZZ~]"))
      ))
    (cond (wr (return wr)))
    % General case ...
    %  PP type first ...
    (setq w0 (aeval(wff!> 0 lst)))
    (setq w1 (aeval(wff!> 1 lst)))
    (setq w2 (aeval(wff!> 2 lst)))
    (setq w3 (aeval(wff!> 3 lst)))
    (setq w4 (aeval(wff!> 4 lst)))
    (cond (!*trace
      (prin2 "Making Petrov-Plebanski (PP) classification ...")
      (terpri)))
    (setq wpp (petrov!> (list w0 w1 w2 w3 w4)))
    % Segre type ...
    (setq wr
      (cond
	((equal wpp "0"  )
	   (setq ww (aeval (list 'plus
             (list 'times    f11 f11)
	     (list 'times -1 f12 (gfab!> 1 0 lst)))))
	   (cond
	     ((zt!> ww "W=F11'^2-F10'*F12'" 2) (rfin!> wpp "[(112)]"))
	     ((zt!> f00 "F00" 4)               (rfin!> wpp "[1(11,1)]"))
	     ((zt!> f22 "F22" 4)               (rfin!> wpp "[1(11,1)]"))
	     (t  (rfincond!> wpp "[(111),1]"
				 " if W>0 and "
				 "[1(11,1)]"
				 " if W<0"))))
	((equal wpp "I"  ) (rfincond!> wpp "[111,1]"
                                           " if D>0 and "
                                           "[11ZZ~]"
                                           " if D<0"))
	((equal wpp "II" ) (rfin!> wpp "[112]"))
	((equal wpp "III") (rfin!> wpp "[13]"))
	((equal wpp "N"  )
	   (setq wi6 (aeval (list 'plus
	     (list 'times    (gfab!> 0 0 lst) (gfab!> 2 2 lst))
	     (list 'times  2 (gfab!> 1 1 lst) (gfab!> 1 1 lst))
	     (list 'times -2 (gfab!> 0 1 lst) (gfab!> 2 1 lst))
	     (list 'times -2 (gfab!> 1 0 lst) (gfab!> 1 2 lst))
	     (list 'times    (gfab!> 0 2 lst) (gfab!> 2 0 lst)))))
	   (cond
	     ((zt!> wi6 "I6" 2) (rfin!> wpp "[(13)]"))
	     (t                 (rfin!> wpp "[1(12)]"))))
	((equal wpp "D"  )
	   (setq wi6 (aeval (list 'plus
	     (list 'times    (gfab!> 0 0 lst) (gfab!> 2 2 lst))
	     (list 'times  2 (gfab!> 1 1 lst) (gfab!> 1 1 lst))
	     (list 'times -2 (gfab!> 0 1 lst) (gfab!> 2 1 lst))
	     (list 'times -2 (gfab!> 1 0 lst) (gfab!> 1 2 lst))
	     (list 'times    (gfab!> 0 2 lst) (gfab!> 2 0 lst)))))
	   (cond
	     ((zt!> wi6 "I6" 2) (rfin!> wpp "[(11)ZZ~]"))
	     (t
		(setq wip (aeval (list 'plus
		   (list 'times     w0 w4)
		   (list 'times  -4 w1 w3)
		   (list 'times   3 w2 w2))))
	        (setq ww (aeval (list 'plus
                   (list 'times    f11 f11)
	           (list 'times -1 f12 (gfab!> 1 0 lst)))))
		(setq wq (aeval
                  (list 'plus wip
		    (list 'times -3 (list 'expt (list 'plus w2 ww) 2)))))
		(cond
		  ((zt!> wq "Q" 4)
		     (setq ws1 (aeval (list 'plus
		       (list 'times    (gfab!> 2 0 lst) (gfab!> 1 2 lst))
		       (list 'times -1 (gfab!> 1 0 lst) (gfab!> 2 2 lst)))))
		     (setq ws2 (aeval (list 'plus
		       (list 'times    (gfab!> 0 0 lst) (gfab!> 2 2 lst))
		       (list 'times -1 (gfab!> 2 0 lst) (gfab!> 0 2 lst)))))
		     (setq ws3 (aeval (list 'plus
		       (list 'times    (gfab!> 1 0 lst) (gfab!> 0 2 lst))
		       (list 'times -1 (gfab!> 0 0 lst) (gfab!> 1 2 lst)))))
		     (setq ws4 (aeval (list 'plus
		       (list 'times    (gfab!> 0 0 lst) (gfab!> 1 1 lst))
		       (list 'times -1 (gfab!> 1 0 lst) (gfab!> 0 1 lst)))))
		     (setq ws5 (aeval (list 'plus
		       (list 'times    (gfab!> 0 1 lst) (gfab!> 1 2 lst))
		       (list 'times -1 (gfab!> 0 2 lst) (gfab!> 1 1 lst)))))
		     (setq ws6 (aeval (list 'plus
		       (list 'times    (gfab!> 1 1 lst) (gfab!> 2 2 lst))
		       (list 'times -1 (gfab!> 1 2 lst) (gfab!> 2 1 lst)))))
		     (setq wi7 (aeval (list 'plus
		       (list 'times f01 ws1)
		       (list 'times f11 ws2)
		       (list 'times (gfab!> 2 1 lst) ws3))))
		     (cond
		       ((and (zt!> ws1 "S1" 6)
                             (zt!> ws2 "S2" 6)
                             (zt!> ws3 "S3" 6))
			 (cond
			   ((and (zt!> ws4 "S4" 6)
			         (zt!> ws5 "S5" 6)
			         (zt!> ws6 "S6" 6))
                               (rfin!> wpp "[(11)(1,1)]"))
			   (t  (rfin!> wpp "[(11)2]"))))
		       ((zt!> wi7 "I7" 6) (rfin!> wpp "[(11)2]"))
		       (t                 (rfin!> wpp "[11ZZ~]"))))
		  (t (rfincond!> wpp "[(11)ZZ~]"
				     " if S7<0 and "
				     "[(11)1,1] or [11(1,1)]"
				     " if S7>0"))))
	))))
    (return wr)))

(de rfin!> (wpp wss)
  (progn
    (prin2 "Petrov-Plebanski type is ")
    (prin2 wpp)
    (prin2 ".") (terpri)
    (prin2 "Segre type is ")
    (prin2 wss)
    (prin2 ".")
    (terpri)
    (cons wpp wss)))

(de rfincond!> (wpp wss1 wcc1 wss2 wcc2)
  (progn
    (prin2 "PP type is ")
    (prin2 wpp)
    (prin2 ".") (terpri)
    (prin2 "Segre type is ")
    (prin2 wss1)
    (prin2 wcc1)
    (prin2 wss2)
    (prin2 wcc2)
    (prin2 ".")
    (terpri)
    (cons wpp (cons wss1 wss2))))

(de gfab!> (wa wb lst)
  (cond ((lessp wb wa) (nz!>(coalg!>(getel2!> lst wb wa))))
	(t             (nz!>        (getel2!> lst wa wb)))))

(de ffabsum!> (wa wb lst)
  (list 'quotient
    (list 'plus
      (list 'times (gfab!> wa 0 lst) (gfab!> wb 2 lst))
      (list 'times (gfab!> wa 2 lst) (gfab!> wb 0 lst))
      (list 'times -2 (gfab!> wa 1 lst) (gfab!> wb 1 lst)) )
    4))

(de wff!> (wa lst)
  (cond
    ((eqn wa 0) (ffabsum!> 0 0 lst))
    ((eqn wa 1) (list 'quotient
                  (list 'plus (ffabsum!> 0 1 lst) (ffabsum!> 1 0 lst))
		  2))
    ((eqn wa 2) (list 'quotient
                  (list 'plus (ffabsum!> 0 2 lst) (ffabsum!> 2 0 lst)
			      (list 'times 4 (ffabsum!> 1 1 lst)))
		  6))
    ((eqn wa 3) (list 'quotient
                  (list 'plus (ffabsum!> 1 2 lst) (ffabsum!> 2 1 lst))
		  2))
    ((eqn wa 4) (ffabsum!> 2 2 lst))
    ))

%--------- Vector and Scalar classification 06.96 -------------------------

(de scaltype!> (lst)
  (prog (w)
    (setq w (aeval(nz!>(car lst))))
    (cond ((zerop w) (prin2 "Scalar is 0.") (terpri))
	  (t         (prin2 "Scalar is nonzero.") (terpri)))
    (return (to1!> w))))

(de vectype!> (lst)
  (prog (v01 v10 v00 v11 w)
    (setq v00 (aeval (gfab!> 0 0 lst)))
    (setq v01 (aeval (gfab!> 0 1 lst)))
    (setq v10 (aeval (gfab!> 1 0 lst)))
    (setq v11 (aeval (gfab!> 1 1 lst)))
    (setq w (aeval (list 'plus (list 'times  2 v01 v10)
		               (list 'times -2 v00 v11))))
    (cond
      ((zt!> w "2*V01'*V10'-2*V00'*V11'" 2)
	 (prin2 "Vector is Null.") (terpri))
      (t (prin2 "Vector is Time or Space-like.") (terpri)))
    (return (to1!> w))))


%===========   End of GRGclass.sl  =========================================%

