%==========================================================================%
%   GRGmater.sl               Matter fields: EM, YM, Scalar, Dirac, Fluid  %
%==========================================================================%
%   GRG 3.2 Standard Lisp Source Code       (C) 1988-96 Vadim V. Zhytnikov %
%==========================================================================%
% This file is distributed without any warranty. You may modify it but you %
% are not allowed to remove author's name and/or distribute modified file. %
%==========================================================================%

%---------- YM field. 09.96 -----------------------------------------------

(de sconst!> (wa wb wc)
  (gets0!> !#!S!C!O!N!S!T (list3 wa wb wc) '((a 1 2 3)) 0))

% FFYM = d AYM + AYM/\AYM
(de ffymfromaym!> nil
  (prog (w wc)
    (makebox!> '!#!F!F!Y!M)
    (for!> x (0 1 9) do (progn
      (setq w (ncons (dex!> (getel1!> !#!A!Y!M x))))
      (for!> y (0 1 9) do (for!> z (0 1 9) do
	(progn
          (setq wc (sconst!> x y z))
	  (cond (wc
	    (setq w (cons
              (fndfpr!> (list 'quotient wc 2)
                            (dfprod2!> (getel1!> !#!A!Y!M y)
                                       (getel1!> !#!A!Y!M z))) w)))))))
       (putel1!> (evalform!> (dfsum!> w)) !#!F!F!Y!M x)))
     (return t)))

% First YM equation ...
(de firstym!> nil
  (prog (w wc)
    (makebox!> '!#!Y!M!F!q)
    (for!> x (0 1 9) do (progn
      (setq w (ncons (dex!> (dual!> (getel1!> !#!F!F!Y!M x)))))
      (for!> y (0 1 9) do (for!> z (0 1 9) do
	(progn
          (setq wc (sconst!> x y z))
	  (cond (wc
	    (setq w (cons
              (fndfpr!> wc (dfprod2!>
                             (getel1!> !#!A!Y!M y)
                             (dual!>(getel1!> !#!F!F!Y!M z)))) w)))))))
       (putel1!> (equation!> (evalform!> (dfsum!> w)) nil) !#!Y!M!F!q x)))
     (return t)))

% Second YM equation ...
(de secondym!> nil
  (prog (w wc)
    (makebox!> '!#!Y!M!S!q)
    (for!> x (0 1 9) do (progn
      (setq w (ncons (dex!> (getel1!> !#!F!F!Y!M x))))
      (for!> y (0 1 9) do (for!> z (0 1 9) do
	(progn
          (setq wc (sconst!> x y z))
	  (cond (wc
	    (setq w (cons
              (fndfpr!> wc (dfprod2!>
                             (getel1!> !#!A!Y!M y)
                             (getel1!> !#!F!F!Y!M z))) w)))))))
       (putel1!> (equation!> (evalform!> (dfsum!> w)) nil) !#!Y!M!S!q x)))
     (return t)))

% YMACT = -1/8/pi FFYM/\*FFYM
(de ymact!> nil
  (prog (w)
    (for!> x (0 1 9) do (progn
      (setq w (cons (dfprod2!> (getel1!> !#!F!F!Y!M x)
			       (dual!> (getel1!> !#!F!F!Y!M x))) w))))
    (setq w (evalform!>
              (fndfpr!> '(quotient (minus 1) (times 8 pi))
			 (dfsum!> w))))
    (setq !#!Y!M!A!C!T (ncons w))))

% FFYM = 1/2 FTYM.a.b S'a'b
(de ffymfromftym!> nil
  (prog (w)
    (makebox!> '!#!F!F!Y!M)
    (for!> x (0 1 9) do (progn
      (setq w nil)
      (fordim!> a do (fordim!> b do  (cond ((lessp a b)
        (setq w (cons (fndfpr!> (getel!> !#!F!T!Y!M (list x a b))
			        (getel2!> !#!S a b)) w))))))
      (putel1!> (evalform!> (dfsum!> w)) !#!F!F!Y!M x)))))

% FTYM.a.b = D.b _| D.a _| FFYM
(de ftymfromffym!> nil
  (prog nil
    (makebox!> '!#!F!T!Y!M)
    (for!> x (0 1 9) do
      (fordim!> a do (fordim!> b do  (cond ((lessp a b)
        (putel!> (evalalg!>
		   (vform1!> (getiframe!> b)
		     (vform!> (getiframe!> a) (getel1!> !#!F!F!Y!M x))))
	         !#!F!T!Y!M (list3 x a b)))))))
    (return t)))

% TYM
(de tymbydef!> nil
  (prog (w wr)
    (setq !#!T!Y!M (mkt!> 2))
    (setq w (list 'times -1 ![sigprod!] (car ![sgn!])
                            (duald!> (car !#!Y!M!A!C!T))))
    (fordim!> a do (fordim!> b do (cond ((leq a b)
      (setq wr (ncons  (list 'times (getmetr!> a b) w)))
      (fordim!> m do (for!> x (0 1 9) do
	(setq wr (cons (list 'times -1
			 (list 'quotient (car ![sgn!]) '(times 4 pi))
                         (getm!> '!#!F!T!Y!M nil (list3 x a m) '(nil nil nil))
			 (getm!> '!#!F!T!Y!M nil (list3 x b m) '(nil nil 1)))
                       wr))))
      (putel!> (evalalg!> (cons 'plus wr)) !#!T!Y!M (list2 a b))))))))


%---------- EM field. 09.96 -----------------------------------------------

% FF = d A
(de fffroma!> nil
  (setq !#!F!F (ncons (evalform!> (dex!> (car !#!A))))))

% FF = 1/2 FT.a.b S'a'b
(de fffromft!> nil
  (prog (w)
    (fordim!> a do (fordim!> b do  (cond ((lessp a b)
      (setq w (cons (fndfpr!> (getel2!> !#!F!T a b)
			      (getel2!> !#!S a b)) w))))))
    (setq !#!F!F (ncons (evalform!> (dfsum!> w))))))

% FT.a.b = D.b _| D.a _| FF
(de ftfromff!> nil
  (prog nil
    (setq !#!F!T (mkt!> 2))
    (fordim!> a do (fordim!> b do  (cond ((lessp a b)
      (putel!> (evalalg!>
		 (vform1!> (getiframe!> b)
		   (vform!> (getiframe!> a) (car !#!F!F))))
	       !#!F!T (list2 a b))))))
    (return t)))

% EMACT = -1/8/pi FF/\*FF
(de emact!> nil
  (setq !#!E!M!A!C!T (ncons (evalform!>
    (fndfpr!> '(quotient (minus 1) (times 8 pi))
	       (dfprod2!> (car !#!F!F) (dual!> (car !#!F!F))))))))

% TEM
(de tembydef!> nil
  (prog (w wr)
    (setq !#!T!E!M (mkt!> 2))
    (setq w (list 'times -1 ![sigprod!] (car ![sgn!])
                            (duald!> (car !#!E!M!A!C!T))))
    (fordim!> a do (fordim!> b do (cond ((leq a b)
      (setq wr (ncons  (list 'times (getmetr!> a b) w)))
      (fordim!> m do
	(setq wr (cons (list 'times -1
			      (list 'quotient (car ![sgn!]) '(times 4 pi))
                              (getasy2!> !#!F!T a m nil)
			      (getm!> '!#!F!T nil (list2 b m) '(nil 1)))
                       wr)))
      (putel!> (evalalg!> (cons 'plus wr)) !#!T!E!M (list2 a b))))))))

% d # FF = # J
(de firstmw!> nil
  (setq !#!M!W!F!q (ncons
    (equation!> (evalform!> (dex!> (dual!> (car !#!F!F))))
		(cond
                  ((not !#!J) nil)
		  (t (evalform!> (fndfpr!> (list 'times 4 'pi (car ![sgn!])
					     (list 'expt -1
					           (difference ![dim!] 2)))
					   (dual!> (car !#!J))))))
                ))))

% d FF = 0
(de secondmw!> nil
  (setq !#!M!W!S!q (ncons
    (equation!> (evalform!> (dex!> (car !#!F!F))) nil))))

% d # J =0
(de contineq!> nil
  (setq !#!C!O!q (ncons
    (equation!> (evalform!> (dex!> (dual!> (car !#!J)))) nil))))

% First scalar ...
(de firstscal!> nil
  (setq !#!S!C!F (ncons (evalalg!> (chsigna!> (duald!>
    (dfprod2!> (car !#!F!F) (dual!> (car !#!F!F)))))))))

% Second scalar ...
(de secondscal!> nil
  (setq !#!S!C!S (ncons (evalalg!> (duald!>
    (dfprod2!> (car !#!F!F) (car !#!F!F)))))))

% FFU = FF - i #FF
(de ffufromff!> nil
  (setq !#!F!F!U (ncons (evalform!>
    (dfsum2!> (car !#!F!F)
	      (fndfpr!> '(minus i) (dual!> (car !#!F!F))))))))

% FFU = 2 FIU_AB SU^AB
(de ffufromfiu!> nil
  (setq !#!F!F!U (ncons (evalform!> (fndfpr!> 2 (dfsum!> (list
    (fndfpr!> (getel1!> !#!F!I!U 0) (getel1!> !#!S!U 2))
    (fndfpr!> (getel1!> !#!F!I!U 2) (getel1!> !#!S!U 0))
    (fndfpr!> -2 (fndfpr!> (getel1!> !#!F!I!U 1) (getel1!> !#!S!U 1))))))))))

% FF= 1/2 (FFU + ~FFU)
(de fffromffu!> nil
  (setq !#!F!F (ncons (evalform!> (fndfpr!> '(quotient 1 2)
    (dfsum2!> (car !#!F!F!U) (coform!> (car !#!F!F!U))))))))

% FIU_AB = -i/2 # ( FFU/\SU_AB )
(de fiufromffu!> nil
  (prog nil
    (makebox!> '!#!F!I!U)
    (for!> a (0 1 2) do
      (putel1!> (evalalg!> (list 'times '(quotient (minus i) 2)
			      (duald!>
				 (dfprod2!> (car !#!F!F!U)
					    (getel1!> !#!S!U a)))))
		!#!F!I!U a ))))

% FIU_AB = -i # ( FF/\SU_AB )
(de fiufromff!> nil
  (prog nil
    (makebox!> '!#!F!I!U)
    (for!> a (0 1 2) do
      (putel1!> (evalalg!> (list 'times '(minus i)
			      (duald!>
				 (dfprod2!> (car !#!F!F)
					    (getel1!> !#!S!U a)))))
		!#!F!I!U a ))))

% SCU = 2 FIU_AB FIU^AB
(de scufromfiu!> nil
  (setq !#!S!C!U (ncons (evalalg!> (list 'times 4 (list 'plus
    (list 'times (getel1!> !#!F!I!U 2) (getel1!> !#!F!I!U 0))
    (list 'times -1 (getel1!> !#!F!I!U 1) (getel1!> !#!F!I!U 1)) ))))))

% SCU = -i/2 #( FFU/\FFU )
(de scufromffu!> nil
  (setq !#!S!C!U (ncons (evalalg!> (list 'times '(quotient (minus i) 2)
    (duald!> (dfprod2!> (car !#!F!F!U) (car !#!F!F!U))))))))

% TEMS.AA.BB
(de tems!> nil
  (prog nil
    (makebox!> '!#!T!E!M!S)
    (for!> a (0 1 2) do (for!> b (0 1 2) do (cond ((leq a b)
      (putel!> (evalalg!> (list 'times '(quotient 1 (times 2 pi))
			    (getel1!> !#!F!I!U a)
			    (coalg!> (getel1!> !#!F!I!U b))))
	       !#!T!E!M!S (list2 a b))))))))

% d FFU = i #J
(de complexmw!> nil
  (setq !#!M!W!U!q (ncons
    (equation!> (evalform!> (dex!> (car !#!F!F!U)))
		(cond
                  ((not !#!J) nil)
		  (t (evalform!> (fndfpr!> (list 'times -4 'i 'pi
                                                        (car ![sgn!]))
					   (dual!> (car !#!J))))))
                ))))

% FIU/\SD_AA = 0
(de sduality!> nil
  (prog nil
    (makebox!> '!#!S!D!q)
    (for!> a (0 1 2) do
      (putel1!> (equation!> (evalform!>
                              (dfprod2!> (car !#!F!F!U)
					 (getel1!> !#!S!D a))) nil)
		!#!S!D!q  a))))

%---------- Scalar field. 01.91, 09.96 ------------------------------------

% Scalar field action (minimal interaction) ...
(de sactmin!> nil
  (prog (w wr wss)
    (setq w (car !#!F!I))
    (fordim!> a do (fordim!> b do
      (setq wr (cons (list 'times (dfisign!>)
                                  (list 'df w (x!> a))
				  (list 'df w (x!> b))
				  (gimetr!> a b)) wr))))
    (setq wr (cons (list 'times '(expt !S!M!A!S!S 2) w w) wr))
    (setq wr (cons 'plus wr))
    (setq !#!S!A!C!T!M!I!N (ncons (evalform!>
      (fndfpr!> '(quotient -1 2) (fndfpr!> wr (car !#!V!O!L))))))
    (return t)))

(de dfisign!> nil
  (cond ((lessp (car ![sgn!]) 0)  1 )
	(t                       -1 )))

% Scalar field action ...
(de sact!> nil
  (prog (w wr wss)
    (setq w (car !#!F!I))
    (fordim!> a do (fordim!> b do
      (setq wr (cons (list 'times (dfisign!>)
                                  (list 'df w (x!> a))
				  (list 'df w (x!> a))
				  (gimetr!> a b)) wr))))
    (setq wr (cons (list 'times '(expt !S!M!A!S!S 2) w w) wr))
    (cond (!*nonmin
      (setq wr (cons (list 'times (car !#!A!C!O!N!S!T)
                                  (car !#!R!R)
				  w w ) wr))))
    (setq wr (cons 'plus wr))
    (setq !#!S!A!C!T (ncons (evalform!>
      (fndfpr!> '(quotient -1 2) (fndfpr!> wr (car !#!V!O!L))))))
    (return t)))

% Scalar field equation ...
(de kgeq!> nil
  (prog (w wf)
    (setq wf (car !#!F!I))
    (setq w (list 'plus
      (list 'times (dfisign!>) ![sigprod!] (list 'expt -1 ![dim1!])
                   (duald!> (dex!> (dual!> (dfun!> wf)))) )
      (list 'times -1 wf (list 'plus '(expt !S!M!A!S!S 2)
				     (cond (!*nonmin (list 'times
						       (car !#!A!C!O!N!S!T)
						       (car !#!R!R)))
					   (t 0))))  ))
    (setq !#!S!C!q (ncons (equation!> (evalalg!> w) nil)))
    (return t)))

% Scalar energy-momentum tensor (minimal interaction) ...
(de tsclmin!> nil
  (prog (w)
    (setq !#!T!S!C!L!M!I!N (mkt!> 2))
    (setq w (duald!> (car !#!S!A!C!T!M!I!N)))
    (setq w (evalalg!> (list 'times -1 ![sigprod!] (car ![sgn!]) w)))
    (fordim!> wa do (fordim!> wb do
      (cond ((leq wa wb)
        (putel!> (evalalg!> (list 'plus
                         (list 'times (vfun!> (getiframe!> wa) (car !#!F!I))
                                      (vfun!> (getiframe!> wb) (car !#!F!I)))
                         (list 'times (getmetr!> wa wb) w)))
                 !#!T!S!C!L!M!I!N (list2 wa wb))))))
    (return t)))


%----------  Dirac field. 12.90, 9.96  ------------------------------------

% Current 1-form from Dirac spinor ...
(de dcurr!> nil
  (progn
   (setq !#!J (ncons (evalform!>
     (fndfpr!> (list 'times (mp!> 1) '(sqrt 2) '!E!C!O!N!S!T)
       (dfsum!> (list2 (spintetr!> !#!C!H!I)
                       (spintetr!> !#!P!H!I) ))))))
   t))

(de spintetr!> (wss)
  (dfsum!> (list
    (fndfpr!> (getel1!> wss 0)
      (fndfpr!> (coalg!>(getel1!> wss 0)) (getframe!> 1)))
    (fndfpr!> (getel1!> wss 1)
      (fndfpr!> (coalg!>(getel1!> wss 1)) (getframe!> 0)))
    (fndfpr!> (getel1!> wss 0)
      (fndfpr!> (coalg!>(getel1!> wss 1)) (getframe!> 3)))
    (fndfpr!> (getel1!> wss 1)
      (fndfpr!> (coalg!>(getel1!> wss 0)) (getframe!> 2))) )))

% Covariant derivative with ieA and 1/2 Q terms ...
% wi - index, wss - spinor, bool=t - 1/2 Q term must be included
% wc=t +i for phi and wc=nil -i for chi
(de dexcs!> (wi wss bool wc)
  (prog (w)
    (setq w (list (dfun!>(getel1!> wss wi))
                  (chsign!> t (fndfpr!> (getel1!> wss 0)
                                (pmf!>(getel1!> !#!o!m!e!g!a!u (add1 wi)))))
                  (fndfpr!> (getel1!> wss 1)
                            (pmf!>(getel1!> !#!o!m!e!g!a!u wi)))))
    (cond (!#!A
      (setq w (cons (fndfpr!> (list 'times '!E!C!O!N!S!T
				    (cond (wc 'i) (t '(minus i))))
                      (fndfpr!> (getel1!> wss wi) (car !#!A))) w))))
    (cond ((and bool !*torsion)
      (setq w (cons (fndfpr!> '(quotient -1 2)
                       (fndfpr!> (getel1!> wss wi) (car !#!Q!Q))) w))))
    (return(dfsum!> w))))

% Dirac equation ...
(de dequ!> (wa wb we wc)
  (prog nil
    (set we (mkbox!> we))
    (for!> a (0 1 1) do
      (putel1!> (equation!>
        (evalalg!> (list 'plus
              (list 'times '(sqrt 2) (mpa!> 'i) (dfcs!> a wa wc))
              (list 'times  '(minus !D!M!A!S!S) (coalg!>(getel1!> wb a)))))
	nil)
	(eval we) a))
    (return t)))

(de dfcs!> (wi wss wc)
  (list 'plus (vform1!> (getiframe!> (cond ((eqn wi 0) 2) (t 0)))
                        (dexcs!> 0 wss t wc))
              (vform1!> (getiframe!> (cond ((eqn wi 0) 1) (t 3)))
                        (chsign!> t (dexcs!> 1 wss t wc)))))

% Dirac action 4-form ...
(de dact!> nil
  (prog (www)
    (setq www (list
      (fndfpr!> '(quotient i (sqrt 2))  (sdstetr!> !#!P!H!I t))
      (fndfpr!> '(quotient (minus i) (sqrt 2))  (sdstetr!> !#!C!H!I nil))
      (fndfpr!> '(minus !D!M!A!S!S)
        (fndfpr!> (scaldir!>) (car !#!V!O!L))) ))
    (setq www (append www (mapcar www (function coform!>))))
    (setq !#!D!A!C!T (ncons(evalform!>(dfsum!> www))))
    (return t)))

(de sdstetr!> (wss wc)
  (dfsum!> (list
    (dfprod2!> (dual!>(dexcs!> 0 wss nil wc))
      (fndfpr!> (coalg!>(getel1!> wss 0)) (getframe!> 1)))
    (dfprod2!> (dual!>(dexcs!> 1 wss nil wc))
      (fndfpr!>(coalg!>(getel1!> wss 1)) (getframe!> 0)))
    (dfprod2!> (dual!>(dexcs!> 0 wss nil wc))
      (fndfpr!>(coalg!>(getel1!> wss 1)) (getframe!> 3)))
    (dfprod2!> (dual!>(dexcs!> 1 wss nil wc))
      (fndfpr!>(coalg!>(getel1!> wss 0)) (getframe!> 2))) )))

(de scaldir!> nil
  (list 'plus
    (list 'times    (getel1!> !#!P!H!I 0) (getel1!> !#!C!H!I 1))
    (list 'times -1 (getel1!> !#!P!H!I 1) (getel1!> !#!C!H!I 0))))

% Dirac spin 3-form ...
(de spinsd!> nil
  (prog (w)
    (setq !#!S!P!D!I!U (mkbox!> '!#!S!P!D!I!U))
    (setq w '(quotient i (sqrt 8)))
    (putel1!> (evalform!> (fndfpr!> w (dual!> (dfsum!> (list
      (sst!> !#!P!H!I 0 1 0 nil)
      (sst!> !#!P!H!I 0 0 2 nil)
      (sst!> !#!C!H!I 0 1 0 t)
      (sst!> !#!C!H!I 0 0 2 t))))))
      !#!S!P!D!I!U 0)
    (putel1!> (evalform!> (fndfpr!> w (dual!>
                (fndfpr!> '(quotient 1 2) (dfsum!> (list
      (sst!> !#!P!H!I 1 1 0 nil)
      (sst!> !#!P!H!I 1 0 2 nil)
      (sst!> !#!C!H!I 1 1 0 t)
      (sst!> !#!C!H!I 1 0 2 t)
      (sst!> !#!P!H!I 0 1 3 t)
      (sst!> !#!P!H!I 0 0 1 t)
      (sst!> !#!C!H!I 0 1 3 nil)
      (sst!> !#!C!H!I 0 0 1 nil)))))))
      !#!S!P!D!I!U 1)
    (putel1!> (evalform!> (fndfpr!> w (dual!> (dfsum!> (list
      (sst!> !#!P!H!I 1 1 3 t)
      (sst!> !#!P!H!I 1 0 1 t)
      (sst!> !#!C!H!I 1 1 3 nil)
      (sst!> !#!C!H!I 1 0 1 nil))))))
      !#!S!P!D!I!U 2)))

(de sst!> (wss wa wb wt bool)
  (prog (w)
    (setq w
      (fndfpr!> (getel1!> wss wa)
        (fndfpr!> (coalg!>(getel1!> wss wb)) (getframe!> wt))))
    (return (cond (bool w) (t (chsign!> t w))))))

% Dirac energy-momentum tensor ...
(de tdi!> nil
  (prog (w wa)
    (setq !#!T!D!I (mkt!> 2))
    (setq w (mkt!> 1))
    (for!> a (0 1 3) do
      (putel1!>
        (dfsum!> (list
          (chsign!> t (vform!> (getiframe!> a) (car !#!D!A!C!T)))
          (pmf!>(ddss!> !#!P!H!I a t t))
          (pmf!>(coform!> (ddss!> !#!P!H!I (ccin!> a) t t)))
          (pmf!>(ddss!> !#!C!H!I a nil nil))
          (pmf!>(coform!> (ddss!> !#!C!H!I (ccin!> a) nil nil)))))
	w a))
    (cond
     (!*torsion
       (for!> a (0 1 3) do (for!> b (0 1 3) do
         (progn
           (setq wa (dfprod2!> (getlo!> !#!T b) (getel1!> w a)))
           (putel!> (evalalg!> (duald!> (pmf!> wa)))
                    !#!T!D!I (list2 a b))))) )
     (t(for!> a (0 1 3) do (for!> b (0 1 3) do
         (cond ((leq a b) (progn
           (setq wa nil)
           (setq wa (cons (dfprod2!> (getlo!> !#!T b) (getel1!> w a)) wa))
           (setq wa (cons (dfprod2!> (getlo!> !#!T a) (getel1!> w b)) wa))
           (putel!> (evalalg!> (duald!> (fndfpr!> '(quotient -1 2)
                                        (mpf!> (dfsum!> wa)))))
                    !#!T!D!I (list2 a b))))))) ))
    (return t)))

(de ddss!> (wss wa bool wc)
  (prog (w)
    (setq w (cond
      ((eqn wa 0) (fndfpr!> (coalg!>(getel1!> wss 1))
                            (dexcs!> 1 wss nil wc)))
      ((eqn wa 1) (fndfpr!> (coalg!>(getel1!> wss 0))
                            (dexcs!> 0 wss nil wc)))
      ((eqn wa 2) (fndfpr!> (coalg!>(getel1!> wss 0))
                            (dexcs!> 1 wss nil wc)))
      ((eqn wa 3) (fndfpr!> (coalg!>(getel1!> wss 1))
                            (dexcs!> 0 wss nil wc)))))
    (setq w (fndfpr!> (mpa!> '(quotient i (sqrt 2)))
                      (dual!> w)))
    (return (cond (bool w) (t (chsign!> t w))))))

(de ccin!> (w)
  (cond ((eqn w 2) 3)
        ((eqn w 3) 2)
        (t w)))

%---- Total Energy-Momentun and Spin. 10.96 -------------------------------

(de tenmom!> nil
  (prog (w wc wr wss)
    (makebox!> '!#!T!E!N!M!O!M)
    (fordim!> a do (fordim!> b do (cond ((leq a b)
      (setq w ![tlst!])
      (setq wr nil)
      (while!> w
	(setq wc (eval(car w)))
	(setq wss (get (car w) '!=sidxl))
	(cond (wc (setq wr (cons (getelsyc!> wc wss a b) wr))))
	(setq w (cdr w)))
      (cond (wr (putel!> (evalalg!> (cons 'plus wr))
                         !#!T!E!N!M!O!M (list2 a b))))
      ))))))

(de getelsyc!> (w wss wa wb)
  (cond (wss (getel2!> w wa wb))
	(t (list 'times '(quotient 1 2)
             (list 'plus (getel2!> w wa wb)
                         (getel2!> w wb wa))))))

(de spinu!> nil
  (prog (w wc wr)
    (makebox!> '!#!S!P!I!N!U)
    (for!> a (0 1 2) do (progn
      (setq w ![slst!])
      (setq wr nil)
      (while!> w
	(setq wc (eval(car w)))
	(cond (wc (setq wr (cons (getel1!> wc a) wr))))
	(setq w (cdr w)))
      (cond (wr (putel1!> (evalform!> (dfsum!> wr))
                         !#!S!P!I!N!U a)))
      ))))

(de tenmomt!> nil
  (prog (w)
    (fordim!> m do
      (setq w (cons (getm!> '!#!T!E!N!M!O!M nil (list2 m m) '(1 nil)) w)))
    (setq !#!T!E!N!M!O!M!T (ncons (evalalg!> (cond (w (cons 'plus w))
						   (t nil)))))))

(de tenmoms!> nil
  (prog nil
    (makebox!> '!#!T!E!N!M!O!M!S)
    (putel!> (tenmomc!> 1 1) !#!T!E!N!M!O!M!S (list2 0 0))
    (putel!> (tenmomc!> 1 3) !#!T!E!N!M!O!M!S (list2 0 1))
    (putel!> (tenmomc!> 3 3) !#!T!E!N!M!O!M!S (list2 0 2))
    (putel!> (tenmomc!> 0 1) !#!T!E!N!M!O!M!S (list2 1 1))
    (putel!> (tenmomc!> 0 3) !#!T!E!N!M!O!M!S (list2 1 2))
    (putel!> (tenmomc!> 0 0) !#!T!E!N!M!O!M!S (list2 2 2))
    ))

(de tenmomc!> (wa wb)
  (evalalg!> (list 'plus (getel2s!> !#!T!E!N!M!O!M wa wb)
	       (list 'times '(quotient -1 4) (getmetr!> wa wb)
			     (car !#!T!E!N!M!O!M!T)))))


%----- Ideal Fluid. 10.96 -------------------------------------------------

(de tfli!> nil
  (prog (w)
    (setq !#!T!I!F!L (mkt!> 2))
    (fordim!> a do (fordim!> b do (cond ((leq a b)
      (setq w (tfli0!> a b))
      (putel!> (evalalg!> w) !#!T!I!F!L (list2 a b))))))))

(de tfli0!> (a b)
  (list 'plus (list 'times  -1 (car !#!P!R!E!S) (car !#!U!S!Q)
                               (getmetr!> a b))
	      (list 'times (list 'plus (car !#!E!N!E!R) (car !#!P!R!E!S))
			     (getloa!> !#!U!U a) (getloa!> !#!U!U b))))

%----- Spin Fluid. 11.96 --------------------------------------------------

(de spfl!> nil
  (prog (w)
    (fordim!> a do (fordim!> b do (cond ((lessp a b)
      (setq w (cons (fndfpr!> (getel2!> !#!S!P!F!L!T a b)
			      (getel2!> !#!S a b)) w))))))
    (setq !#!S!P!F!L (ncons (evalform!> (dfsum!> w))))))

(de spflt!> nil
  (prog nil
    (setq !#!S!P!F!L!T (mkt!> 2))
    (fordim!> a do (fordim!> b do (cond ((leq a b)
      (putel!> (evalalg!> (vform1!> (getiframe!> b)
			    (vform!> (getiframe!> a)
			      (car !#!S!P!F!L))))
               !#!S!P!F!L!T (list2 a b))))))))

(de frenkel!> nil
  (prog (w)
    (setq w (evalform!> (vform!> (car !#!U!V) (car !#!S!P!F!L))))
    (cond (w (msg!> 6702)))
    (setq !#!F!C!o (ncons (equation!> w nil)))))

(de spflu!> nil
  (prog (w)
    (fordim!> a do
      (setq w (cons (fndfpr!> (getloa!> !#!U!U a) (getframe!> a)) w)))
    (setq w (evalform!> (dual!> (dfsum!> w))))
    (makebox!> '!#!S!P!F!L!U)
    (putel1!> (evalform!>
                (fndfpr!> (getel2!> !#!S!P!F!L!T 1 3) w))
	      !#!S!P!F!L!U 0)
    (putel1!> (evalform!> (fndfpr!> '(quotient 1 2) (fndfpr!>
		(list 'difference (getel2!> !#!S!P!F!L!T 2 3)
				  (getel2!> !#!S!P!F!L!T 0 1)) w)))
	      !#!S!P!F!L!U 1)
    (putel1!> (evalform!>
                (chsign!> t (fndfpr!> (getel2!> !#!S!P!F!L!T 0 2) w)))
	      !#!S!P!F!L!U 2)
    ))

(de tsfluid!> nil
  (progn
    (setq !#!T!S!F!L (mkt!> 2))
    (cond (!*torsion (tsfluidq!>))
	  (t         (tsfluidq0!>)))))

(de tsfluidq0!> nil
  (prog (w)
    (fordim!> a do (fordim!> b do (cond ((leq a b)
      (setq w (list 'plus (tfli0!> a b) (sdeltq0!> a b)))
      (putel!> (evalalg!> w) !#!T!S!F!L (list2 a b))))))))

(de spin3!> (a b c)
  (multa!> (getloa!> !#!U!U a) (getasy2!> !#!S!P!F!L!T b c nil)))

(de spin2!> (b c)
  (getasy2!> !#!S!P!F!L!T b c nil))

(de dspin3!> (a b c d)
  (prog (w)
    (setq w (ncons (chsign!> t (dfun!> (spin3!> b c d)))))
    (fordim!> m do
      (setq w (append w (list
	(fndfpr!> (spin3!> m c d) (getel2!> !#!o!m!e!g!a m b))
	(fndfpr!> (spin3!> b m d) (getel2!> !#!o!m!e!g!a m c))
	(fndfpr!> (spin3!> b c m) (getel2!> !#!o!m!e!g!a m d)) ))))
    (setq w (dfsum!> w))
    (return (evalalg!> (vform1!> (getiframe!> a) w)))))

(de projq0!> (a b)
  (list 'plus (getimetr!> a b)
	      (list 'quotient (multa!> (getel1!> !#!U!U a)
				       (getel1!> !#!U!U b))
			      (car !#!U!S!Q))))

(de sdeltq0!> (a b)
  (prog (w)
    (fordim!> c do (fordim!> d do
      (setq w (cons (list 'times (projq0!> c d)
				 (list 'plus (dspin3!> c a b d)
					     (dspin3!> c b a d))) w))))
    (return (cons 'plus w))))

(de tsfluidq!> nil
  (prog (w)
    (fordim!> a do (fordim!> b do (progn
      (setq w (list 'plus (tfli0!> a b) (sdeltq!> a b)))
      (putel!> (evalalg!> w) !#!T!S!F!L (list2 a b)))))))

(de sdeltq!> (a b)
  (prog (w)
    (fordim!> d do
      (setq w (cons (list 'times
	 (list 'quotient -2 (car !#!U!S!Q))
	 (getloa!> !#!U!U a)
         (getel1!> !#!U!U d)
         (dspin2!> b d)) w)))
    (return (cons 'plus w))))

(de dspin2!> (a b)
  (prog (w)
    (setq w (ncons (chsign!> t (dfun!> (spin2!> a b)))))
    (fordim!> m do
      (setq w (append w (list
	(fndfpr!> (spin2!> m b) (getel2!> !#!o!m!e!g!a m a))
	(fndfpr!> (spin2!> a m) (getel2!> !#!o!m!e!g!a m b)) ))))
    (setq w (dfsum!> w))
    (return (evalalg!> (vform1!> (car !#!U!V) w)))))

%========= End of GRGmater.sl =============================================%

