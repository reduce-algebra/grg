%==========================================================================%
%   GRGgrav.sl                                                Gravitation  %
%==========================================================================%
%   GRG 3.2 Standard Lisp Source Code       (C) 1988-96 Vadim V. Zhytnikov %
%==========================================================================%
% This file is distributed without any warranty. You may modify it but you %
% are not allowed to remove author's name and/or distribute modified file. %
%==========================================================================%

% Various constants of Physics Equations ...

(de aconst!> nil
  (setq !#!A!C!O!N!S!T (copy '( !A!C0 ))))

(de mconst!> nil
  (setq !#!M!C!O!N!S!T (copy '(nil !M!C1 !M!C2 !M!C3 ))))

(de lconst!> nil
  (setq !#!L!C!O!N!S!T
    (copy '( !L!C0 !L!C1 !L!C2 !L!C3 !L!C4 !L!C5 !L!C6 ))))


%---- Irreducible Torsion 2-forms in general case 10.96 -------------------

(de qtfcomp!> nil
  (prog (w)
    (makebox!> '!#!T!H!Q!T)
    (setq w (list 'quotient -1 ![dim1!]))
    (fordim!> a do
      (putel1!> (evalform!> (fndfpr!> w (dfprod2!> (getframe!> a)
						   (car !#!Q!Q))))
		!#!T!H!Q!T a)) ))

(de qafcomp!> nil
  (prog (w)
    (makebox!> '!#!T!H!Q!A)
    (setq w (list 'quotient 1 3))
    (fordim!> a do
      (putel1!> (evalform!> (fndfpr!> w (vform!> (getup!> !#!D a)
						 (car !#!Q!Q!A))))
		!#!T!H!Q!A a)) ))

(de qcfcomp!> nil
  (prog (w)
    (makebox!> '!#!T!H!Q!C)
    (fordim!> a do
      (putel1!> (evalform!> (dfsum!> (list
		  (getel1!> !#!T!H!E!T!A a)
		  (chsign!> t (getel1!> !#!T!H!Q!A a))
		  (chsign!> t (getel1!> !#!T!H!Q!T a)) )))
		!#!T!H!Q!C a)) ))


%----- Irreducible Nonmetricity 1-forms. 10.96 ----------------------------

(de compnnw!> nil
  (prog (w)
    (fordim!> a do
      (setq w (cons (getm!> '!#!N nil (list2 a a) '(1 nil)) w)))
    (setq !#!N!N!W (ncons (evalform!> (dfsum!> w))))))

(de compnnt!> nil
  (prog (w)
    (fordim!> a do (fordim!> m do
      (setq w (cons (fndfpr!> (vform1!> (getup!> !#!D m)
					(getel2s!> !#!N a m))
			      (getframe!> a)) w))))
    (setq w (cons (fndfpr!> (list 'quotient -1 ![dim!])
			    (car !#!N!N!W)) w))
    (setq !#!N!N!T (ncons (evalform!> (dfsum!> w))))))

(de compnw!> nil
  (prog (w)
    (setq !#!N!W (mkt!> 2))
    (setq w (list 'quotient 1 ![dim!]))
    (fordim!> a do (fordim!> b do (cond ((leq a b)
      (putel!> (evalform!> (fndfpr!> (list 'times w (getmetr!> a b))
				     (car !#!N!N!W)))
	       !#!N!W (list2 a b)))))) ))

(de compnt!> nil
  (prog (w ww)
    (setq !#!N!T (mkt!> 2))
    (setq w (list 'quotient ![dim!] (times (sub1 ![dim!])
                                           (add1 (add1 ![dim!])))))
    (setq ww (list 'quotient -2 ![dim!]))
    (fordim!> a do (fordim!> b do (cond ((leq a b)
      (putel!> (evalform!> (fndfpr!> w (dfsum!> (list
		  (fndfpr!> (vform1!> (getiframe!> a) (car !#!N!N!T))
                            (getlo!> !#!T b))
		  (fndfpr!> (vform1!> (getiframe!> b) (car !#!N!N!T))
                            (getlo!> !#!T a))
		  (fndfpr!> (list 'times ww (getmetr!> a b))
			    (car !#!N!N!T))))))
	       !#!N!T (list2 a b)))))) ))

(de compna!> nil
  (prog (w wa)
    (setq !#!N!A (mkt!> 2))
    (setq wa (mkt!> 1))
    (fordim!> a do (progn
      (setq w nil)
      (fordim!> m do
        (setq w (cons (dfprod2!> (getframe!> m)
				 (dfsum!> (list
				   (getel2s!> !#!N a m)
				   (chsign!> t (getel2s!> !#!N!W a m))
				   (chsign!> t (getel2s!> !#!N!T a m)))))
		       w)))
      (putel1!> (dfsum!> w) wa a)))
    (fordim!> a do (fordim!> b do (cond ((leq a b)
      (putel!> (evalform!> (fndfpr!> (list 'quotient 1 3)
		 (dfsum!> (list (vform!> (getiframe!> a) (getel1!> wa b))
				(vform!> (getiframe!> b) (getel1!> wa a))))))
	       !#!N!A (list2 a b)))))) ))

(de compnc!> nil
  (prog (w)
    (setq !#!N!C (mkt!> 2))
    (setq w (list 'quotient 1 ![dim!]))
    (fordim!> a do (fordim!> b do (cond ((leq a b)
      (putel!> (evalform!> (dfsum!> (list
		  (getel2s!> !#!N a b)
		  (cond ((geq ![dim!] 3)
                              (chsign!> t (getel2s!> !#!N!A a b)) )
			(t nil))
		  (chsign!> t (getel2s!> !#!N!W a b))
		  (chsign!> t (getel2s!> !#!N!T a b)) )))
	       !#!N!C (list2 a b)))))) ))

%----- Irreducible Curvature 2-forms. 10.96 -------------------------------

% OMEGA[.a.b]
(de getoma!> (wa wb)
  (cond (!*nonmetr (fndfpr!> '(quotient 1 2) (dfsum!> (list2
	   (getm!> '!#!O!M!E!G!A nil (list2 wa wb) '(2 nil))
	   (chsign!> t
	      (getm!> '!#!O!M!E!G!A nil (list2 wb wa) '(2 nil))) ))))
	(t (getm!> '!#!O!M!E!G!A nil (list2 wa wb) '(2 nil)))))

% OMEGA(.a.b)
(de getoms!> (wa wb)
  (cond (!*nonmetr (fndfpr!> '(quotient 1 2) (dfsum!> (list2
	   (getm!> '!#!O!M!E!G!A nil (list2 wa wb) '(2 nil))
	   (getm!> '!#!O!M!E!G!A nil (list2 wb wa) '(2 nil)) ))))
	(t nil)))

(de getomao!> (wa wb)
  (dfsum!> (list  (getoma!> wa wb)
		  (chsign!> t (getasy2!> !#!O!M!C wa wb t))
		  (chsign!> t (getasy2!> !#!O!M!R wa wb t))
		  (chsign!> t (getasy2!> !#!O!M!A wa wb t))
		  (chsign!> t (getasy2!> !#!O!M!D wa wb t)) )))

(de getomso!> (wa wb)
  (dfsum!> (list  (getoms!> wa wb)
		  (chsign!> t (getel2s!> !#!O!S!H wa wb))
		  (chsign!> t (getel2s!> !#!O!S!C wa wb))
		  (chsign!> t (getel2s!> !#!O!S!A wa wb)) )))

% Ricci Tensor ...
(de riccio!> nil
  (prog (w woo)
    (setq !#!R!I!C (mkt!> 2))
    (setq woo (mkt!> 1))
    (fordim!> b do (progn
      (setq w nil)
      (fordim!> m do
	(setq w (cons (vform!> (getiframe!> m)
			       (getel2!> !#!O!M!E!G!A m b)) w)))
      (putel1!> (dfsum!> w) woo b)))
    (fordim!> a do (fordim!> b do
      (cond ((or !*torsion !*nonmetr (leq a b))
	(putel!> (evalalg!> (vform1!> (getiframe!> b) (getel1!> woo a)))
                 !#!R!I!C (list2 a b))))))))

% A-Ricci Tensor ...
(de riccioa!> nil
  (prog (w woo)
    (setq !#!R!I!C!A (mkt!> 2))
    (setq woo (mkt!> 1))
    (fordim!> b do (progn
      (setq w nil)
      (fordim!> m do
	(setq w (cons (vform!> (getup!> !#!D m) (getoma!> m b)) w)))
      (putel1!> (dfsum!> w) woo b)))
    (fordim!> a do (fordim!> b do
	(putel!> (evalalg!> (vform1!> (getiframe!> b) (getel1!> woo a)))
                 !#!R!I!C!A (list2 a b))))))

% S-Ricci Tensor ...
(de riccios!> nil
  (prog (w woo)
    (setq !#!R!I!C!S (mkt!> 2))
    (setq woo (mkt!> 1))
    (fordim!> b do (progn
      (setq w nil)
      (fordim!> m do
	(setq w (cons (vform!> (getup!> !#!D m) (getoms!> m b)) w)))
      (putel1!> (dfsum!> w) woo b)))
    (fordim!> a do (fordim!> b do
	(putel!> (evalalg!> (vform1!> (getiframe!> b) (getel1!> woo a)))
                 !#!R!I!C!S (list2 a b))))))

% RR from ARIC
(de rscalara!> nil
  (prog (w)
    (fordim!> wa do (fordim!> wb do
      (setq w (cons (multa!> (getimetr!> wa wb)
			     (getel2!> !#!R!I!C!A wa wb))
		    w))))
      (setq w (summa!> w))
      (setq !#!R!R (ncons w)) ))

(de mkrrf!> nil
  (prog (wc)
    (setq !#!O!M!R (mkt!> 2))
    (setq wc (list 'quotient 1 (times ![dim!] (sub1 ![dim!]))))
    (fordim!> a do (fordim!> b do (cond ((lessp a b)
      (putel!> (evalform!> (fndfpr!> (list 'times wc (car !#!R!R))
			     (getm!> '!#!S nil (list2 a b) '(2 2))))
	       !#!O!M!R (list2 a b))))))))

(de getra!> (wa wb)
  (cond (!*nonmetr (list 'times '(quotient 1 2)
		     (list 'difference (getel2!> !#!R!I!C!A wa wb)
				       (getel2!> !#!R!I!C!A wb wa))))
        (t         (list 'times '(quotient 1 2)
		     (list 'difference (getel2!> !#!R!I!C wa wb)
				       (getel2!> !#!R!I!C wb wa))))  ))

(de getrsa!> (wa wb)
  (list 'difference
    (list 'times '(quotient 1 2)
      (list 'difference (getel2!> !#!R!I!C!S wa wb)
		        (getel2!> !#!R!I!C!S wb wa)))
    (list 'times (list 'quotient 1 ![dim!])
		 (vform1!> (getiframe!> wb)
		   (vform!> (getiframe!> wa)
		     (car !#!O!M!E!G!A!H))))))

%(de getrsa!> (wa wb)
%  (list 'times '(quotient 1 2)
%    (list 'difference (getel2!> !#!R!I!C!S wa wb)
%		      (getel2!> !#!R!I!C!S wb wa))))

(de getrsc!> (wa wb)
  (list 'times '(quotient 1 2)
    (list 'plus (getel2!> !#!R!I!C!S wa wb)
	        (getel2!> !#!R!I!C!S wb wa))))

(de getrc!> (wa wb)
  (cond (!*nonmetr (list 'times '(quotient 1 2)
		     (list 'plus (getel2!> !#!R!I!C!A wa wb)
				 (getel2!> !#!R!I!C!A wb wa)
				 (list 'times (list 'quotient -2 ![dim!])
					      (getmetr!> wa wb)
					      (car !#!R!R)))))
        (!*torsion (list 'times '(quotient 1 2)
		     (list 'plus (getel2!> !#!R!I!C wa wb)
				 (getel2!> !#!R!I!C wb wa)
				 (list 'times (list 'quotient -2 ![dim!])
					      (getmetr!> wa wb)
					      (car !#!R!R)))))
        (t         (list 'plus (getel2s!> !#!R!I!C wa wb)
			       (list 'times (list 'quotient -1 ![dim!])
					    (getmetr!> wa wb)
					    (car !#!R!R))))))

(de mkrcf!> nil
  (prog (wc wx w)
    (setq !#!O!M!C (mkt!> 2))
    (setq wx (mkt!> 1))
    (fordim!> a do (progn
      (setq w nil)
      (fordim!> m do
	(setq w (cons (fndfpr!> (getrc!> a m) (getframe!> m)) w)))
      (putel1!> (dfsum!> w) wx a)))
    (setq wc (list 'quotient 1 (sub1(sub1 ![dim!]))))
    (fordim!> a do (fordim!> b do (cond ((lessp a b)
      (putel!> (evalform!> (fndfpr!> wc (dfsum!> (list
		  (dfprod2!> (getel1!> wx a) (getlo!> !#!T b))
		  (chsign!> t
		    (dfprod2!> (getel1!> wx b) (getlo!> !#!T a)))))))
	       !#!O!M!C (list2 a b))))))))

(de mkraf!> nil
  (prog (wc wx w)
    (setq !#!O!M!A (mkt!> 2))
    (setq wx (mkt!> 1))
    (fordim!> a do (progn
      (setq w nil)
      (fordim!> m do
	(setq w (cons (fndfpr!> (getra!> a m) (getframe!> m)) w)))
      (putel1!> (dfsum!> w) wx a)))
    (setq wc (list 'quotient 1 (sub1(sub1 ![dim!]))))
    (fordim!> a do (fordim!> b do (cond ((lessp a b)
      (putel!> (evalform!> (fndfpr!> wc (dfsum!> (list
		  (dfprod2!> (getel1!> wx a) (getlo!> !#!T b))
		  (chsign!> t
		    (dfprod2!> (getel1!> wx b) (getlo!> !#!T a)))))))
	       !#!O!M!A (list2 a b))))))))

(de mkrdf!> nil
  (prog (wc w)
    (setq !#!O!M!D (mkt!> 2))
    (fordim!> m do (fordim!> n do (cond ((lessp m n)
      (setq w (cons (dfprod2!> (getoma!> m n) (getel2!> !#!S m n)) w))))))
    (setq w (evalform!>(dfsum!> w)))
    (setq wc (list 'quotient 1 6))
    (fordim!> a do (fordim!> b do (cond ((lessp a b)
      (putel!> (evalform!> (fndfpr!> wc
		 (vform!> (getiframe!> b) (vform!> (getiframe!> a) w))))
	       !#!O!M!D (list2 a b))))))))

(de mkrbf!> nil
  (prog (wc wx w)
    (setq !#!O!M!B (mkt!> 2))
    (setq wx (mkt!> 1))
    (fordim!> a do (progn
      (setq w nil)
      (fordim!> m do
	(setq w (cons (dfprod2!> (getomao!> a m) (getframe!> m)) w)))
      (putel1!> (dfsum!> w) wx a)))
    (setq wc (list 'quotient 1 2))
    (fordim!> a do (fordim!> b do (cond ((lessp a b)
      (putel!> (evalform!> (fndfpr!> wc (dfsum!> (list
		  (vform!> (getiframe!> b) (getel1!> wx a))
		  (chsign!> t (vform!> (getiframe!> a) (getel1!> wx b)))))))
	       !#!O!M!B (list2 a b))))))))

(de mkrwf!> nil
  (prog nil
    (setq !#!O!M!W (mkt!> 2))
    (fordim!> a do (fordim!> b do (cond ((lessp a b)
      (putel!> (evalform!> (dfsum!> (list
		 (getoma!> a b)
		 (chsign!> t (getel2!> !#!O!M!C a b))
		 (chsign!> t (getel2!> !#!O!M!R a b))
		 (cond ((or !*torsion !*nonmetr)
                         (chsign!> t (getel2!> !#!O!M!A a b))) (t nil))
		 (cond ((or !*torsion !*nonmetr)
                         (chsign!> t (getel2!> !#!O!M!B a b))) (t nil))
		 (cond ((or !*torsion !*nonmetr)
                         (chsign!> t (getel2!> !#!O!M!D a b))) (t nil))
		 )))
	       !#!O!M!W (list2 a b))))))))

(de mkomegah!> nil
  (prog (w)
    (fordim!> m do
      (setq w (cons (getel2!> !#!O!M!E!G!A m m) w)))
    (setq !#!O!M!E!G!A!H (ncons (evalform!> (dfsum!> w))))))

(de mkrshf!> nil
  (prog (wc wcc w)
    (setq !#!O!S!H (mkt!> 2))
    (setq wc (list 'quotient 1 ![dim!]))
    (fordim!> a do (fordim!> b do (cond ((leq a b)
      (putel!> (evalform!> (fndfpr!> (list 'times wc (getmetr!> a b))
				     (car !#!O!M!E!G!A!H)))
	       !#!O!S!H (list2 a b))))))))

%(de mkrshf!> nil
%  (prog (wc wcc w)
%    (setq !#!O!S!H (mkt!> 2))
%    (setq wc (list 'quotient -1 (difference (expt ![dim!] 2) 4)))
%    (setq wcc (minus ![dim!]))
%    (fordim!> a do (fordim!> b do (cond ((leq a b)
%      (putel!> (evalform!> (fndfpr!> wc (dfsum!> (list
%		 (dfprod2!> (getlo!> !#!T a)
%                            (vform!> (getiframe!> b) (car !#!O!M!E!G!A!H)))
%		 (dfprod2!> (getlo!> !#!T b)
%                            (vform!> (getiframe!> a) (car !#!O!M!E!G!A!H)))
%		 (fndfpr!> (list 'times wcc (getmetr!> a b))
%			   (car !#!O!M!E!G!A!H)  )))))
%	       !#!O!S!H (list2 a b))))))))

(de mkrscf!> nil
  (prog (wc wx w)
    (setq !#!O!S!C (mkt!> 2))
    (setq wx (mkt!> 1))
    (fordim!> a do (progn
      (setq w nil)
      (fordim!> m do
	(setq w (cons (fndfpr!> (getrsc!> a m) (getframe!> m)) w)))
      (putel1!> (dfsum!> w) wx a)))
    (setq wc (list 'quotient 1 ![dim!]))
    (fordim!> a do (fordim!> b do (cond ((leq a b)
      (putel!> (evalform!> (fndfpr!> wc (dfsum!> (list
		  (dfprod2!> (getlo!> !#!T a) (getel1!> wx b))
		  (dfprod2!> (getlo!> !#!T b) (getel1!> wx a))))))
	       !#!O!S!C (list2 a b))))))))

(de mkrshf2!> nil
  (prog (wc wx w)
    (setq !#!O!S!H (mkt!> 2))
    (fordim!> a do (fordim!> b do (cond ((leq a b)
      (putel!> (evalform!> (dfsum!> (list
		  (getoms!> a b)
		  (chsign!> t (getel2!> !#!O!S!C a b)))))
	       !#!O!S!H (list2 a b))))))))

(de mkrsaf!> nil
  (prog (wc wx wxx wcc w)
    (setq !#!O!S!A (mkt!> 2))
    (setq wx (mkt!> 1))
    (fordim!> a do (progn
      (setq w nil)
      (fordim!> m do
	(setq w (cons (fndfpr!> (getrsa!> a m) (getframe!> m)) w)))
      (putel1!> (dfsum!> w) wx a)))
    (setq w nil)
    (fordim!> m do
      (setq w (cons (dfprod2!> (getframe!> m) (getel1!> wx m)) w)))
    (setq wxx (dfsum!> w))
    (setq w nil)
    (setq wc (list 'quotient 1 ![dim!]))
    (setq wc (list 'quotient ![dim!] (difference (expt ![dim!] 2) 4)))
    (setq wcc (list 'quotient -2 ![dim!]))
    (fordim!> a do (fordim!> b do (cond ((leq a b)
      (putel!> (evalform!> (fndfpr!> wc (dfsum!> (list
		  (dfprod2!> (getlo!> !#!T a) (getel1!> wx b))
		  (dfprod2!> (getlo!> !#!T b) (getel1!> wx a))
		  (fndfpr!> (list 'times wcc (getmetr!> a b)) wxx)
                  ))))
	       !#!O!S!A (list2 a b))))))))

(de mkrsvf!> nil
  (prog (wc wx w)
    (setq !#!O!S!V (mkt!> 2))
    (setq wx (mkt!> 1))
    (fordim!> a do (progn
      (setq w nil)
      (fordim!> m do
	(setq w (cons (dfprod2!> (getomso!> a m) (getframe!> m)) w)))
      (putel1!> (dfsum!> w) wx a)))
    (setq wc (list 'quotient 1 4))
    (fordim!> a do (fordim!> b do (cond ((leq a b)
      (putel!> (evalform!> (fndfpr!> wc (dfsum!> (list
		  (vform!> (getiframe!> b) (getel1!> wx a))
		  (vform!> (getiframe!> a) (getel1!> wx b))))))
	       !#!O!S!V (list2 a b))))))))

(de mkrsuf!> nil
  (prog nil
    (setq !#!O!S!U (mkt!> 2))
    (fordim!> a do (fordim!> b do (cond ((leq a b)
      (putel!> (evalform!> (dfsum!> (list
		 (getoms!> a b)
		 (chsign!> t (getel2!> !#!O!S!H a b))
		 (chsign!> t (getel2!> !#!O!S!A a b))
		 (chsign!> t (getel2!> !#!O!S!C a b))
		 (cond
                   ((geq ![dim!] 4) (chsign!> t (getel2!> !#!O!S!V a b)))
		   (t nil))
		 )))
	       !#!O!S!U (list2 a b))))))))

%------- Einstein Equations. 10.96 ----------------------------------------

(de einstein!> nil
  (prog (wl wr)
    (setq !#!E!E!q (mkt!> 2))
    (fordim!> wa do (fordim!> wb do (cond ((leq wa wb)
      (setq wl (list  (getel2!> !#!R!I!C wa wb)
		      (list 'times '(quotient -1 2) (getmetr!> wa wb)
				    (car !#!R!R))
		      (cond (!*cconst
		        (list 'times (getmetr!> wa wb) '!C!C!O!N!S!T)))))
      (setq wr (list 'times 8 'pi '!G!C!O!N!S!T
                            (getel2!> !#!T!E!N!M!O!M wa wb)))
      (putel!> (equation!> (summa!> wl) (evalalg!> wr))
               !#!E!E!q (list2 wa wb))))))))

(de einsteint!> nil
  (setq !#!T!E!E!q (ncons (equation!>
    (evalalg!> (cond (!*cconst (list 'plus (car !#!R!R)
					   (list 'times -4 '!C!C!O!N!S!T)))
		     (t (car !#!R!R))))
    (evalalg!> (list 'times -8 'pi '!G!C!O!N!S!T
			    (car !#!T!E!N!M!O!M!T)))))))

(de einsteinc!> nil
  (prog (wl wr)
    (makebox!> '!#!C!E!E!q)
    (for!> wa (0 1 2) do (for!> wb (0 1 2) do (cond ((leq wa wb)
      (setq wl (getel2!> !#!R!C wa wb))
      (setq wr (list 'times 8 'pi '!G!C!O!N!S!T
                            (getel2!> !#!T!E!N!M!O!M!S wa wb)))
      (putel!> (equation!> (evalalg!> wl) (evalalg!> wr))
               !#!C!E!E!q (list2 wa wb))))))))

%------ Gravitational Equations -------------------------------------------

% Curvature Momentum ...
(de pomegau!> nil
  (prog (wc objlst finlst w w0 w1 w2 obj)
    % we are trying to calculate required parts ...
    (setq wc 0)
    (setq objlst (cond
      (!*torsion '( !#!O!M!W!U !#!O!M!C!U !#!O!M!R!U
                    !#!O!M!A!U !#!O!M!B!U !#!O!M!D!U ))
      (t         '( !#!O!M!W!U !#!O!M!C!U !#!O!M!R!U ))))
    (foreach!> obj in objlst do (progn
      (setq wc (add1 wc))
      (cond
        ((evalalg!> (getel1!> !#!L!C!O!N!S!T wc))
          (setq finlst (cons (cons wc obj) finlst))
          (setq ![chain!] nil)
          (setq w (request!> obj))
          (cond ((eq w !!er!!) (setq finlst (cons !!er!! finlst))
                               %(return !!er!!)
                               )
                ((null w)      (setq ![er!] 6046)
                               (setq finlst (cons !!er!! finlst))
                               (trsf!> obj)
                               %(return !!er!!)
                               )  )))))
%    (foreach!> obj in objlst do (progn
%      (setq wc (add1 wc))
%      (cond
%        ((evalalg!> (getel1!> !#!L!C!O!N!S!T wc))
%          (setq finlst (cons (cons wc obj) finlst))
%          (setq ![chain!] nil)
%          (setq w (request!> obj))
%          (cond ((eq w !!er!!) (setq finlst (cons !!er!! finlst))
%                               (return !!er!!)  )
%                ((null w)      (setq ![er!] 6046)
%                               (setq finlst (cons !!er!! finlst))
%                               (trsf!> obj)
%                               (return !!er!!)  )  )))))
    (cond ((memq !!er!! finlst) (return !!er!!)))
    % now we go on ...
    (makebox!> '!#!P!O!M!E!G!A!U)
    (foreach!> obj in finlst do (progn
      (setq wc (cond ((memq (car obj) '(1 3 4 6)) 'i)
		     (t '(minus i))))
      (setq w0 (cons (fndfpr!>
                 (list 'times wc (getel1!> !#!L!C!O!N!S!T  (car obj)))
		 (getel1!> (eval(cdr obj)) 0)) w0))
      (setq w1 (cons (fndfpr!>
                 (list 'times wc (getel1!> !#!L!C!O!N!S!T  (car obj)))
		 (getel1!> (eval(cdr obj)) 1)) w1))
      (setq w2 (cons (fndfpr!>
                 (list 'times wc (getel1!> !#!L!C!O!N!S!T  (car obj)))
		 (getel1!> (eval(cdr obj)) 2)) w2))
      ))
    (setq wc (list 'times 'i
	       (list 'plus (getel1!> !#!L!C!O!N!S!T 0)
			   (cond (!*nonmin (list 'times
                                     (mp!> 8) 'pi
				     '!G!C!O!N!S!T
				     (getel1!> !#!A!C!O!N!S!T 0)
				     (car !#!F!I) (car !#!F!I)
				     ))))))
    (setq w0 (cons (fndfpr!> wc (getel1!> !#!S!U 0)) w0))
    (setq w1 (cons (fndfpr!> wc (getel1!> !#!S!U 1)) w1))
    (setq w2 (cons (fndfpr!> wc (getel1!> !#!S!U 2)) w2))
    (putel1!> (evalform!>(dfsum!> w0)) !#!P!O!M!E!G!A!U 0) (setq w0 nil)
    (putel1!> (evalform!>(dfsum!> w1)) !#!P!O!M!E!G!A!U 1) (setq w1 nil)
    (putel1!> (evalform!>(dfsum!> w2)) !#!P!O!M!E!G!A!U 2) (setq w2 nil)
    (return t)))

% Torsion Momentum ...
(de ptheta!> nil
  (prog (wc objlst finlst w w0 w1 w2 w3)
    % we are trying to calculate required parts ...
    (setq wc 0)
    (setq objlst '( !#!T!H!Q!C!U !#!T!H!Q!T!U !#!T!H!Q!A!U ))
    (foreach!> obj in objlst do (progn
      (setq wc (add1 wc))
      (cond
        ((evalalg!> (getel1!> !#!M!C!O!N!S!T wc))
          (setq finlst (cons (cons wc obj) finlst))
          (setq ![chain!] nil)
          (setq w (request!> obj))
          (cond ((eq w !!er!!) (setq finlst (cons !!er!! finlst))
                               %(return !!er!!)
                               )
                ((null w)      (setq ![er!] 6046)
                               (setq finlst (cons !!er!! finlst))
                               (trsf!> obj)
                               %(return !!er!!)
                               ) )))))
%    (foreach!> obj in objlst do (progn
%      (setq wc (add1 wc))
%      (cond
%        ((evalalg!> (getel1!> !#!M!C!O!N!S!T wc))
%          (setq finlst (cons (cons wc obj) finlst))
%          (setq ![chain!] nil)
%          (setq w (request!> obj))
%          (cond ((eq w !!er!!) (setq finlst (cons !!er!! finlst))
%                               (return !!er!!))
%                ((null w)      (setq ![er!] 6046)
%                               (setq finlst (cons !!er!! finlst))
%                               (trsf!> obj)
%                               (return !!er!!)) )))))
    (cond ((memq !!er!! finlst) (return !!er!!)))
    % now we go on ...
    (makebox!> '!#!P!T!H!E!T!A)
    (foreach!> obj in finlst do (progn
      (setq wc 'i)
      (setq w0 (cons (fndfpr!>
                 (list 'times wc (getel1!> !#!M!C!O!N!S!T  (car obj)))
		 (getel1!> (eval(cdr obj)) 0)) w0))
      (setq w1 (cons (fndfpr!>
                 (list 'times wc (getel1!> !#!M!C!O!N!S!T  (car obj)))
		 (getel1!> (eval(cdr obj)) 1)) w1))
      (setq w2 (cons (fndfpr!>
                 (list 'times wc (getel1!> !#!M!C!O!N!S!T  (car obj)))
		 (getel1!> (eval(cdr obj)) 2)) w2))
      (setq w3 (cons (fndfpr!>
                 (list 'times wc (getel1!> !#!M!C!O!N!S!T  (car obj)))
		 (getel1!> (eval(cdr obj)) 3)) w3))
      ))

    (setq w0 (ncons (evalform!> (dfsum!> w0))))
    (setq w1 (ncons (evalform!> (dfsum!> w1))))
    (setq w2 (ncons (evalform!> (dfsum!> w2))))
    (setq w3 (ncons (evalform!> (dfsum!> w3))))

    (setq w0 (append w0 (mapcar w0 'coform!>)))
    (setq w1 (append w1 (mapcar w1 'coform!>)))
    (setq w2 (append w2 (mapcar w3 'coform!>)))
    (setq w3 (mapcar w2 'coform!>))
    (putel1!> (evalform!>(dfsum!> w0)) !#!P!T!H!E!T!A 0) (setq w0 nil)
    (putel1!> (evalform!>(dfsum!> w1)) !#!P!T!H!E!T!A 1) (setq w1 nil)
    (putel1!> (evalform!>(dfsum!> w2)) !#!P!T!H!E!T!A 2) (setq w2 nil)
    (putel1!> (evalform!>(dfsum!> w3)) !#!P!T!H!E!T!A 3) (setq w3 nil)
    (return t)))

%----- Gravitational action 4-form. 12.90 ---------------------------------

(de lact!> nil
  (prog (w)
    (setq w (list
      (dfprod2!> (getel1!> !#!P!O!M!E!G!A!U 0)
                 (getel1!> !#!O!M!E!G!A!U   2))
      (dfprod2!> (getel1!> !#!P!O!M!E!G!A!U 2)
                 (getel1!> !#!O!M!E!G!A!U   0))
      (fndfpr!> -2 (dfprod2!> (getel1!> !#!P!O!M!E!G!A!U 1)
                              (getel1!> !#!O!M!E!G!A!U  1)))
      ))

    (setq w (ncons (evalform!> (dfsum!> w))))

    (setq w (append w (mapcar w 'coform!>)))
    (cond (!*cconst
      (setq w (cons
        (fndfpr!> (list 'times -2 '!C!C!O!N!S!T) (car !#!V!O!L)) w))))
    (cond (!*torsion (setq w (append w (list
          (fndfpr!> (list 'quotient (mp!> 1) 2)
                     (dfprod2!> (getel1!> !#!P!T!H!E!T!A 0)
                                (getel1!> !#!T!H!E!T!A 1)))
          (fndfpr!> (list 'quotient (mp!> 1) 2)
                     (dfprod2!> (getel1!> !#!P!T!H!E!T!A 1)
                                (getel1!> !#!T!H!E!T!A 0)))
          (fndfpr!> (list 'quotient (pm!> 1) 2)
                     (dfprod2!> (getel1!> !#!P!T!H!E!T!A 2)
                                (getel1!> !#!T!H!E!T!A 3)))
          (fndfpr!> (list 'quotient (pm!> 1) 2)
                     (dfprod2!> (getel1!> !#!P!T!H!E!T!A 3)
                                (getel1!> !#!T!H!E!T!A 2)))
         )))))
    (setq w (cons
      (fndfpr!> (list 'plus
		  (list 'quotient (getel1!> !#!L!C!O!N!S!T 0) 2)
		  (cond (!*nonmin
		    (list 'times (mp!> 4) 'pi '!G!C!O!N!S!T
			         (getel1!> !#!A!C!O!N!S!T 0)
			         (car !#!F!I) (car !#!F!I)))
		    (t nil)))
	(fndfpr!> (car !#!R!R) (car !#!V!O!L))) w))
    (setq !#!L!A!C!T (ncons (evalform!> (dfsum!> w))))
    (return t)))


% Torsion equation. 01.91
(de torsequation!> nil
  (prog (wc)
    (setq wc '(times -16 pi !G!C!O!N!S!T))
    (makebox!> '!#!T!O!R!S!q)
    (putel1!> (equation!> (evalform!> (chsign!> t (dfsum!> (list
      (dex!> (getel1!> !#!P!O!M!E!G!A!U 0 ))
      (fndfpr!> -2 (dfprod2!> (connecu!> 1)
                              (getel1!> !#!P!O!M!E!G!A!U 0 )))
      (fndfpr!>  2 (dfprod2!> (connecu!> 0)
                              (getel1!> !#!P!O!M!E!G!A!U 1 )))
      (fndfpr!> '(quotient -1 2) (dfprod2!> (getframe!> 0)
                                            (getel1!> !#!P!T!H!E!T!A 2)))
      (fndfpr!> '(quotient  1 2) (dfprod2!> (getframe!> 2)
                                            (getel1!> !#!P!T!H!E!T!A 0)))
      ))))
      (evalform!> (fndfpr!> wc (getel1!> !#!S!P!I!N!U 0))))
      !#!T!O!R!S!q 0)
    (putel1!> (equation!> (evalform!> (chsign!> t (dfsum!> (list
      (dex!> (getel1!> !#!P!O!M!E!G!A!U 1 ))
      (fndfpr!> -1 (dfprod2!> (connecu!> 2)
                              (getel1!> !#!P!O!M!E!G!A!U 0 )))
      (dfprod2!> (connecu!> 0)
                 (getel1!> !#!P!O!M!E!G!A!U 2 ))
      (fndfpr!> '(quotient -1 4) (dfprod2!> (getframe!> 1)
                                            (getel1!> !#!P!T!H!E!T!A 0)))
      (fndfpr!> '(quotient  1 4) (dfprod2!> (getframe!> 0)
                                            (getel1!> !#!P!T!H!E!T!A 1)))
      (fndfpr!> '(quotient  1 4) (dfprod2!> (getframe!> 3)
                                            (getel1!> !#!P!T!H!E!T!A 2)))
      (fndfpr!> '(quotient -1 4) (dfprod2!> (getframe!> 2)
                                            (getel1!> !#!P!T!H!E!T!A 3)))
      ))))
      (evalform!> (fndfpr!> wc (getel1!> !#!S!P!I!N!U 1))))
      !#!T!O!R!S!q 1)
    (putel1!> (equation!> (evalform!> (chsign!> t (dfsum!>( list
      (dex!> (getel1!> !#!P!O!M!E!G!A!U 2 ))
      (fndfpr!>  2 (dfprod2!> (connecu!> 1)
                              (getel1!> !#!P!O!M!E!G!A!U 2 )))
      (fndfpr!> -2 (dfprod2!> (connecu!> 2)
                              (getel1!> !#!P!O!M!E!G!A!U 1 )))
      (fndfpr!> '(quotient  1 2) (dfprod2!> (getframe!> 1)
                                            (getel1!> !#!P!T!H!E!T!A 3)))
      (fndfpr!> '(quotient -1 2) (dfprod2!> (getframe!> 3)
                                            (getel1!> !#!P!T!H!E!T!A 1)))
      ))))
      (evalform!> (fndfpr!> wc (getel1!> !#!S!P!I!N!U 2))))
      !#!T!O!R!S!q 2)
    ))

(de connecu!> (w)
  (pmf!> (getel1!> !#!o!m!e!g!a!u w)))


% Metric Equation. 01.91
(de metrequation!> nil
  (prog (wc woo wcc wtt wtheta wa wb)
    (setq wc '(times 8 pi !G!C!O!N!S!T))
    (setq woo (mkt!> 1))
    % OMEGAU/\POMEGAU
    (for!> x (0 1 3) do
      (putel1!> (evalform!>(dfsum!>(list
        (fndfpr!> 2 (dfprod2!> (vform!> (getiframe!> x)
                                         (getel1!> !#!O!M!E!G!A!U 0 ))
                                (getel1!> !#!P!O!M!E!G!A!U 2 )))
        (fndfpr!> 2 (dfprod2!> (vform!> (getiframe!>  x)
                                         (getel1!> !#!O!M!E!G!A!U 2 ))
                                (getel1!> !#!P!O!M!E!G!A!U 0 )))
        (fndfpr!> -4 (dfprod2!> (vform!> (getiframe!>  x)
                                        (getel1!> !#!O!M!E!G!A!U 1 ))
                                (getel1!> !#!P!O!M!E!G!A!U 1 ))) )))
        woo  x))
    (setq wcc (mkt!> 1))
    % OMEGAU/\POMEGAU + cc
    (for!> x (0 1 3) do
      (putel1!> (list2 (getel1!> woo x)
                       (coform!> (getel1!> woo (ccin!> x))))
                wcc x))
    (setq woo nil)
    (setq wtt (mkt!> 1))
    % Effective PTHETA
    (cond
      % If TORSION is On then    wtheta = PTHETA
      (!*torsion (setq wtheta !#!P!T!H!E!T!A))
      % If TORSION is Off then   wtheta = D POMEGA
      (t (setq wa (mkt!> 1))
         (dcpomega!> wa) % wa - D POMEGA
         (setq wb (mkt!> 1))
         (crsigma!> wb wa) % wb - SIGMAi
         (setq wa
           (list
             (vform!> (getiframe!> 2) (getel1!> wb 2))
             (vform!> (getiframe!> 0) (getel1!> wb 0))
             (vform!> (getiframe!> 1) (getel1!> wb 1)) ))
         (setq wa (cons (coform!> (car wa)) wa))
         (setq wa (dfsum!> wa)) % wa - SIGMA
         (setq wtheta (mkt!> 1))
         (for!> x (0 1 2) do
         (putel1!> (evalform!> (dfsum!> (list
             (fndfpr!> 2 (getel1!> wb x))
             (fndfpr!> '(quotient -1 2) (dfprod2!> (getframe!> x) wa)) )))
           wtheta x))    % wtheta - THETAeff
	 (putel1!> (coform!>(getel1!> wtheta 2)) wtheta 3)
	 (setq wa nil)
	 (setq wb nil)
	))
    (for!> x (0 1 3) do (putel1!> (evalform!> (dfsum!> (append
        (cons (dctheta!> x wtheta) (getel1!> wcc x) )  % D PTHETA
        (list
          (chsign!> t (vform!> (getiframe!> x)         % LACT
                               (car !#!L!A!C!T)))
	  % THETA/\PTHETA iff TORSION is On
          (cond (!*torsion (dfprod2!> (vform!> (getdsgn!>  x)
                                               (getel1!> !#!T!H!E!T!A 0))
                                      (getel1!> !#!P!T!H!E!T!A 1))))
          (cond (!*torsion (dfprod2!> (vform!> (getdsgn!>  x)
                                      (getel1!> !#!T!H!E!T!A 1))
                           (getel1!> !#!P!T!H!E!T!A 0))))
          (cond (!*torsion (chsign!> t
                           (dfprod2!> (vform!> (getdsgn!>  x)
                                               (getel1!> !#!T!H!E!T!A 2))
                                      (getel1!> !#!P!T!H!E!T!A 3))) ))
          (cond (!*torsion (chsign!> t
                           (dfprod2!> (vform!> (getdsgn!>  x)
                                               (getel1!> !#!T!H!E!T!A 3))
                           (getel1!> !#!P!T!H!E!T!A 2)))) )))))
        wtt x))
    (setq wcc nil)
    (setq !#!M!E!T!R!q (mkt!> 2))
    (for!> x (0 1 3) do (for!> y (0 1 3) do
      (cond ((and (leq x y) (or !*full (member (list2 x y)
                                '((0 0)(0 1)(0 2)(1 1)(1 2)(2 2)(2 3)))))
      (putel!> (equation!> (evalalg!> (makezz!> x y wtt))
			   (evalalg!> (list 'times wc
					(getel2s!> !#!T!E!N!M!O!M x y))))
               !#!M!E!T!R!q (list2 x y))))))
    (return t)))

(de getdsgn!> (wa) (mpf!> (getiframe!> wa)))

(de makezz!> (wa wb wss)
  (prog (waa wbb)
    (setq waa (getel1!> wss wa))
    (setq wbb (getel1!> wss wb))
    (return (duald!> (fndfpr!> '(quotient -1 4) (dfsum!> (list
	                (dfprod2!> (getlo!> !#!T wa) wbb)
	                (dfprod2!> (getlo!> !#!T wb) waa) )))))))

(de dctheta!> (x wth)
  (cond ((eqn x 3) (coform!> (evalform!> (dfsum!> (dctheta0!> 2 wth)))))
	(t                   (evalform!> (dfsum!> (dctheta0!> x wth))))))

(de dctheta0!> (x wth)
  (cond
   ((eqn x 0) (list
    (dexsgn!> (getel1!> wth 1))
    (chsign!> t
    (dfprod2!> (dfsum!> (list2 (getel1!> !#!o!m!e!g!a!u 1)
                               (getel1!> !#!o!m!e!g!a!d 1)))
               (getel1!> wth 1)) )
    (chsign!> t
    (dfprod2!> (getel1!> !#!o!m!e!g!a!u 2)
               (getel1!> wth 2)) )
    (chsign!> t
    (dfprod2!> (getel1!> !#!o!m!e!g!a!d 2)
               (getel1!> wth 3)) )  ))
   ((eqn x 1) (list
    (dexsgn!> (getel1!> wth 0))
    (dfprod2!> (dfsum!> (list2 (getel1!> !#!o!m!e!g!a!u 1 )
                               (getel1!> !#!o!m!e!g!a!d 1 )))
               (getel1!> wth 0))
    (dfprod2!> (getel1!> !#!o!m!e!g!a!u 0 )
               (getel1!> wth 3))
    (dfprod2!> (getel1!> !#!o!m!e!g!a!d 0 )
               (getel1!> wth 2))  ))
   ((eqn x 2) (list
    (chsign!> t (dexsgn!> (getel1!> wth 3)))
    (chsign!> t
    (dfprod2!> (dfsum!> (list2 (chsign!> t (getel1!> !#!o!m!e!g!a!u 1 ))
                                           (getel1!> !#!o!m!e!g!a!d 1 )))
               (getel1!> wth 3)) )
    (dfprod2!> (getel1!> !#!o!m!e!g!a!u 2 )
               (getel1!> wth 0))
    (chsign!> t
    (dfprod2!> (getel1!> !#!o!m!e!g!a!d 0 )
               (getel1!> wth 1)))  ))
   ((eqn x 3) (mapcar (dctheta!> 2 wth) 'coform!>))
   ))

(de dexsgn!> (lst)  (mpf!> (dex!> lst)))

(de dcpomega!> (w)
  (progn
    (putel1!> (dfsum!> (list
      (dex!> (getel1!> !#!P!O!M!E!G!A!U 0))
      (fndfpr!> -2 (dfprod2!> (connecu!> 1)
                              (getel1!> !#!P!O!M!E!G!A!U 0)))
      (fndfpr!>  2 (dfprod2!> (connecu!> 0)
                              (getel1!> !#!P!O!M!E!G!A!U 1)))))
      w 0)
    (putel1!> (dfsum!> (list
      (dex!>(getel1!> !#!P!O!M!E!G!A!U 1))
      (fndfpr!> -1 (dfprod2!> (connecu!> 2)
                              (getel1!> !#!P!O!M!E!G!A!U 0)))
      (dfprod2!> (connecu!> 0)
                 (getel1!> !#!P!O!M!E!G!A!U 2)) ))
      w 1)
    (putel1!> (dfsum!> (list
      (dex!>(getel1!> !#!P!O!M!E!G!A!U 2))
      (fndfpr!>  2 (dfprod2!> (connecu!> 1)
                              (getel1!> !#!P!O!M!E!G!A!U 2)))
      (fndfpr!> -2 (dfprod2!> (connecu!> 2)
                              (getel1!> !#!P!O!M!E!G!A!U 1))) ))
      w 2) ))

(de crsigma!> (lst w)
  (prog (wa wb)
    (setq wa(vform!>(getiframe!> 1)(getel1!> w 1)))
    (setq wb(chsign!> t(vform!>(getiframe!> 2)(getel1!> w 0))))
    (putel1!>(dfsum!>(list wa wb (coform!> wa)(coform!> wb)))  lst 0)
    (setq wa(vform!>(getiframe!> 3)(getel1!> w 2)))
    (setq wb(chsign!> t(vform!>(getiframe!> 0)(getel1!> w 1))))
    (putel1!>(dfsum!>(list wa wb (coform!> wa)(coform!> wb)))   lst 1)
    (putel1!>(evalform!>(dfsum!>(list
       (vform!>(getiframe!> 0)(getel1!> w  0))
       (chsign!> t(vform!>(getiframe!> 1)(coform!>(getel1!> w 2))))
       (vform!>(getiframe!> 3)(coform!>(getel1!> w 1)))
       (chsign!> t(vform!>(getiframe!> 3)(getel1!> w 1))) )))
       lst 2) ))


%========= End of GRGgrav.sl ==============================================%

