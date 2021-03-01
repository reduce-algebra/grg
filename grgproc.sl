%==========================================================================%
%   GRGproc.sl                                Forms and Vectors Processor  %
%==========================================================================%
%   GRG 3.2 Standard Lisp Source Code       (C) 1988-96 Vadim V. Zhytnikov %
%==========================================================================%
% This file is distributed without any warranty. You may modify it but you %
% are not allowed to remove author's name and/or distribute modified file. %
%==========================================================================%

%---- Main algebraic simplification functionS -----------------------------

(de eval!> (w)
  (cond (!*aeval  (reval (aeval w)) )
        (t               (reval w)  ) ))

(de raeval!> (w)  (reval (aeval w)))

%---------- Algebraic Simplification --------------------------------------

% Algebraic simplification with NIL return ...
(de evalalg!> (w)
  (cond ((or (null w) (eqn w 0)) nil)
	(t (zn!>(eval!> w)))))

% Alg or Alg Equation simplification ...
(de evalalgx!> (w)
  (cond ((and (pairp w) (eq (car w) 'equal))
           (equationa!> (cadr w) (caddr w)))
	(t (evalalg!> w))))

%---------- Form Simplification -------------------------------------------

% Form simplification ...
(de evalform!> (lst)
  (cond ((null lst) nil)
        (t (proc (wa wb wc)
             (while!> lst
               (setq wa (eval!> (caar lst)))
               (cond ((not(or (eqn wa 0) (null wa)))
                 (setq wc (cons (cons wa (cdar lst)) wc)) ))
               (setq lst (cdr lst)))
           (return (reversip wc))))))

% Alg or Alg Equation simplification ...
(de evalformx!> (w)
  (cond ((and (pairp w) (eq (car w) 'equal))
           (equationf!> (cadr w) (caddr w)))
	(t (evalform!> w))))

% Form simplification with AEVAL ...
(de aevalform!> (lst)
  (cond((null lst)nil)
       (t(proc(wa wb wc)
           (while!> lst
             (setq wa(aeval(caar lst)))
             (cond((not(or(eqn wa 0)(null wa)))
               (setq wc(cons(cons wa(cdar lst))wc)) ))
             (setq lst(cdr lst)))
           (return(reversip wc))))))


%---------- 0 <-> nil -----------------------------------------------------

(de nz!>  (w) (cond (w w) (t 0)))            % alg -> alg0
(de zn!>  (w) (cond ((eqn w 0) nil)(t w)))   % alg0 -> alg

%-------- Multiplication ------------------------------------------------

% Times W * Alg ...
(de multa!> (w wa)
  (cond ((or (null w) (null wa))  nil)
	((eqn w 1)                wa )
	(t (list 'times w wa))))

% Times W * Alg or Alg Equation ...
(de multax!> (w wa)
  (cond ((and (pairp wa) (eq (car wa) 'equal))
           (equation!> (multa!> w (cadr wa)) (multa!> w (caddr wa))))
	(t (multa!> w wa))))

% Times W * Form ...
(de multf!> (w wa)
  (cond ((or (null w) (null wa))  nil)
	((eqn w 1)                wa )
	(t (fndfpr!> w wa))))

% Times W * Form or Form Equation ...
(de multfx!> (w wa)
  (cond ((and (pairp wa) (eq (car wa) 'equal))
           (equation!> (multf!> w (cadr wa)) (multf!> w (caddr wa))))
	(t (multf!> w wa))))

%-------- Summatuon --------------------------------------------------------

% Sum list of Alg ...
(de summa!> (w)
  (cond ((null w) nil)
        (t (evalalg!> (cons 'plus w)))))

% Sum list of Alg or Alg Equations ...
(de summax!> (w)
  (cond ((not(equationp!> w)) (summa!> w))
	(t (equation!> (summa!> (mapcar w 'eqleft!>))
		       (summa!> (mapcar w 'eqright!>))))))

% Sum list of Forms ...
(de summf!> (w)
  (cond ((null w) nil)
        (t (evalform!> (dfsum!> w)))))

% Sum list of Forms or Forms Equations ...
(de summfx!> (w)
  (cond ((not(equationp!> w)) (summf!> w))
	(t (equation!> (summf!> (mapcar w 'eqleft!>))
		       (summf!> (mapcar w 'eqright!>))))))

(de eqleft!>  (w)  (cond ((pairp w) (cadr w)) (t nil)))
(de eqright!> (w)  (cond ((pairp w) (caddr w)) (t nil)))

(de equationp!> (w)
  (cond ((null w) nil)
        ((null(car w)) (equationp!>(cdr w)))
	((pairp(car w)) (eq (caar w) 'equal))
	(t nil)))

% Summation ...
(de algsum!> (w)
  (progn
    (setq w (algsum1!> w))
    (cond ((null w) w)
	  ((null(cdr w)) (car w))
	  (t (cons 'plus w)))))

(de algsum1!> (w)
  (cond ((null w) nil)
	((null(car w)) (algsum1!>(cdr w)))
	(t (cons (car w) (algsum1!>(cdr w))))))


%-------- Equations building functions -------------------------------------

(de equation!> (wl wr) % makes (equal wl wr) or nil if both null ...
  (cond ((and (null wl) (null wr)) nil)
        (t (list 'equal wl wr))))

(de equationf!> (w1 w2)  % form=form  with eval ...
  (cond((and(null(setq w1(evalform!> w1)))
            (null(setq w2(evalform!> w2)))) nil)
       (t(list3 'equal w1 w2))))

(de equationf1!> (w1 w2) % form=form -> form-form=0  with eval ...
  (cond((null(setq w1
          (evalform!>(dfsum!>(list w1 (chsign!> t w2))))))
             nil)
       (t(list3 'equal w1 nil))))

(de equationa!> (w1 w2)  % alg=alg  with eval ...
  (cond((and(or(null(setq w1(eval!> w1)))(eqn w1 0))
            (or(null(setq w2(eval!> w2)))(eqn w2 0)) ) nil)
       (t(list3 'equal (zn!> w1) (zn!> w2)))))

(de equationa1!> (w1 w2)  % alg=alg -> alg-alg=0 with eval ...
  (cond((or(null(setq w1(eval!>(list3 'difference w1 w2))))(eqn w1 0))
            nil)
       (t(list3 'equal (zn!> w1) nil))))


%------ Forms <-> Reduce matrix conversion support -------------------------

% (LIST of 1-forms) -> Reduce matrix ... 05.96
(de mkmtetr!> (lst)
  (cons 'mat
    (foreach!> a in (dimlist!> 0) collect
      (foreach!> b in (dimlist!> 0) collect
        (getfdx!> (getel1!> lst a) b)))))

% Reduce matrix -> tetrad (LIST of 1-forms) ... 05.96
(de mktetrm!> (w ww)
  (prog(wa wb wc) (setq wa 0)
    (foreach!> x in w do (progn
      (setq wa(add1 wa))
      (setq wc nil) (setq wb -1)
      (foreach!> y in x do (progn (setq wb(add1 wb))
        (setq wc(dfsum!>(list2 wc(fndfpr!>(zn!>(eval!> y))(mkdx!> wb)))))))
      (putel1!> (evalform!> wc) ww (sub1 wa))))
    (return t)))


%----- Matrix Reduce <-> GRG conversion support ----------------------------

(de mat!> (lst) % 05.96  GRG -> Reduce
  (cons  'mat
    (foreach!> i in (dimlist!> 0) collect
      (foreach!> j in (dimlist!> 0) collect
        (getel!> lst (list2 i j))))))

(de mats!> (lst) % 05.96  GRG -> Reduce
  (cons  'mat
    (foreach!> i in (dimlist!> 0) collect
      (foreach!> j in (dimlist!> 0) collect
        (getel2s!> lst i j)))))

(de matsf!> (fun) % 05.96  GRG -> Reduce
  (cons  'mat
    (foreach!> i in (dimlist!> 0) collect
      (foreach!> j in (dimlist!> 0) collect
        (eval (list fun i j))))))

(de rmat!> (lst wm) % 05.96  Reduce -> GRG
  (prog (w)
    (fordim!> i do (progn
       (setq wm (cdr wm))
       (setq w (car wm))
       (fordim!> j do (progn
         (putel!> (zn!>(eval!>(car w))) lst (list2 i j))
         (setq w (cdr w))))))))

(de rmats!> (lst wm) % 05.96  Reduce -> GRG
  (prog (w)
    (fordim!> i do (progn
       (setq wm (cdr wm))
       (setq w (car wm))
       (fordim!> j do (progn
         (cond((leq i j)
           (putel!> (zn!>(eval!>(car w))) lst (list2 i j))))
         (setq w (cdr w))))))))


%---------- Sign Changing --------------------------------------------------

(de chsignf!> (w) (chsign!> t    w))  % form
(de chsigna!> (w) (chsign!> nil  w))  % alg expression

% Sign changing ... BOOL=T - Form, BOOL=NIL - Alg
(de chsign!> (bool lst)
  (cond((null lst) nil)
       (bool(mapcar lst 'chsign1!>))
       (t(chsign2!> lst))))

(de chsignx!> (wt w)
  (cond ((and (pairp w) (eq (car w) 'equal))
	   (equation!> (chsign!> wt (cadr w)) (chsign!> wt (caddr w))))
	(t (chsign!> wt w))))

(de chsign1!> (w)
  (cond((and(pairp(car w))(eq(caar w) 'minus))
         (cons (cadar w) (cdr w)))
       ((numberp(car w))
         (cons (minus(car w)) (cdr w)))
       (t(cons (list2 'minus(car w)) (cdr w)))))

(de chsign2!> (w)
  (cond((and(pairp w)(eq(car w) 'minus)) (cadr w))
       ((numberp w) (minus w))
       (t (list2 'minus w))))


%----------  Exterior Forms Processor. 10.01.91  ---------------------------

% Exterior forms summation ...
(de dfsum!> (lst)
  (cond
    ((null lst)nil)
    ((null(cdr lst))(car lst))
    (t(proc (w ww wt wn wr wx)
        (setq w (flcopy!> lst))
        (setq ww w)
	(loop!>
          (setq wn nil)
	  (setq w ww)
	  (while!> w
	    (cond((car w)
	      (cond((null wn) (setq wn (cadaar w)))
		   ((lessp(cadaar w)wn) (setq wn (cadaar w))))))
	    (setq w (cdr w)))
	(exitif(null wn))
	  (setq w ww)
	  (setq wt nil)
	  (while!> w
	    (cond((car w)
	      (cond((eqn wn (cadaar w))
		(progn
		  (setq wx (cdaar w))
		  (setq wt (cons (caaar w) wt))
		  (rplaca w (cdar w)) )))))
	    (setq w (cdr w)))
	  (cond((cdr wt)(setq wt (cons (cons 'plus wt) wx)))
	       (t       (setq wt (cons (car wt) wx))))
	  (setq wr (cons wt wr)) )
	(return(reversip wr)) ))))

(de flcopy!> (w)
  (cond((null w) nil)
       (t(cons (car w) (flcopy!> (cdr w))))))

% alg * form or vector  multiplication ...
(de fndfpr!> (alg form)
  (cond((or(null form)(zerop alg)(null alg))nil)
       ((eqn alg 1) form)
       ((eqn alg -1) (chsign!> t form))
       (t(proc(wa)
           (while!> form
             (setq wa
               (cons (cons (list 'times alg (caar form)) (cdar form))
                     wa))
             (setq form(cdr form)))
           (return(reversip wa))))))

% Exterior product ...
(de dfprod!> (lst)
   (cond ((memq nil lst) nil)
         ((null(cdr lst)) (car lst))
         (t (dfprod2!> (car lst) (dfprod!>(cdr lst))))))

% Exterior product form1/\form2 ...
(de dfprod2!> (frm1 frm2)
  (cond((null(and frm1 frm2))nil)
    (t(proc (x y wa wb wc w res sgn)
        (setq w t)
        (while!> frm1
          (setq wa frm2)
          (while!> frm2
            (setq sgn t)
            (setq x(cddar frm1))
            (setq y(cddar frm2))
            (while!> (and x y (null(and(caar x)(caar y)))
                          (prog2 (and (null(cdr y)) (setq w(not(cdar y))))
                                  t))
                (setq wb(cons(cons(or(caar x)(caar y))
                                  (eq(cdar x)(cdar y)))
                             wb))
                (cond((and(caar x)(not(cdar y)))
                       (setq sgn(not sgn)) ))
                (setq x(cdr x))
                (setq y(cdr y)) )
            (tohead (and x y (caar x) (caar y)
                        (progn (setq wb nil) (setq frm2(cdr frm2)) t)))
            (while!> x
              (setq wb(cons(cons(caar x)
                                (cond((caar wb)(not(cdar wb)))
                                     (t(cdar wb))))
                           wb))
              (cond((and(caar wb)(null w))
                     (setq sgn(not sgn))))
              (setq x(cdr x)))
            (while!> y
              (setq wb(cons(cons(caar y)
                                (cond((caar wb)(not(cdar wb)))
                                     (t(cdar wb))))
                           wb))
              (setq y(cdr y)))
            (setq x(list3(quote times)(caar frm1)(caar frm2)))
            (cond((null sgn)(setq x(list2(quote minus)x))))
            (setq y(cons x(cons
                            (plus(cadar frm1)(cadar frm2))
                            (reversip wb))))
            (setq wc(cons y wc))
            (setq wb nil)
            (setq frm2(cdr frm2)))
          (setq frm1(cdr frm1))
          (setq frm2 wa)
          (cond(wc(prog2(setq res(cons(reversip wc)res))
                        (setq wc nil)))) )
        (return(dfsum!> res)) )) ))

(de dfsum2!> (wa wb) (dfsum!> (list2 wa wb)))

% Exterior differential  d form ...
(de dex!> (frm) (dex1!> frm ![umod!]))

(de dex1!> (frm wm)
  (cond ((null frm) nil) (t
    (prog(w)
      (foreach!> x in frm do (prog2
        (setq w (cons (dfprod2!> (dfun1!> (car x) wm)
			         (ncons (cons 1 (cdr x))) )
		      w))
        (cond (wm (setq w (cons (fndfpr!> (car x) (dexxb!>(cdr x)))
		                w))))
	))
      (return (dfsum!> w))))))

(de dexxb!> (w) % with d(b/\...) accumulation
  (proc (wc wr ww)
    (setq ww (car w))
    (cond ((setq wc (assoc (car w) ![dbas!])) (return(cdr wc)))
	  (t (setq w (cdr w))))
    (setq wc -1)
    (while!> w
      (setq wc (add1 wc))
      (cond ((caar w) (setq wr (cons (getel1!> !#!b wc) wr))))
      (setq w (cdr w)))
    (setq wr (evalform!>(nbform!>(dex1!>(dfprod!>(reversip wr))nil))))
    (setq ![dbas!] (cons (cons ww wr) ![dbas!]))
    (return wr)))

%(de dexxb!> (w) % without d(b/\...) accumulation
%  (proc (wc wr)
%    (setq w (cdr w))
%    (setq wc -1)
%    (while!> w
%      (setq wc (add1 wc))
%      (cond((caar w)(setq wr(cons(getel1!> !#!b wc)wr))))
%      (setq w (cdr w)))
%    (return(nbform!>(dex1!>(dfprod!>(reversip wr))nil)))))

% Exterior differential   d Alg ...
(de dfun!> (lst) (dfun1!> lst ![umod!]))

(de dfun1!> (lst wm)
  (cond((null lst) nil) (t
    (proc (wb wc wd)
      (foreach!> x in ![cord!] do (prog2
        (setq wd (mkdf!> lst x wm))
        (cond (wd
          (setq wb
            (cons (cons wd (cdar (mkdx!> (get x '!=cord))))
                  wb))))))
      (return(reversip wb)))) ))


(de mkdf!> (lst id wm)
   (evalalg!> (cond (wm (bfun!> (getel1!> !#!e (get id '!=cord)) lst))
                    (t  (list3 'df lst id)))))

(de bfun!> (wb lst)
 (cond((null lst) nil)
      (t(proc (w wn wc)
	  (while!> wb
	    (setq wn (cadar wb))
	    (setq wc -1)
            (while!> (not(eqn wn 1))
              (setq wn (quotient wn 2))
              (setq wc (add1 wc)) )
	    (setq w(cons(list 'times (caar wb)
                              (list 'df lst (getel1!> ![cord!] wc)))
			w))
	    (setq wb(cdr wb)))
	  (return(cond((null w) nil)
		      ((null(cdr w)) (car w))
		      (t(cons 'plus w))))))))


%----------  Vectors processor.  08.01.91  ---------------------------------

%  Vec _| 1-form ...
(de vform1!> (wv wf)
   (cond((or (null wv)(null wf)) nil)
     (t(proc (w wa)
         (setq wa wf)
         (while!> wv
           (setq wf wa)(setq wa nil)
           (while!> wf
             (cond((eqn(cadar wf)(cadar wv))
                    (setq w(cons(list 'times(caar wf)(caar wv))w)))
                  (t(setq wa(cons(car wf)wa))))
             (setq wf(cdr wf)))
           (setq wv(cdr wv)))
         (return(cond((null w) nil)
		     ((null(cdr w)) (car w))
                     (t(cons 'plus w))))))))

% Vec | Alg ...
(de vfun!> (wv wf)
   (cond ((or (null wv) (null wf)) nil)
         (t (vfun1!> wv wf ![umod!]))))

%(de vfun0!> (wv wf)
%   (cond((or(null wv)(null wf)) nil)
%        (t(vfun1!> wv wf nil))))

(de vfun1!> (wv wf wm)
   (proc (wb wa x cord)
     (setq cord ![cord!])
     (while!> (and cord wv)
       (setq x (car cord))
       (setq cord (cdr cord))
       (cond
         ((eqn (expt 2 (add1(get x '!=cord))) (cadar wv))
	    (progn
             (setq wa (mkdf!> wf x wm))
	     (cond(wa
	       (setq wb
		 (cons (list 'times (caar wv) wa)
		       wb))))
             (setq wv (cdr wv)) ))))
     (return (cond ((null wb) nil)
                   ((null (cdr wb)) (car wb))
                   (t (cons 'plus wb))) )))

% Vecr _| n-form for n>1 ...
(de vform!> (wv wf)
   (cond((or(null wv)(null wf)) nil)
     (t(proc(w wl wa wb wc wss)
         (while!> wv
           (setq wl wf)
           (while!> wl
             (setq wa(cddar wv))
             (setq wb(cddar wl))
             (setq wc nil)
             (while!> (and wa wb)
               (exitif (and(caar wa)(caar wb)))
               (setq wc(cons(car wb)wc))
               (setq wa(cdr wa))
               (setq wb(cdr wb)))
             (cond((and wa wb) (progn
               (setq wss(cdar wb))
               (setq wc(cons(cons nil(cdar wb))wc))
               (setq wb(cdr wb))
               (while!> wb
                 (setq wc(cons(cons(caar wb)(not(cdar wb)))wc))
                 (setq wb(cdr wb)))
               (setq w (cons(ncons(append(list
                       (list 'times(caar wv)
                             (cond(wss(caar wl))
                                  (t(list 'minus(caar wl)))))
                 (difference(cadar wl)(cadar wv)) )
                 (rever!> wc))) w)) )))
             (setq wl(cdr wl)))
           (setq wv(cdr wv)))
         (return(cond(w(dfsum!> w))
                     (t nil)))))))

(de rever!>(wc)
   (proc(w wss)
     (while!> wc
       (cond((and(null wss)(null(caar wc))) nil)
            (t(prog2(setq wss t)(setq w(cons(car wc)w)))))
       (setq wc(cdr wc)))
     (return w)))

% [ vec1 , vec2 ] ...
(de vbrack!> (w1 w2)
   (cond((and w1 w2)
          (dfsum!> (list2 (vcvc!> w1 w2 ![umod!])
                          (chsign!> t (vcvc!> w2 w1 ![umod!])))))
	(t nil)))

(de vcvc!> (w1 w2 wm)
   (proc (w wc ww wa)
     (while!> w2
       (setq wc (vfun1!> w1 (caar w2) wm))
       (cond (wc (setq w (cons (cons wc (cdar w2)) w))))
       (cond (wm
         (cond ((setq wa (vcb!> w1 (sub1(log2!>(cadar w2)))))
           (setq ww (cons (fndfpr!> (caar w2) wa) ww))))))
       (setq w2 (cdr w2)))
     (return (cond ((and wm ww) (dfsum!> (cons (reversip w) ww)))
		   (t (reversip w))))))

(de vcb!> (w1 we)
  (cond ((null w1) nil)
    (t(proc (wa w)
        (setq we (getel1!> !#!e we))
	(while!> w1
          (setq wa (vcvc!> (getel1!> !#!e (sub1(log2!>(cadar w1))))
			   we nil))
	  (cond (wa
            (setq w (cons (fndfpr!> (caar w1) (nbvec!> wa)) w))))
	  (setq w1 (cdr w1)))
        (return (cond (w (dfsum!> w))
		      (t nil)))))))


%----------  Complex conjugation. 25.12.90  --------------------------------

(de coexpr!> (wt w) % wt - type, 0 alg, n form, -1 vector
  (cond ((eqn wt  0) (coalg!> w))
	((eqn wt -1) (covec!> w))
	(t           (coform!> w))))

(de coexprx!> (wt w)
  (cond ((and (pairp w) (eq (car w) 'equal))
	   (equation!> (coexpr!> wt (cadr w))
                       (coexpr!> wt (caddr w))))
	(t (coexpr!> wt w))))

% Conjugation of Alg ...
(de coalg!> (w)
   (cond ((atom w)
            (cond ((or (eq w '!*sq) (eq w 'taylor!*))
                                (err!> 9999))        % *sq form !!!
                  ((eq w 'i) '(minus i))             % i ->  -i
                  ((get w '!=conj) (get w '!=conj))  % x~ -> x, x -> x~
                  (t w)))                            % y -> y
         (t (mapcar w 'coalg!>))))

% Conjugation of Form ...
(de coform!> (wf) (cofv!> wf ![ccb!]))

% Conjugation of Vector ...
(de covec!> (wf) (cofv!> wf ![ccbi!]))

(de cofv!> (wf wb)
   (cond ((null wf) nil)
     (t(proc (w wa wp wx wn)
         (while!> wf
           (setq wa (coalg!>(caar wf)))
           (setq wx (cddar wf))  % wx = d x/\d y ...
           (setq wp nil)
           (setq wn -1)
           (while!> wx
             (setq wn (add1 wn))
             (cond((caar wx)
               (setq wp (cons
                 (cond (![umod!] (getel1!> wb wn))
                       (t (mkdx!>
                            (get (coalg!>(getel1!> ![cord!] wn)) '!=cord))))
		 wp))))
             (setq wx (cdr wx)))
           (setq wp (dfprod!>(reversip wp)))  % wp = (d x/\d y ...)~
           (setq w (cons (fndfpr!> wa wp) w))
           (setq wf (cdr wf)))
         (return(evalform!>(dfsum!> w)))))))

%---------- Vector Product 09.96 -------------------------------------------

%  vec.vec  Need !#G !#T
(de vprod!> (wa wb)
  (prog (w wx wy)
    (fordim!> m do (progn
      (setq wx (vform1!> wa (getframe!> m)))
      (setq wy (vform1!> wb (getlo!> !#!T m)))
      (cond ((and wx wy) (setq w (cons (list 'times wx wy) w))))))
   (return (cond (w (cons 'plus w)) (t nil)))))

%  frm1.frm1  Need !#D !#GI
(de fprod!> (wa wb)
  (prog (w wx wy)
    (fordim!> m do (progn
      (setq wx (vform1!> (getiframe!> m) wa))
      (setq wy (vform1!> (getup!> !#!D m) wb))
      (cond ((and wx wy) (setq w (cons (list 'times wx wy) w))))))
   (return (cond (w (cons 'plus w)) (t nil)))))

%---------- Dualisation 05.96 ----------------------------------------------

% Dualisation  #(alg) -> dim-form ...
% Use: !#VOL
(de dual0!> (w)
  (cond ((null w) nil)
        (t (fndfpr!> w (car !#!V!O!L)))))

% Dualisation  #(dim-form) -> alg ...
% Use: !#VOL
(de duald!> (w)
  (cond ((null w) nil)
        (t (list 'times (invsvol!>) (caar w)))))
% version for spinorial regime only = - i #
(de dualdi!> (w)
  (cond ((null w) nil)
        (t (list 'times (invsvoli!>) (caar w)))))

(de invsvol!> nil
  (cond ((null(car !#!V!O!L)) 0)
	(t (list 'quotient ![sigprod!] (caaar !#!V!O!L)))))

(de invsvoli!> nil
  (cond ((null(car !#!V!O!L)) 0)
	(t (list 'quotient 'i (caaar !#!V!O!L)))))

% Defines P of the P-form ...
(de pformq!> (w)
  (proc (wp)
    (cond ((null w) (return 0)))
    (setq wp 0)
    (setq w (cddar w))
    (while!> w
      (cond ((caar w) (setq wp (add1 wp))))
      (setq w (cdr w)))
    (return wp)))

% Dualisation  #(p-form) -> (dim-p)-form ...
% Use: !#sdetG !#G !#T !#VOL
(de dual!> (w)
  (cond ((null w) nil)
    (t(proc (wp wdp wr wl wf wc)
	(setq wp (pformq!> w)) % We are dualizing p-form=wp
	(cond ((eqn wp ![dim!]) (return (duald!> w))))
	(setq wdp (difference ![dim!] wp)) % to (dim-p)-form
	(setq ![tlow!] % List of T_a (lower index a)
          (foreach!> x in (dimlist!> 0) collect (getlo!> !#!T x)))
	(setq wl (mklambda!> wdp ![dim!])) % All T_a/\... (dim-p)-forms
	(setq wf (invsvol!>)) % The coefficient
	(while!> wl
	  (setq wc (dfprod2!> (cdar wl) w))
	  (cond (wc (setq wr (cons (fndfpr!> (list 'times wf (caar wc))
				             (tprod!> (caar wl)))
			           wr))))
	  (setq wl (cdr wl)))
	(return (dfsum!> wr)) ))))

(de mklambda!> (wp wd)
  (proc (wr ww wc wn wi wa)
    (setq wr (mklist!> (sub1 wp) (sub1 wd)))
    (setq wr (mapcar wr 'lform1!>))
    (setq wi (sub1 wp))
    (while!> (greaterp wi 0)
      (setq ww nil)
      (while!> wr
	(setq wc (car wr))
	(setq wn (mklist!> (sub1 wi) (sub1(caar wc))))
	(while!> wn
	  (setq wa (car wn))
	  (setq ww (cons (cons (cons wa (car wc))
			       (dfprod2!> (getel1!> ![tlow!] wa)
                                          (cdr wc)))
                         ww))
          (setq wn (cdr wn)))
	(setq wr (cdr wr)))
      (setq wr (reversip ww))
      (setq wi (sub1 wi)))
   (return wr)))

(de lform1!> (w) (cons (ncons w) (getel1!> ![tlow!] w)))

(de tprod!> (w)
  (cond ((null(cdr w)) (getframe!> (car w)))
	(t (dfprod2!> (getframe!> (car w))
                      (tprod!> (cdr w))))))

(de mklist!> (wa wb)
  (cond ((greaterp wa wb) nil)
	(t (cons wa (mklist!> (add1 wa) wb)))))


%---------- Limits ---------------------------------------------------------

%  Limits  6.03.94 ...
%(de lima!> (wx wl wt lst)
%  (cond((null lst) nil)
%       ((eq wt 'p) (list 'limit!+ lst wx wl))
%       ((eq wt 'm) (list 'limit!- lst wx wl))
%       (t (list 'limit lst wx wl))))
%
%(de limf!> (wx wl wt lst)
%  (cond((null lst) nil)
%       (t(proc (wr)
%	   (while!> lst
%	     (setq wr (cons (cons (lima!> wx wl wt (caar lst))
%				  (cdar lst)) wr))
%	     (setq lst (cdr lst)))
%	   (return(reversip wr))))))


%----------  SUBstitutions 7.03.94 -----------------------------------------

(de subalg!> (wl lst)
  (cond((null lst) nil)
       (t(cons 'sub (append wl (ncons lst))))))

(de subdf!> (wl lst)
  (cond((null lst) nil)
       (t(proc (wr)
	   (while!> lst
	     (setq wr (cons (cons (subalg!> wl(caar lst))
				  (cdar lst)) wr))
	     (setq lst (cdr lst)))
	   (return(reversip wr))))))


%-------- Anholonomic Mode  04.03.91, 05.96 --------------------------------

% Anholonomic/Holonomic command ...
(de turnbg!> (wm)
  (prog2
    (setq wm (errorset!> (list 'turnbg0!> wm) ![erst1!] ![erst2!]))
    (cond ((atom wm) (erm!> wm) (erm!> 8803) (msg!> 88033) !!er!!)
          (t         (car wm))) ))

(de turnbg0!> (wm)
  (proc (w)
    (cond((eq wm ![umod!]) (progn    % current mode ?
            (prin2 "Current Basis is ")
            (cond(![umod!](prin2 "an")))
            (prin2 "holonomic already.")(terpri)
            (return t))))
    (setq ![chain!] nil)
    (setq w (request!> '!#!b))        % basis ?
    (cond((eq w !!er!!) (return w))
         ((null w) (trsf!> '!#!b)(setq ![er!] 6046)(return !!er!!)))
    (setq ![chain!] nil)
    (setq w (request!> '!#!e))        % inverse basis ?
    (cond((eq w !!er!!) (return w))
         ((null w) (trsf!> '!#!b)(setq ![er!] 6046)(return !!er!!)))
    (setq w (evalform!>(dfprod!> !#!b)))  % singular basis ?
    (cond ((null w) (prog2 (setq ![er!] 8400) (return !!er!!))))
    (setq w (evalform!>(dfprod!> !#!e)))  % singilar inverse basis ?
    (cond ((null w) (prog2 (setq ![er!] 8401) (return !!er!!))))
    (cond (wm (mktables!>))
          (t (prog2 (setq ![xf!] !#!b)              % b = d x
                    (setq ![xv!] !#!e))))           % e = @ x
    (setq ![xb!] nil)
    (setq w (altdata!>(alldata!>)))
    (while!> w                   % converting all data to new basis ...
      (cond ((or (memq (car w) '( ![cord!] ![const!] ![fun!] ![apar!]
                                  !#!b !#!e))
                 (zerop (gettype!> (car w))))    nil)
            (t (set (car w)
                 (allcoll!> (eval(car w)) (car w) nil
                            (cond((get (car w) '!=idxl)(get (car w) '!=idxl))
                                 (t '(0)))
                            (function nbel!>)))  ))
      (setq w (cdr w)))
    (setq ![umod!] wm)
    (cond ((null wm) (progn
      (setq ![ccb!] nil)
      (setq ![ccbi!] nil)
      (setq ![xv!] nil)
      (setq ![xf!] nil))))
    (ftype!>)
    (fitype!>)
    (done!>)
    (return t)))

% New basis for element ...
(de nbel!> (lst wi wn)
  (cond ((null lst) nil)
        ((and (eqn (gettype!> wn) -1) (not (flagp wn '!+equ))) % vec
          (nbvec!> lst))
        ((not (flagp wn '!+equ))                               % form
          (nbform!> lst))
        ((eqn (gettype!> wn) -1)                               % eq vec
          (equation!> (nbvec!>(cadr lst)) (nbvec!>(caddr lst))))
        (t                                                     % eq form
          (equation!> (nbform!>(cadr lst)) (nbform!>(caddr lst))))
        ))

% New basis for form ...
(de nbform!> (w)
  (cond ((null w) w)
        (t (evalform!> (dfsum!> (mapcar w (function nbform1!>)))))))

(de nbform1!> (w)
  (fndfpr!> (car w)
            (nbxb!> (cdr w))))

% New basis for d X/\d Y/\...
(de nbxb!> (w)
  (cond
    ((assoc (car w) ![xb!]) (cadr (assoc (car w) ![xb!])))
    (t (progn
        (setq ![xb!] (cons (list2 (car w) (evalform!> (mkbxb!>(cdr w) )))
                           ![xb!]))
        (cadar ![xb!])))))

(de mkbxb!> (w)
  (proc (wa wn)
    (setq wn 0)
    (while!> w
      (cond ((caar w)
        (setq wa (cons (getel1!> ![xf!] wn) wa))))
      (setq wn (add1 wn))
      (setq w (cdr w)))
    (return (evalform!> (dfprod!>(reverse wa))))))

(de mktables!> nil
  (prog (w)
     (setq ![xf!] (mkt!> 1))
     (setq w (aeval (list 'quotient 1 (mkmtetr!> !#!b))))
     (mktetrm!> (cdr w) ![xf!])     % d x = b
     (setq ![xv!] (mkt!> 1))
     (setq w (aeval (list 'tp (mkmtetr!> !#!b))))
     (mktetrm!> (cdr w) ![xv!])     % @ x = e
     (setq ![ccb!]                  % ~ b
           (mapcar (mapcar !#!b 'coform!>) (function nbform!>)))
     (setq ![ccbi!]                 % ~ e
           (mapcar (mapcar !#!e 'coform!>) (function nbvec!>)))
     ))

% New basis for vector ...
(de nbvec!> (w)
  (cond ((null w) w)
        (t (evalform!> (dfsum!> (mapcar w (function nbvec1!>)))))))

(de nbvec1!> (w)
  (fndfpr!> (car w)
            (nbxv!> (cadr w))))

(de nbxv!> (w)
  (proc (wc)
    (setq wc -1)
    (while!> (not (eqn w 1))
      (setq w (quotient w 2))
      (setq wc (add1 wc)) )
    (return (getel1!> ![xv!] wc)) ))


%========= End of GRGproc.sl ==============================================%

