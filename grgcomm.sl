%==========================================================================%
%   GRGcomm.sl                                              Main Commands  %
%==========================================================================%
%   GRG 3.2 Standard Lisp Source Code       (C) 1988-97 Vadim V. Zhytnikov %
%==========================================================================%
% This file is distributed without any warranty. You may modify it but you %
% are not allowed to remove author's name and/or distribute modified file. %
%==========================================================================%


%---------- Some General Aux Functions -----------------------------------

% Data name -> Internal variables list ...
(de dgood!> (lst)
  (prog (w wa wss)
    (setq w lst)
    (cond ((eqs!> lst '(all)) (return(alldata!>)))) % word!!!
    (setq wss lst)
    (setq lst (assocf!> lst ![datl!]))
    (cond ((and (null lst) w (null(cdr w)) (idp(car w)))
      (progn (setq wa (incomiv!>(explode(car w))))
             (cond ((flagp wa '!+ivar) (setq lst (ncons wa)))))))
    (cond ((null lst) (progn (setq ![er!] 6030)
			     (doubl!> wss)
                             (return !!er!!))))
    (setq lst (car lst))
    (cond ((atom lst) (setq lst (ncons lst))))
    (setq w (constrpl!> lst))
    (cond ((eq w !!er!!) (return !!er!!)))
    (return lst)))

% Same but for Write Macro Tensors are alowed ...
(de dgoodw!> (lst)
  (prog (w wa wss)
    (setq w lst)
    (cond ((eqs!> lst '(all)) (return(alldata!>)))) % word!!!
    (setq wss lst)
    (setq lst (assocf!> lst ![datl!]))
    (cond ((and (null lst) w (null(cdr w)) (idp(car w)))
      (progn (setq wa (incomiv!>(explode(car w))))
             (cond ((or (flagp wa '!+ivar) (flagp wa '!+macros2))
                      (setq lst (ncons wa)))))))
    (cond ((null lst) (progn (setq ![er!] 6030)
			     (doubl!> wss)
                             (return !!er!!))))
    (setq lst (car lst))
    (cond ((atom lst) (setq lst (ncons lst))))
    (setq w (constrpl!> lst))
    (cond ((eq w !!er!!) (return !!er!!)))
    (return lst)))

% All existing data variables ...
(de alldata!> nil
  (proc (w lst)
    (setq lst ![datl!])
    (while!> lst
      (cond ((and (atom(cadar lst)) (eval(cadar lst)))
        (setq w (cons (cadar lst) w))))
      (setq lst (cdr lst)))
    (setq lst ![abbr!])
    (while!> lst
      (cond ((eval(car lst)) (setq w (cons (car lst) w))))
      (setq lst (cdr lst)))
    (return(reversip w))))

% Data variables list modification in correspondence with flags ..
(de altdata!> (w)
  (cond ((null w) nil)
        ((atom (car w)) (consmem!> (car w) (altdata!>(cdr w))))
        ((eval(caar w)) (appmem!> (cdar w) (altdata!>(cdr w))))
        (t (altdata!>(cdr w)))))


%-----  Commands in `grg.cfg' file ---------------------------------------

% Package ...
(dm package!> (w) (list 'package0!> (list 'quote (cdr w))))
(de package0!> (w)
  (prog (ww)
    (setq ![lower!] (islowercase!>))
    lab
    (cond ((null w) (return nil)))
    (setq ww (loadpack!> (ncons(car w)) nil))
    (cond ((eq ww !!er!!) (prog2 (erm!> ![er!]) (return !!er!!))))
    (setq w (cdr w))
    (go lab)
    ))

% On ...
(dm on!> (w) (list 'on0!> (list 'quote (cdr w))))
(de on0!> (w)
  (prog (ww)
    (setq ![lower!] (islowercase!>))
    lab
    (cond((null w)(return nil)))
    (setq ww (onoff!> (ncons(car w)) t))
    (cond((eq ww !!er!!) (prog2 (erm!> ![er!])(return !!er!!))))
    (setq w (cdr w))
    (go lab)
    ))

% Off ...
(dm off!> (w) (list 'off0!> (list 'quote (cdr w))))
(de off0!> (w)
  (prog (ww)
    (setq ![lower!] (islowercase!>))
    lab
    (cond((null w)(return nil)))
    (setq ww (onoff!> (ncons(car w)) nil))
    (cond((eq ww !!er!!) (prog2 (erm!> ![er!])(return !!er!!))))
    (setq w (cdr w))
    (go lab)
    ))

% Signature ...
(dm signature!> (w) (list 'signature0!> (list 'quote (cdr w))))
(de signature0!> (w)
  (proc (wr ww)
     (setq ww w)
     (while!> ww
	(cond ((equal (car ww) '!+) (setq wr (cons 1 wr)))
	      ((equal (car ww) '!-) (setq wr (cons -1 wr)))
	      (t (erm!> 9002) (bye)))
	(setq ww (cdr ww)))
     (setq ![sgn!] (reverse wr))
     (setq ![dim!] (length ![sgn!]))
     (cond ((lessp ![dim!] 2) (erm!> 9002) (bye)))
     (tunedim!>) ))


%-----  On ...;  and  Off ...;  commands  20.02.94 -----------------------

(de onoff!> (lst bool)
  (proc (w wc wo ww)
    (cond ((null lst) (return nil)))
    (setq w (memlist!> '!, lst))
    (cond ((eq w !!er!!) (prog2 (setq ![er!] 1100) (return !!er!!))))
    (while!> w
      (setq wc (car w))
      (cond
        ((or (cdr wc) (not(idp(car wc)))) % bad parameter ...
          (prog2 (setq ![er!] 1100) (return !!er!!))) )
      (setq wc (idtostcase!> (car wc)))
      (cond
        ((flagp wc 'switch) % reduce switch ...
          (progn
	    (setq ww (makeswvar!> wc))
            (setq wo (eval ww))
	    (cond((not(equal wo bool))(prog2
              (cond
                ((iscsl!>)
                   (cond (bool (eval(list 'on  (list 'quote (ncons wc)))))
                         (t    (eval(list 'off (list 'quote (ncons wc)))))))
                (t (cond (bool (eval(list '!~on  (list 'quote (ncons wc)))))
                         (t    (eval(list '!~off (list 'quote (ncons wc))))))))
              (onoff1!> wc bool) ))))) % maybe extra grg tuning ...
        ((flagp wc '!+switch) % grg switch ...
          (progn
	    (setq ww (makeswvar!> wc))
            (setq wo (eval ww))
	    (cond((not(equal wo bool))
              (onoff1!> wc bool) )) ))
        (t(progn % none of above ...
          (doub!> wc)(setq ![er!] 6402)(return !!er!!))))
      (cond((not(equal wo bool))
        (setq ![flaghis!] (cons (cons wc wo) ![flaghis!]))))
      (setq w (cdr w)))))

% On/Off GRG switch with tuning ...
(de onoff1!> (w bool)
  (progn
    (set (makeswvar!> w) bool)
    (setq w (get w '!=tuning)) % tuning required ...
    (cond(w (apply w (list bool))))))

% On/Off GRG switch without tuning ...
(de onoff2!> (w bool)
  (set (makeswvar!> w) bool))

% On/Off GRG switch without tuning but with history ...
(de onoff3!> (w bool)
  (prog (ww wo)
    (setq ww (makeswvar!> w))
    (setq wo (eval ww))
    (set ww bool)
    (setq ![flaghis!] (cons (cons w wo) ![flaghis!]))))

% Makes *SWITCH from SWITCH ...
(de makeswvar!> (w)
  (incom!>(cons '!* (explode2 w))))

% Tuning for TORSION ...
(de tunetorsion!> (bool)
  (cond ((and bool (null !*nonmetr)) % Result is Q but N=0
               (put '!#!R!I!C     '!=sidxl nil)
	       (put '!#!G!T       '!=sidxl nil)
	       (put '!#!T!D!I     '!=sidxl nil)
	       (put '!#!T!S!F!L   '!=sidxl nil)
               )
        ((and bool !*nonmetr) % Result is Q and N
               (put '!#!R!I!C     '!=sidxl nil)
	       (put '!#!G!T       '!=sidxl nil)
	       (put '!#!T!D!I     '!=sidxl nil)
	       (put '!#!T!S!F!L   '!=sidxl nil)
               )
	((null !*nonmetr) % Result is Q=0 and N=0
	       (put '!#!R!I!C     '!=sidxl '((s 1 2)))
	       (put '!#!G!T       '!=sidxl '((s 1 2)))
	       (put '!#!T!D!I     '!=sidxl '((s 1 2)))
	       (put '!#!T!S!F!L   '!=sidxl '((s 1 2)))
               )
	((null !*nonmetr) % Result is Q=0 but N
	       (put '!#!R!I!C     '!=sidxl nil)
	       (put '!#!G!T       '!=sidxl nil)
	       (put '!#!T!D!I     '!=sidxl '((s 1 2)))
	       (put '!#!T!S!F!L   '!=sidxl '((s 1 2)))
               )
	))

% Tuning for NONMETR ...
(de tunenonmetr!> (bool)
  (cond (bool % Result is N with arbitrary Q
	       (put '!#!R!I!C     '!=sidxl nil)
	       (put '!#!G!T       '!=sidxl nil)
               )
	(!*torsion % Result is N=0 but Q
               (put '!#!R!I!C     '!=sidxl nil)
	       (put '!#!G!T       '!=sidxl nil)
               )
	((null !*torsion) % Result N=0 and Q=0
	       (put '!#!R!I!C     '!=sidxl '((s 1 2)))
	       (put '!#!G!T       '!=sidxl '((s 1 2)))
               )
	))


%-----  Stop; command ----------------------------------------------------

(de stop!> nil !!stop!! )

%-----  Next; command ----------------------------------------------------

(de next!> nil !!next!! )

%-----  Pause; command ---------------------------------------------------

(de pause!> nil
  (proc(w)
    (cond (![pause!] (return t))
          (t (prin2 "Pausing ...") (terpri)
             (setq ![pause!] t)))
    (loop!> (setq w (runcom!> nil))
            (exitif (or (eq w !!stop!!) (eq w !!next!!))))
    (setq ![pause!] nil)
    (return w)))

%-----  Inverse ; command ------------------------------------------------

(de invi!> (lst)
  (prog (wa wb)
    (cond((null lst)(return nil)))
    (setq lst (memlist!> '!, lst))
    (cond((or (eq lst !!er!!) (not(eqn(length lst)2)) )
      (prog2(setq ![er!] 1100)(return !!er!!))))
    (setq wa (car lst))
    (setq wb (cadr lst))
    (cond((or (cdr wa) (cdr wb) (not(idp(car wa))) (not(idp(car wb))) )
      (prog2(setq ![er!] 1100)(return !!er!!))))
    (setq wa (car wa))
    (setq wb (car wb))
    (cond((or (and (not(flagp wa '!+fun)) (not(redgood!> wa)) )
              (and (not(flagp wb '!+fun)) (not(redgood!> wb)) ) )
      (prog2(setq ![er!] 1100)(return !!er!!))))
    (put wa 'inverse wb)
    (put wb 'inverse wa)
    (return t)))


%-----  Order, Factor, RemFac commands -----------------------------------

(de orfare!> (lst wt)
  (proc nil
    (cond((null lst)(return nil)))
    (setq lst(memlist!> '!, lst))
    (cond((eq lst !!er!!)
      (prog2 (setq ![er!] 2202) (return !!er!!))))
    (setq lst (mapcar lst 'translata!>))
    (cond((memq !!er!! lst) (return !!er!!)))
    (apply wt (list lst))))


%----- Substitutions calls -----------------------------------------------

(de  smatch!> nil 'match)

(de famatch!> nil
  (cond ((getd 'match00) 'match00)
        (t               'match  ) ))

(de slet!> nil
  (cond ((and (getd '!~let) (not(iscsl!>))) '!~let)
        (t                                  'let  ) ))

(de falet!> nil
  (cond ((getd 'let00)                      'let00)
        ((and (getd '!~let) (not(iscsl!>))) '!~let)
        (t                                  'let  ) ))

(de sclear!> nil
  (cond ((and (getd '!~clear) (not(iscsl!>))) '!~clear)
        (t                                    'clear  ) ))

(de faclear!> nil
  (cond ((and (getd '!~clear) (not(iscsl!>))) '!~clear)
        (t                                    'clear  ) ))

%-----  Clear ; command --------------------------------------------------

(de cleri!> (lst wt) %   wt=t clear   wt=nil for all clear
  (proc (w wa wss)
    (cond ((null lst) (return nil)))
    (setq lst (memlist!> '!, lst))
    (cond ((eq lst !!er!!)
      (prog2 (setq ![er!] 2202) (return !!er!!))))
    (while!> lst
      (setq wa (translata!>(car lst)))
      (cond((eq wa !!er!!) (return !!er!!))
           ((null wa)(prog2(setq ![er!] 8710)(return !!er!!))) )
      (setq w (cons wa w))
      (setq lst (cdr lst)))
    (setq w (reverse w))
    (cond ((null wt) % this is for all case returning (clear w)
      (return (list (faclear!>) (list 'quote w)))))
    (eval (list (sclear!>) (list 'quote w))) % making (clear w)
    (while!> w  % remembering
      (setq wss (list (sclear!>) (ncons(car w))))
      (setq ![sublist!] (delete wss ![sublist!]))
      (setq w (cdr w)))
    (return t)))


%-----  Let ; and Match ; commands ---------------------------------------

(de leti!>   (lst wt) (letmatchi!> lst wt t))
(de matchi!> (lst wt) (letmatchi!> lst wt nil))

% WW=T - Let, WW=NIL - Match
% WT=T - Execute (Let/Match command), WT=NIL - Form (For All command)
(de letmatchi!> (lst wt ww)
  (proc (w wa wl wr wss)
    (cond ((null lst) (return nil)))
    (setq lst (memlist!> '!, lst))
    (cond((eq lst !!er!!)
      (prog2 (setq ![er!] 2202) (return !!er!!))))
    (while!> lst
      (setq wa (seek1!> (car lst) '!=))
      (cond
	((null wa)(progn
	  (cond((not(eq (caar lst) '!S!o!l))
                 (prog2(setq ![er!] 8709)(return !!er!!))))
	  (setq wa (soltra!>(car lst)))
	  (cond((eq wa !!er!!)(return !!er!!)))
	  (setq w (cons wa w))))
        ((or(null(car wa))(null(cdr wa)))
          (prog2(setq ![er!] 8709)(return !!er!!)))
	(t(progn
            (setq wl (translata!>(reverse(car wa))))
            (setq wr (translate!>(cdr wa)))
            (cond((or(eq wl !!er!!)(eq wr !!er!!)) (return !!er!!))
                 ((null wl) (prog2(setq ![er!] 8710)(return !!er!!)))
                 ((and wr(not(zerop(car wr))))
                   (prog2(setq ![er!] 8711)(return !!er!!))))
            (setq w (cons (list 'equal wl (cond(wr(cdr wr))(t 0))) w)))))
      (setq lst (cdr lst)))
    (setq w (reverse w))
    (cond((null wt) % for all case - returning
      (return (list (cond (ww (falet!>)) (t (famatch!>)))
                    (list 'quote w)))))
    % let/match case - executing
    (cond (ww (eval (list (slet!>)   (list 'quote w))))
          (t  (eval (list (smatch!>) (list 'quote w)))))
    (while!> w % remembering
      (setq wss (list (sclear!>) (ncons(cadar w))))
      (setq ![sublist!] (cons wss (delete wss ![sublist!])))
      (setq w (cdr w)))
    (return t)))

% Solution Translation ...
(de soltra!> (w)
  (cond((or (null(setq w (cdr w))) (cdr w)
            (atom(setq w (car w)))
	    (not(numberp(setq w (car w)))) )
	 (progn (doub!> '!S!o!l) (setq ![er!] 2020) !!er!!))
       (t(soltra1!> w))))

(de soltra1!> (wn)
  (cond((null ![sol!]) (prog2 (setq ![er!] 2113) !!er!!))
       (t(proc (w wnn)
	   (setq wnn wn)
	   (setq w ![sol!])
	   (while!> (and w (not(zerop wn)))
	     (setq w (cdr w))
	     (setq wn (sub1 wn)))
	   (cond
             ((or(null w)(not(zerop wn)))
	       (progn(doub!> wnn)(setq ![er!] 2114)(return !!er!!)))
	     ((null(car w))
	       (progn(setq ![er!] 2115)(return !!er!!))))
	   (return(mapcar (car w) 'nz!>))))))


%-----  For ...; commands ------------------------------------------------

(de forinstrs!> (lst)
  (cond
    ((null lst) nil)
    ((eqs!> (car lst) 'all) (foralli!> (cdr lst))) % word!!! for all ...
    ((memqs!> 'print lst) (proc (w)                % word!!! for...print...
       (while!> (not(eqs!> (car lst) 'print))      % word!!!
	 (setq w (cons(car lst)w))(setq lst(cdr lst)))
       (cond((null(cdr lst))
	 (prog2(setq ![er!] 6042)(return !!er!!))))
       (return(printi!>(append (cdr lst)
			       (cons 'for          % word!!!
				     (reverse w)))))))
    (t(prog2(setq ![er!] 6042) !!er!!))))


%-----  For All...; command ----------------------------------------------

(de foralli!> (lst)
  (proc (w wt wa wss w1 w2 w3)
    (cond((null lst)(return nil))
         ((memqs!> 'let lst)(setq wt 'let))     % word!!!
	 ((memqs!> 'match lst)(setq wt 'match)) % word!!!
         ((memqs!> 'clear lst)(setq wt 'clear)) % word!!!
         (t(prog2(setq ![er!] 8712)(return !!er!!))))
    (while!> lst
      (exitif (eqs!> wt (car lst)))
      (setq wa(cons(car lst)wa))
      (setq lst(cdr lst)))
    (cond((or(null lst)(null(cdr lst))(null wa))
      (prog2(setq ![er!] 8713)(return !!er!!))))
    (setq lst (cdr lst))
    (cond((memqs!> 'such wa)(progn % word!!!
      (setq wa (reverse wa))
      (setq w3 (seek1q!> wa 'such)) % word!!!
      (cond((or (null(car w3)) (null(cdr w3)) (null(cddr w3))
		(not(eqs!> (cadr w3) 'that))) % word!!!
	     (prog2(setq ![er!] 8712)(return !!er!!))))
      (setq wa (car w3))
      (setq w3 (cddr w3))  )))
    (setq wa(memlist!> '!, wa))
    (cond((eq wa !!er!!)
      (prog2 (setq ![er!] 2202) (return !!er!!))))
    (while!> wa
      (cond((or(cdar wa)(not(idp(caar wa))))
        (prog2 (setq ![er!] 8714) (return !!er!!))))
      (setq w (cons(caar wa)w))
      (setq wa (cdr wa)))
    (setq w1 w)
    (while!> w1
      (cond((not(flagp (car w1) '!+grgvar))
	(setq w2 (cons(car w1)w2))))
      (setq w1 (cdr w1)))
    (flag w '!+grgvar)
    (cond((null w3)(setq w3 t))
	 (t(progn
	     (setq w3 (booltra!> w3))
	     (cond((eq w3 !!er!!)(return !!er!!))))))
    (setq wa
      (cond((eq wt 'let)   (leti!> lst nil))     % not words
           ((eq wt 'match) (matchi!> lst nil))   % not words
           (t              (cleri!> lst nil))))
    (cond((eq wa !!er!!)
      (prog2(remflag w2 '!+grgvar)(return !!er!!))))
    (errorset (list 'forall (list 'quote (list w w3 wa)))
              ![erst1!] ![erst2!] )
    (remflag w2 '!+grgvar)
    (setq wa (cadadr wa))
    (cond((not(eqs!> wt 'clear)) (setq wa (mapcar wa 'cadr)))) % not word
    (while!> wa
      (setq wss (list 'forall
                  (list w w3
                    (list (faclear!>)
                      (list 'quote (ncons(car wa)))))))
      (setq ![sublist!] (delete wss ![sublist!]))
      (cond((not(eq wt 'clear)) % not word
	(setq ![sublist!] (cons wss ![sublist!]))))
      (setq wa (cdr wa)))
    (return t)))

%-----  Print...; command ------------------------------------------------

(de printi!> (lst)
  (prog (wi)
    (cond ((null lst) (return nil)))
    (setq ![modp!] ![umod!])
    (cond ((not(and (fancyon!>) (not !*latex))) (terpri)))
    (cond ((memqs!> 'for lst) (progn  % word!!!
      (setq lst (seek1q!> lst 'for))  % word!!!
      (setq wi (cdr lst))
      (setq lst (reverse(car lst))))))
    (cond ((null lst) (return nil)))
    (cond(wi(setq wi (memlist!> '!, wi))))
    (cond((eq wi !!er!!)(prog2(setq ![er!] 2202)(return wi))))
    (cond(wi(setq wi (itercon!> wi))))
    (cond((eq wi !!er!!)(prog2(setq ![er!] 21031)(return wi))))
    (setq ![allzero!] t)
    (setq ![extvar!] (mapcar wi 'caar))
    % This with prohibited unknown vars -> for
    % (setq lst (pretrans!> lst)) % Pre Translation ...
    % This with allowed unknown vars -> for
    (setq lst (pretransext!> lst)) % pre translation ...
    (cond ((and ![extvara!] !*nofreevars)
	     (mapcar ![extvara!] 'doub!>)
             (setq ![er!] 2018)
	     (setq ![extvara!] nil)
             (return !!er!!))
          ((and ![extvara!]
                (not(and (eqn (length ![extvara!]) 1)
			 (equal (list 'dummyvar!> (car ![extvara!])) lst))))
             (setq wi (mapcar ![extvara!] 'ncons))
	     (setq wi (mapcar wi 'ncons))
	     (setq ![extvar!] ![extvara!])
             (setq ![extvara!] nil) ))
    (cond ((eq lst !!er!!) (return !!er!!)))
    (setq lst (printico!> wi nil lst nil))
    (cond((eq lst !!er!!)(return !!er!!)))
    (cond
      (![allzero!]
        (progn (alpri!> nil)
	       (grgend!>)
               (grgterpri!>) (terpri)))
      ((and (not !*latex) (fancyon!>)) (terpri)))
    (return t)))

(de appendn!> (wa wd)
  (cond((null wa) wd)
       (t(cons(ncons(car wa))(appendn!>(cdr wa)wd)))))

(de printico!> (wi wt lst wp)
  (cond
    ((null wi) (progn
      (setq lst (fintrans!> lst)) % final translation
      (cond((eq lst !!er!!) !!er!!)
           ((null lst) nil)
           (t(progn (setq ![allzero!] nil)
               (cond(wt(prinvarl!>(reverse wt))))
	       (cond(!*math(gprin!> "(")))
               (cond
                 ((zerop(car lst)) (alpri!> (cdr lst))) % algexpr
                 (t (dfpri!> (cdr lst) (car lst))))     % form
	       (cond(!*math(gprin!> ")")))
	       (cond((ifmodo!>)(ooend!>)))
               (grgterpri!>)
               (cond((not(and (fancyon!>) (not !*latex))) (terpri)))
               )))))
     (t(proc (wa we)
        (setq wa (errorset!> (list3 'itertr!> (list2 'quote (car wi))
                                           (list2 'quote wp) )
                          ![erst1!] ![erst2!]))
        (cond((atom wa)(prog2(setq ![er!] wa)(return !!er!!)))
             (t(setq wa(reverse(car wa)))))
        (while!> wa
          (put (caar wa) '!=subind (cdar wa))
          (setq we (printico!> (cdr wi) (cons(cdar wa)wt) lst (cdar wa)))
          (remprop (caar wa) '!=subind)
          (cond((eq we !!er!!)(return we)))
          (setq wa (cdr wa)))))))

(de prinvarl!> (w)
  (proc (wr we)
    (cond (!*math              (setq wr '( !(!*  )))
	  (!*macsyma           (setq wr '( !/!*  )))
	  (!*maple             (setq wr '( !#!   )))
	  ((or !*grg !*reduce) (setq wr '( !%    ))))
    (setq we ![extvar!])
    (while!> w
      (setq wr (cons(car w)(cons '!= (cons(car we)wr))))
      (setq w (cdr w))
      (cond((and w (fancyon!>)) (setq wr (cons '!,  wr))))
      (setq we (cdr we)))
    (setq wr (cons
      (cond (!*math      '!*!)   )
	    (!*macsyma   '!*!/   )
	    (!*grg       '!;!    )
	    ((fancyon!>) '!:     )
	    % ((fancyon!>) '!:!\!     )
	    (t           '!:!    ))  wr))
    (setq wr (reverse   wr))
    (cond((ifmodo!>) (prog2(gprinwb!> wr)(gterpri!>)))
	 (t (algprinwb!> wr)))
    (cond ((fancyon!>) (algpri!> " ")))
    ))

(de itercon!> (lst)
  (proc (w wc)
    (while!> lst
      (setq wc (car lst))
      (setq lst (cdr lst))
      (cond((or(memq '!< wc)(memq '!> wc)(memq '!<!= wc)(memq '!>!= wc))
	     (progn (setq wc (itercon1!> wc))
		    (cond((eq wc !!er!!)(return !!er!!)))
		    (setq w (append wc w)) ))
	   (t(setq w (cons(ncons wc)w)))))
   (return(reversip w))))

(de itercon1!> (lst)
  (proc (w wc wa)
    (while!> lst
      (cond
        ((memq (car lst) '(!< !> !<!= !>!=))
          (cond((or(null(cdr lst))(null wa))(return !!er!!))
	       (t(progn (setq w (cons (cons(reverse wa)wc) w))
		        (setq wa nil)
		        (setq wc (itcty!>(car lst)))
                        (setq lst (cdr lst)) ))))
        (t(prog2(setq wa (cons(car lst)wa))
                (setq lst (cdr lst))))))
    (setq w (cons (cons(reverse wa)wc) w))
    (return w)))

(de itcty!> (w)
  (cond
    ((eq w '!<) 1)
    ((eq w '!>) 2)
    ((eq w '!<!=) 3)
    ((eq w '!>!=) 4)))


%----- Comment ... command -----------------------------------------------

(de comment!> (lst)
  (cond (![unl!] (progn
	   (wrs ![unl!])
           (print '(cout!>)) (terpri)
           (print (list 'comin!> (list 'quote lst))) (terpri)
	   (wrs ![wri!])  ))
        (t nil)))


%-----  Zero/Nullify command ----------------------------------------------

(de zero!> (lst) % 05.96
  (proc (w wc)
    (cond ((null lst) (return nil))
          ((eqs!> lst '(time)) (progn % word!!!
             (setq ![time!] (time))
             (setq ![gctime!] (gctime))
             (return nil))))
    (cond ((eq (setq w (dgood!> lst)) !!er!!) (return !!er!!)))
    (setq w (altdata!> w))
    (while!> w
      (setq wc (car w))
      (cond ((not (memq wc '(![cord!] ![const!] ![fun!] ![sol!] ![apar!] )))
        (cond
          ((eq wc '!#!G)  (setq ![mtype!]  3) (setq ![dtype!]  1) )
          ((eq wc '!#!G!I) (setq ![mitype!] 3) (setq ![ditype!] 1) )
          ((eq wc '!#!T)  (setq ![ftype!]  3) )
          ((eq wc '!#!D)  (setq ![fitype!] 3) ) )
        (set wc (mkbox!> wc))))
      (setq w (cdr w)))))


%----- Forget ; command --------------------------------------------------

(de forget!> (lst)
  (proc (w)
    (cond ((null lst) (return nil))
          ((eq (setq w (dgood!> lst)) !!er!!) (return !!er!!)))
    (setq w (altdata!> w))
    (while!> w
      (cond ((flagp (car w) '!+abbr) (forget1!>(car w)))
            (t (msg!> 8701)))
      (setq w (cdr w)))))

(de forget1!> (w)
  (prog (wa wb wl)
    (cond
      ((flagp w '!+abbr) (prog2
        (setq wb ![abbr!])
        (setq ![abbr!]
          (loop!>
            (cond ((eq w (car wb)) (return (app!> wa (cdr wb))))
                  (t(prog2 (setq wa (cons (car wb) wa))
                           (setq wb (cdr wb))))))))))
%   (setplist w nil)     % AMI: removes ALL properties and flags
    (remprop w 'vartype) % PSL: removes GLOBAL/FLUID
    (setq wl (ncons w))
    (set w nil)
    (foreach!> x in ![allflags!] do (remflag wl x))
    (foreach!> x in ![allprops!] do (remprop w x))
    ))

%-------- Hold/Relese; ---------------------------------------------------

(de hold!> (lst wt)
  (prog (w)
    (cond ((null lst) (return nil))
          ((eq (setq w (dgood!> lst)) !!er!!) (return !!er!!)))
    (setq w (altdata!> w))
    (cond (wt (flag    w '!+hold))
	  (t  (remflag w '!+hold)))
    (return t)))

%---------- Erase/Delete; -----------------------------------------------

(de erase!> (lst) % 5.96
  (proc (w wc)
    (cond ((null lst) (return nil))
          ((eq (setq w (dgood!> lst)) !!er!!) (return !!er!!)))
    (setq w (altdata!> w))
    (while!> w
      (setq wc (car w))
      (cond ((and ![umod!] (memq wc '(!#!b !#!e))) (msg!> 7012))
            ((eq wc '![cord!])
               (rempf!> ![rpflcr!] nil) (setq ![cord!] nil))
            ((eq wc '![const!])
               (rempf!> ![rpflcn!] nil) (setq ![const!] nil))
            ((eq wc '![apar!])
               (rempf!> ![rpflap!] '(2)) (setq ![apar!] nil))
            ((eq wc '![fun!])
               (rempf!> ![rpflfu!] '(1)) (setq ![fun!] nil)
                                      (setq ![gfun!] nil) )
            (t (set wc nil)))
      (cond
        ((eq wc '!#!G)  (setq ![mtype!]  nil) (setq ![dtype!]  nil) )
        ((eq wc '!#!G!I) (setq ![mitype!] nil) (setq ![ditype!] nil) )
        ((eq wc '!#!T)  (setq ![ftype!]  nil) )
        ((eq wc '!#!D)  (setq ![fitype!] nil) ) )
      (setq w (cdr w)) )
    (return t)))

%----- New Commands Driver -----------------------------------------------

(de newcommands!> (w)
  (cond ((null w) nil)
	((eqs!> (car w) 'coordinates) (chcoord!> (cdr w))) % word!!!
	((eqs!> (car w) 'object)      (obdec!> (cdr w) 0)) % word!!!
	((eqs!> (car w) 'equation)    (obdec!> (cdr w) 1)) % word!!!
	((eqs!> (car w) 'connection)  (obdec!> (cdr w) 2)) % word!!!
	(t                            (obdec!> w 0))))


%----- Show Commands Driver ----------------------------------------------

(de shcommands!> (w)
   (cond ((null w) nil)
         ((eqs!> w '(time))       (timei!>))    % word!!!
         ((eqs!> w '(status))     (shstatus!>)) % word!!!
         ((eqs!> w '(all))        (shall!>))    % word!!!
         ((eqs!> w '(gc time))    (gctime!>))   % word!!!
         ((eqs!> (car w) 'switch) (sflag!> (cdr w)))   % word!!!
         ((eqs!> (car w) 'file)   (showfil!> (cdr w))) % word!!!
	 ((memq '!* w)            (shallbuilt!> w))
	 ((stringp (car w))       (showfil!> w))
         ((and (null(cdr w)) (idp(car w))
               (or (flagp (idtostcase!> (car w)) 'switch)
                   (flagp (idtostcase!> (car w)) '!+switch)))
                                  (sflag!> w))
         (t                       (showobj!> w))))

%----- Show Object -------------------------------------------------------

(de showobj!> (lst)
  (proc (w)
    (cond ((null lst) (return nil))
          ((eq (setq w (dgoodw!> lst)) !!er!!) (return !!er!!)))
    (setq w (altdata!> w))
    (cond ((null w) (return nil)))
    (while!> w
      (cond ((memq (car w) '(![cord!] ![const!] ![fun!] ![sol!] ![apar!]))
               nil )
            (t (shobj1!> (car w))))
      (setq w (cdr w)) )
    (terpri)
    (return t)))

(de shobj1!> (w)
  (prog (wi wt wy ww wc wd wx)
    (terpri)
    (setq wi (get w '!=idxl))
    (setq wt (gettype!> w))
    (setq wy (get w '!=sidxl))
    (setq ww (get w '!=way))
    (setq wd (get w '!=dens))
    (gprinreset!>)
    (setq ![gptab!] 2)
    % Name ...
    (cond ((not(or (flagp w '!+abbr) (flagp w '!+macros2))) (thepn!> w)))
    % ID ...
    (gprin!> (incom!>(cdr(explode2 w))))
    % Indices ...
    (while!> wi
      (setq wc (car wi))
      % Position ...
      (cond
	((and (upperp!> wc) (holp!> wc)) (gprin!> "^"))
	((upperp!> wc)                   (gprin!> "'"))
	((holp!> wc)                     (gprin!> "_"))
	(t                               (gprin!> ".")))
      % Type ...
      (cond
	((holp!> wc)  (gprin!>(car ![wh!])) (setq ![wh!] (cdr ![wh!])))
	((tetrp!> wc) (gprin!>(car ![wf!])) (setq ![wf!] (cdr ![wf!])))
	((enump!> wc) (gprin!>(car ![wi!])) (setq ![wi!] (cdr ![wi!]))
		      (cond ((cdr wc) (gprin!> (cdr wc)))
			    (t        (gprin!> "dim"))))
	((spinp!> wc) (for!> x (1 1 (cdr wc)) do (progn
			(gprin!>(car ![ws!]))
                        (setq ![ws!] (cdr ![ws!]))))))
      (cond ((dotp!> wc) (gprin!> "~")))
      (setq wi (cdr wi)))
    (gpris!>)
    % Type ...
    (gprin!> (cond((flagp w '!+pl) "are")(t "is")))
    (gpris!>)
    (cond ((eqn wt -1) (gprin!> "Vector"))
          ((eqn wt 0)  (gprin!> "Scalar"))
          (t (gprin!> wt) (gprin!> "-form")))
    (cond ((flagp w '!+equ)   (gpris!>) (gprin!> "Equation"))
	  ((flagp w '!+fconn) (gpris!>)
             (gprils0!> '("Frame" "Connection")))
	  ((flagp w '!+hconn) (gpris!>)
             (gprils0!> '("Holonomic" "Connection")))
	  ((flagp w '!+uconn) (gpris!>)
             (gprils0!> '("Spinor" "Connection")))
	  ((flagp w '!+dconn) (gpris!>)
             (gprils0!> '("Conjugate" "Spinor" "Connection")))
	  ((flagp w '!+macros2) (gpris!>)
             (gprils0!> '("Macro" "Object")))
	  (wd (gpris!>)
	      (gprin!> "Density")
	      (gpris!>)
	      (cond ((car wd) (gprin!> "sgnD") (setq wx t)))
	      (cond ((cadr wd) (cond (wx (gprin!> "*")))
			       (setq wx t)
			       (gprin!> "D")
			       (cond ((not(eqn (cadr wd) 1))
                                 (gprin!> "^")
				 (cond ((lessp (cadr wd) 0) (gprin!> "(")))
				 (gprin!> (cadr wd))
				 (cond ((lessp (cadr wd) 0) (gprin!> ")")))
				 ))))
	      (cond ((caddr wd) (cond (wx (gprin!> "*")))
				(setq wx t)
                                (gprin!> "sgnL")))
	      (cond ((cadddr wd) (cond (wx (gprin!> "*")))
			       (gprin!> "L")
			       (cond ((not(eqn (cadddr wd) 1))
                                 (gprin!> "^")
				 (cond ((lessp (cadddr wd) 0) (gprin!> "(")))
				 (gprin!> (cadddr wd))
				 (cond ((lessp (cadddr wd) 0) (gprin!> ")")))
				 ))))
              ))
    (gterpri!>)
    % Value ...
    (cond ((flagp w '!+macros2) nil)
          ((eval w) (gprin!> "Value: known")   (gterpri!>))
	  (t        (gprin!> "Value: unknown") (gterpri!>)))
    % Symmetries ...
    (cond((null wy) (go lab1)))
    (gprinreset!>) (gprin!> "  ")
    (setq ![gptab!] 4)
    (gprin!> "Symmetries:")
    (gpris!>)
    (while!> wy
      (shsy!>(car wy))
      (cond((cdr wy) (prog2 (gprin!> ",") (gpris!>))))
      (setq wy (cdr wy)))
    (gterpri!>)
    lab1
    % Ways of calculation ...
    (setq ww (allways!> ww))
    (cond ((null ww) (go lab2)))
    (gprinreset!>) (gprin!> "  ")
    (setq ![gptab!] 4)
    (gprin!> "Ways of calculation:")
    (gterpri!>)
    (while!> ww
      (gprinreset!>)
      (setq ![gptab!] 6)
      (gprin!> "    ")
      (setq wc (car ww))
      (gprils!> (lowertxt!>(car wc)))
      (setq wc (cdr wc))
      (gprin!> "(")
      (while!> wc
	(gprin!> (incom!> (cdr (explode2
	  (cond ((pairp(car wc)) (cadar wc)) (t (car wc))) ))))
	(cond((pairp(car wc)) (gprin!> "*")))
	(cond ((cdr wc) (gprin!> ",")))
	(setq wc (cdr wc)))
      (gprin!> ")")
      (gterpri!>)
      (setq ww (cdr ww)))
    lab2
    (gprinreset!>)))

(de shsy!> (w)
  (cond ((numberp w) (gprin!> w))
	((idp w) (gprin!> (tolc!> w)))
	((idp(car w)) (prog2 (shsy!>(car w)) (shsy!>(cdr w))))
	(t(proc nil
	    (gprin!> "(")
	    (while!> w
	      (shsy!> (car w))
	      (cond((cdr w) (gprin!> ",")))
	      (setq w (cdr w)))
	    (gprin!> ")") ))))

(de allways!> (ww)
  (proc (wr w)
    (while!> ww
      (cond((not(eval(cadar ww))) (setq wr (cons (car ww) wr))))
      (setq ww (cdr ww)))
    (setq ww nil)
    (while!> wr
      (setq w (needdata!>(cdddar wr)))
      (setq w
        (cons (cond((null(caar wr)) '( "Standard way" )) (t(caar wr))) w))
      (setq ww (cons w ww))
      (setq wr (cdr wr)))
    (return ww)))

(de needdata!> (w)
  (cond ((null w) nil)
	((atom (car w))  (cons (car w) (needdata!> (cdr w))))
	((eq (caar w) t) (cons (car w) (needdata!> (cdr w))))
	((eval (caar w)) (append (cdar w) (needdata!> (cdr w))))
	(t (needdata!> (cdr w)))))


%----- Time; and GC Time; commands ---------------------------------------

(de timei!> nil
  (prog (wt wgt)
    (setq wt  (difference (time) ![time!]))
    (setq wgt (difference (gctime) ![gctime!]))
    (cond ((iscsl!>) (setq wt (plus wt wgt))))
    (cond ((not(eqn wt 0)) (setq wgt (quotient (times 100 wgt) wt)))
          (t               (setq wgt 0)))
    (prin2 "Time: ")
    (prtime!> wt)
    (cond ((zerop wt) (prog2 (terpri) (return nil))))
    (prin2 " (")
    (prin2 wgt)
    (prin2 "%GC)")
    (terpri)))

(de gptime!> nil
  (prog (wt wgt)
    (setq wt (difference (time) ![time!]))
    (cond ((iscsl!>) (setq wgt (difference (gctime) ![gctime!]))
                     (setq wt (plus wt wgt))))
    (gprtime!> wt)
    (gterpri!>)))

(de gctime!> nil
  (progn (prin2 "Garbage collections time: ")
         (prtime!> (difference (gctime) ![gctime!])) (terpri)))

(de prtime!> (w)
   (prog (wa wb)
     (setq wb (quotient (remainder w 1000) 10))
     (setq wa (quotient w 1000))
     (prin2 wa)(prin2 ".")
     (cond((lessp wb 10)(prin2 "0")))
     (prin2 wb)
     (prin2 " sec")))

(de gprtime!> (w)
   (prog (wa wb wt)
     (setq wb (quotient (remainder w 1000) 10))
     (setq wa (quotient w 1000))
%     (gprin!> wa)(gprin!> ".")
%     (cond((lessp wb 10)(gprin!> "0")))
%     (gprin!> wb)
%     (gprin!> " sec")
     (setq wt '(!  !s !e !c !"))
     (setq wt (append (explode2 wb) wt))
     (cond((lessp wb 10) (setq wt (cons '!0 wt))))
     (setq wt (cons '!. wt))
     (setq wt (append (explode2 wa) wt))
     (setq wt (cons '!" wt))
     (gprin!>(compress wt))
))



%----- Find/Calculate ; command ------------------------------------------

(de find!> (lst)
  (proc (w wa wss)
    (cond ((null lst) (return nil)))
    (setq w (byfrom!> lst))
    (cond ((eq w !!er!!) (return !!er!!)))
    (setq wss w)
    (cond ((eq(setq w (dgoodw!> w)) !!er!!) (return !!er!!)))
    (setq w (altdata!> w))
    (while!> w
      (cond
	((flagp (car w) '!+macros2)
	  (doubo!>(car w)) (msg!> 100) (setq w (cdr w)))
        ((null(eval(car w))) (progn
          (setq ![chain!] nil)
          (setq wa (request!>(car w)))
          (cond((eq wa !!er!!)
                 (prog2(trsf!>(car w))(return !!er!!)))
	       ((null wa)
                 (progn(setq ![er!] 6046)(trsf!>(car w))(return !!er!!))))
          (setq w (cdr w))))
        (t (aexp!>(car w)) (setq w (cdr w)))))
    (return t)))

% Way extraction ...
(de byfrom!>(w)
  (proc(wa) (setq ![way!] nil)
    (while!>(and w (not(bftp!>(car w))))
      (prog2(setq wa(cons(car w)wa))(setq w(cdr w))))
    (cond((or(null wa)(and w(null(cdr w))))
           (progn(setq ![er!] 6042)(return !!er!!)))
         (w(prog2(setq ![way!] w)(return(reverse wa))))
         (t(prog2(setq ![way!] nil)(return(reverse wa)))))))


%---------- Write ...; command -------------------------------------------

(de write!> (lst)
  (proc (w wa wc)
    (cond ((null lst) (return nil)))
    (setq w (tofile!> lst 'write))
    (cond((eq w !!er!!) (return !!er!!))
         ((null w)  % here ends global write to...; command
           (progn (closewrite!>) % close old global file ..
		  (setq ![wri!] ![lwri!])
		  (setq ![lwri!] nil)
		  (wrs ![wri!])
                  (return t)))
         (t(progn (setq wc (cdr w)) (setq w (car w))))) % wc=t write...to...;
    (cond((eq (setq w (dgoodw!> w)) !!er!!)
           (progn (cond(wc(closelw!>)))
                  (return !!er!!))))
    (cond (wc(wrs ![lwri!])))
    (setq w (altdata!> w))
    (while!> w
      (cond((memq (car w) '(!#!b !#!e)) (setq ![modp!] nil))
           (t (setq ![modp!] ![umod!])))
      (setq wa (dtl!> (car w)))
      (cond((eq wa !!er!!) (progn (cond(wc(closelw!>)))
                                  (return !!er!!))))
      (setq w(cdr w)))
    (cond (wc(closelw!>))) % closing if it is write..to...; command
    (return t)))

(de closelw!> nil
  (progn (close ![lwri!])
	 (setq ![lwri!] nil)
	 (wrs ![wri!]) ))

% Write ; commands for different data types 27.12.90

% General write: if =DATL call special function otherwise Standard ...
(de dtl!> (w)
  (cond ((get w '!=datl) (apply 'eval (get w '!=datl)))
        (t (datlt!> w))))

% The Standard form of Write command ...
(de datlt!> (wn)
  (proc (lst w)
    (cond ((flagp wn '!+macros2) (setq lst (prepmac!> wn)))
	  (t                     (setq lst (eval wn))))
    (cond ((null lst) (prog2 (abse!> wn) (return nil))))
    (gprinreset!>) (thepn0!> wn) (gprin!> ":") (gterpri!>)
    (cond % write as a matrix ...
      ((and !*wmatr (not(ifmodo!>))
            (zerop(gettype!> wn))
	    (eqn (length(get wn '!=idxl)) 2))
	 (setq ![allzero!] nil)
	 (alpri!>(cons 'mat lst))
	 (algterpri!>)
	 (go lab)))
    (cond ((not(and (fancyon!>) (not !*latex))) (terpri)))
    (setq ![idwri!] (incom!>(cdr(explode2 wn))))
    (setq ![allzero!] t)
    (allcom!> lst wn nil (cond ((setq w (get wn '!=idxl)) w)
                               (t '(0)))
                         (function printco!>))
    lab
    (cond
      (![allzero!]
        (progn (cond ((flagp wn '!+equ) (eqpri!> nil nil 0))
                     (t                 (alpri!> nil)))
               (grgend!>)
               (grgterpri!>) (terpri)))
      ((and (not !*latex) (fancyon!>)) (terpri)))
    ))

% Prepare values for Macro tensor ...
(de prepmac!> (wn)
  (prog (wr)
    (setq wr (errorset (list 'require!> (list 'quote (get wn '!=ndl))
                       nil nil)))
    (cond ((atom wr) (return nil)))
    (setq wr (mkbox!> wn))
    (setq wr (allcoll!> wr wn nil
                        (cond((get wn '!=idxl) (get wn '!=idxl))
                              (t '(0)))
                        (function prepmac0!>)))
    (return wr)))

(de prepmac0!> (w wi wn)
  (cond ((syaidxp!> wi (get wn '!=sidxl))
	  (setq w (eval (cons (get wn '!=evf) wi)))
	  (cond ((eqn (gettype!> wn) 0) (evalalg!> w))
		(t (evalform!> w))))
	(t nil)))


% One component printing ...
(de printco!> (we wi wn)
  (prog (wq)
    (cond((null we)(return nil)))
    (setq ![allzero!] nil)
    (setq wq (flagp wn '!+equ))      % equation
    (idwri!> wn wi)                  % write identifier
    (wriassign!> wq)                 % write =
    (prel!> we (gettype!> wn) wq)  % write value
    (grgends!>)
    (grgterpri!>)
    (cond((not(and (fancyon!>) (not !*latex))) (terpri)))
    ))

(de idwri!> (wn wi)
 (cond
  ((fancyon!>) (prog (wa w ww wc wss)
    (setq wc 0)
    (cond
      ((setq wa (get wn '!=idxl))
	(setq wss (needspace!> wa)) % we need extra space between indices?
        (foreach!> x in wi do
	  (progn
	    (setq wc (add1 wc))
	    % index ...
	    (cond
              ((holonomq1!>(car wa))
                 (setq w (getel1!> ![cord!] x)))
	      (t (setq w '( !" ))
	         (cond ((dotp!>(car wa))
                    (setq w (cons (cond (!*latex '!}) (t '!')) w))))
	         (setq w (append (explode2 x) w))
	         (cond ((and (dotp!>(car wa)) !*latex)
                    (setq w (append '(!\ !d !o !t !{) w))))
	         (setq w (cons '!" w))
	         (setq w (compress w))))
	    % place to put index ...
	    (cond((eqn wc 1) (setq ww (fancyidwri!> wn)))
		 (t          (setq ww '!#!#lr)))
	    (cond ((and wss (not(eqn wc 1))) (algpri!> "\,")))
	    (cond
              ((or (upperp!>(car wa)) (eq wn '!#b))
                (algpri!> (list 'expt ww w) ))
	      (t(progn
		(flag (ncons ww) 'print!-indexed)
                (algpri!> (list ww w) )
		(remflag (ncons ww) 'print!-indexed))))
	    (setq wa (cdr wa)))))
      (t (algpri!> (fancyidwri!> wn) ))  )))
  ((ifmodo!>) (ooelem!> ![idwri!] wi))
  (t(prog (wa wp wss wl wx)
    (algpri!> ![idwri!] )
    (cond((setq wa (get wn '!=idxl))
	(setq wss (needspace!> wa)) % we need extra space between indices?
        (foreach!> x in wi do
          (progn
	    (setq wx
              (cond ((holonomq1!>(car wa)) (getel1!> ![cord!] x))
		    (t x)))
	    (cond (wss (algpri!> " "))) % extra space
	    (cond (wss (setq wl (length(explode2 wx))))
		  (t (setq wl 1)))
	    % vertical position ...
            (setq wp (cond
		       ((enump!>(car wa))  0)                     % enum
		       ((and (upperp!>(car wa)) (dotp!>(car wa))) % upper dot
			  (setq ymax!* 2) 1)
		       ((upperp!>(car wa))                        % upper
			  (setq ymax!* 1) 1)
		       (t (setq ymin!* -1) -1)))                  % lower
	    % drawing index itself ...
            (setq pline!* (cons
              (cons (cons (cons posn!* (plus wl posn!*))
                          wp)
                    wx)
              pline!*))
	    % dot for dotted index ...
            (cond ((dotp!>(car wa))
              (setq pline!* (cons
                (cons (cons (cons posn!* (add1 posn!*))
                            (add1 wp))
                      ".")
                pline!*))))
            (setq posn!* (plus wl posn!*))
            (setq wa (cdr wa)) )))) ))))

(de needspace!> (wi)
  (cond ((null wi) nil)
	((holonomq1!>(car wi)) t)
	((greaterp (dimid!>(car wi)) 9) t)
	(t (needspace!> (cdr wi)))))

(de fancyidwri!> (wn)
  (prog (w)
    (setq w (get wn '!=tex))
    (cond
      (w(prog2
        (put wn 'fancy!-special!-symbol
             (cond ((and (pairp w) !*latex) (car w))
                   ((pairp w) (cdr w))
                   (t w)))
        (return wn)))
      (t(return ![idwri!])))))

% Expression or Equality printing ...
(de prel!> (we wt wq)
  (prog (wl wr)
    (cond(!*math(gprin!> "(")))
    (cond (wq (prog2
                (cond(we(prog2 (setq wl (cadr we))
                               (setq wr (caddr we)))))
                (eqpri!> wl wr wt)))
	  ((zerop wt) (alpri!> we))
	  (t          (dfpri!> we wt)))
    (cond(!*math(gprin!> ")")))  ))

% Special write for Constant and Coordinates ...
(de datlc!> (wa txt pl)
  (proc nil
    (cond((null wa)(progn(terpri)
                         (prin2 txt)
                         (cond (pl (prin2 " are absent."))
                               (t  (prin2 " is absent.")))
                         (terpri)
                         (return nil))))
    (prin2 txt)
    (prin2 ":")(terpri)(terpri)
    (gprinreset!>)
    (gprils0!> wa)
    (gterpri!>)(terpri)))

% Special write for Functions ...
(de funl!> nil
  (prog (w)
    (cond((null ![fun!])(progn
           (prin2 "Functions are absent.")(terpri)
           (return t))))
    (prin2 "Functions:")(terpri)(terpri)
    (gprinreset!>)
    (foreach!> x in ![fun!] do (progn
      (cond((setq w(get x '!=depend)) (gfnpri!> w))
           (t (gprin!> x)))
      (gprin!> '! )))
    (gterpri!>)(terpri)))

% Special write for Solutions ...
(de solwri!> nil
  (proc (w wn)
    (cond((null ![sol!])(progn
           (prin2 "Solutions are absent.")(terpri)
           (return t))))
    (prin2 "Solutions:")(terpri)
    (cond((not(and (fancyon!>) (not !*latex))) (terpri)))
    (setq w ![sol!])
    (setq wn 0)
    (while!> w
      (cond
        ((ifmodo!>) (ooelem!> '!S!o!l (ncons wn)))
        (t(progn
            (algpri!> "Sol(" )
            (algpri!> wn )
            (algpri!> ")" ) )))
      (wriassign!> t)
      (prel!> (car w) 0 t)
      (grgends!>)
      (grgterpri!>)
      (cond((not(and (fancyon!>) (not !*latex))) (terpri)))
      (setq wn (add1 wn))
      (setq w (cdr w)))
    (cond((and (fancyon!>) (not !*latex)) (terpri)))
    ))


%---------- Output ...; command ------------------------------------------

(de grgout!> (w) (write!> (cons '!> w)))


%---------- In "..."; command ------------------------------------------

(de from!> (lst)
  (proc (w wp)
    (cond ((null lst) (return nil))
          ((or(not(stringp(car lst))) (cdr lst))
            (prog2 (setq ![er!] 6301) (return !!er!!))))
    (setq w (grgopeninput!> (car lst)))
    (cond ((atom w) (prog2 (setq ![er!] 6321) (return !!er!!))))
    (setq w (car w))
    (rds w)
    (setq ![echo!] t)
    % (terpri)
    (setq wp (listok!> '( !$ )))
    (setq ![echo!] nil)
    % (terpri)
    (rds nil)
    (close w)
    (cond ((eq wp !!er!!) (return !!er!!)))
    (setq wp (collect!> wp))
    (cond ((eq wp !!er!!) (return !!er!!)))
    (setq wp (mapcar wp 'mklevel!>))
    (setq wp (mapcar wp 'car))
    % execute the commands ...
    (while!> wp
      (cond ((and (car wp) (eq (runcom!>(car wp)) !!stop!!))
              (return !!stop!!)))
      (setq wp (cdr wp)))
    (return t)))

%  Open file ...
%  WD - filename, WI - INPUT/OUTPUT, WB - UNLOAD/WRITE
(de rdsio!> (wd wi wb)
  (prog (w wf)
    (cond((not(stringp wd))(prog2(setq ![er!] 6301)(return !!er!!))))
    (setq w (errorset (list 'open wd(list 'quote wi)) nil nil))
    (cond((atom w)(prog2(setq ![er!] 6321)(return !!er!!))))
    (cond
      % input file for load ...
      ((eq wi 'input)
        (prog2 (setq ![loa!] (car w)) (rds ![loa!])))
      % output file for write ...
      ((eq wb 'write) (setq ![lwri!] (car w)))
      % output file for unload ...
      ((eq wb 'unload) (setq ![lunl!] (car w)))
    )))


%---------- Unload ...; command ------------------------------------------

(de unl!> (lst)
  (proc (w wc wa)
    (cond ((null lst) (return nil)))
    (setq w (tofile!> lst 'unload))
    (cond((eq w !!er!!) (return !!er!!))
         ((null w) (progn % global unload file resetting and quit
	   (closeunload!>)
	   (setq ![unl!] ![lunl!])
	   (setq ![lunl!] nil)
           (return t))) % here ends unload to...; command
         (t(progn (setq wc (cdr w)) (setq w (car w)))))
    (setq wa w)
    (cond((eq (setq w (dgood!> w)) !!er!!)
            (prog2 (cond(wc(closelu!>))) (return !!er!!))))
    (cond (wc (wrs ![lunl!])) (t(wrs ![unl!]))) % directing output ...
    (print '(cout!>)) (terpri)
    (print (list 'sgn!> (list 'quote ![sgn!]))) (terpri)
    (setq w (altdata!> w))
    (cond ((and ![umod!] (eqs!> wa '(all))) (progn % word!!!
       (print '(smt!>)) (terpri)
       (setq w (append '(![dbas!] ![xb!] ![xv!]
                         ![xf!] ![ccb!] ![ccbi!]) w)))))
    (while!> w
      (cond ((and (eq (car w) '![cord!]) (null !*unlcord)) nil)
            ((get (car w) '!=unl)
               (apply 'eval (get (car w) '!=unl))
	       (cond ((and (eq (car w) '![fun!]) ![gfun!])
		 (print (list 'putgfun!> (list 'quote ![gfun!])))
                 (terpri))))
            (t(progn
               (cond ((flagp (car w) '!+abbr) (unlnvar!>(car w))))
               (print (list 'setq (car w) (list 'quote (eval(car w)))))
	       (terpri) )))
      (setq w (cdr w)))
    (print  '(rout!>)) (terpri)
    (cond (wc (closelu!>)) (t (wrs ![wri!]))) % restoring output ...
    (return t)))

(de closelu!> nil
  (progn (print t)
         (close ![lunl!])
	 (setq ![lunl!] nil)
	 (wrs ![wri!]) ))

% Unload new-built data ...
(de unlnvar!> (w)
  (proc (lst)
    (cond
      ((flagp w '!+abbr) (print (list 'pushabbr!> (list 'quote w)))
                         (terpri) ))
    (setq lst ![allflags!])
    (while!> lst
         (unlflag!> w (car lst))
         (setq lst(cdr lst)))
    (setq lst  ![allprops!])
    (while!> lst
         (unlprop!> w (car lst))
         (setq lst(cdr lst)))
    ))

% Unloads flag ...
(de unlflag!> (w wf)
  (cond ((flagp w wf)
    (print (list 'flag (list 'quote (list w)) (list 'quote wf)))
    (terpri) )))

% Unloads prop ...
(de unlprop!> (w wf)
  (prog (wa)
    (cond ((setq wa (get w wf))
     (print (list 'put (list 'quote w)
                       (list 'quote wf)
		       (list 'quote wa)))
     (terpri) ))))


%---------- Load ...; command --------------------------------------------

(de loa!> (lst)
  (proc (w wf we)
    (cond ((null lst) (return nil))
          ((eqs!> (car lst) 'package) % word!!!
             (return (loadpack!> (cdr lst) t)))
	  ((not(stringp(car lst)))
             (return (loadpack!> lst t))))
    (setq wf t)
    (cond ((cdr lst) (prog2(setq ![er!] 6301)(return !!er!!))))
    (setq lst (rdsio!> (car lst) 'input nil))
    (cond ((eq lst !!er!!) (return !!er!!)))
    (loop!>
      (setq w (errorset '(read) nil nil))
      (cond ((atom w) % unexpected data
               (progn (cload!>) (setq ![er!] 7720) (return !!er!!)))
            ((or (equal w '(t))
                 (equal w (ncons !$eof!$))
                 (atom w)) % eof encountered
              (progn (cload!>) (copar!>) (return t)))
           ((and wf (not (equal w '((cout!>))))) % not .loa file format
              (progn (cload!>) (setq ![er!] 7200) (return !!er!!))))
      (setq we (errorset (car w) nil nil))
      (cond ((atom we) % unexpected data
        (progn (cload!>) (setq ![er!] 7720) (return !!er!!))))
      (setq wf nil))
    ))

(de cload!> nil
  (progn
    (close ![loa!])
    (rds nil)
    (mtype!>)
    (mitype!>)
    (ftype!>)
    (fitype!>)
    ))

% Basis changing with Load ...
(de smt!> nil
  (prog2
    (setq ![umod!] t)
    (prin2 "Basis is anholonomic now.")
    (terpri)))

% Dimension/Signature control with Load ...
(de sgn!> (w)
  (cond
    ((not(equal w ![sgn!])) % signature diffres
      (cond
	(![firsti!] (setq ![sgn!] w)
		    (setq ![dim!] (length w))
		    (tunedim!>)
                    (sdimsgn!>) )
	(t (erm!> 7900) (err!> 7900))))))

% Load Comment ...
(de comin!> (lst)
  (progn (gprinreset!>)
         (gprils0!> (cons "%" lst))
         (gprin!> ";")
         (gterpri!>)
         ))


%----- Special Load/Unload for Fun, Cord and Const -----------------------

(dm putpnu!> (u) (list 'putpnu0!> (list 'quote (cdr u))))
(de putpnu0!> (u)
   (prog (w wc)
      (setq w '(putpn!>))
      (for!> x (0 1 1) do (progn  (setq wc (eval(car u)))
                                  (setq u (cdr u))
				  (setq w (cons (list 'quote wc) w))))
      (foreach!> x in u do (setq w (cons (list 'quote x) w)))
      (print(reverse w))
      (terpri) ))

(de putgfun!> (w)
  (progn
    (loadpack!> '(dfpart) nil)
    (generic!_function w)
    (cond (!*dfpcommute (dfp!_commute w)))))

(de putpn!> (wd w wf wp wss)
   (proc (wn wa)
     (cond((null w)(return nil)))
     (cond((and (eqn wss 1) !*unlcord)
            (progn (warcor!> w)
                   (rempf!> ![rpflcr!] nil)
                   (setq ![cord!] w)))
          ((eqn wss 1)(return nil))
          ((eqn wss 2)
            (prog2 (warcon!> w)
                   (setq w(setq ![const!](appmem!> w ![const!])))))
          ((eqn wss 3)(progn
                       (warfun!> w)
                       (setq wa(newid!> w ![fun!]))
                       (setq w(setq ![fun!](appmem!> w ![fun!])))
                       (operator wa)))
	  ((eqn wss 4) (setq ![apar!] w)
                       (foreach!> x in ![cord!] do (depend (cons x w)))) )
     (while!> wf
       (flag w (car wf))
       (setq wf(cdr wf)))
     (setq wn 0)
     (while!> w
       (cond(wp(put (car w) wp wn)))
       (setq wn(add1 wn))
       (setq w(cdr w)))
     (cond(wd(foreach!> x in wd do (progn
       (depend x)
       (flag (ncons(car x)) '!+grgvar)
       (put (car x) '!=depend x)  ))))
     ))

(de putfndp!> nil
  (prog (w wa)
    (foreach!> x in ![fun!] do
      (cond((setq wa(get x '!=depend))(setq w(cons wa w)))))
    (return w)))

(de warcor!> (w)
  (progn
    (cond((and ![cord!](not(equal w ![cord!]))) (msg!> 7630)))
    (cond((intersec!> w ![const!]) (msg!> 7635)))
    (cond((intersec!> w ![fun!]) (msg!> 7637))) ))

(de warcon!> (w)
  (progn
    (cond((intersec!> w ![cord!]) (msg!> 7631)))
    (cond((intersec!> w ![fun!]) (msg!> 7632))) ))

(de warfun!> (w)
  (progn
    (cond((intersec!> w ![cord!]) (msg!> 7633)))
    (cond((intersec!> w ![const!]) (msg!> 7634))) ))

(de intersec!> (wa wb)
  (cond((or(null wa)(null wb)) nil)
       ((memq(car wa)wb) t)
       ((memq(car wb)wa) t)
       (t(intersec!>(cdr wa)(cdr wb)))))

(de newid!> (w lst)
  (cond((null w) nil)
       ((not(memq(car w)lst))(cons(car w)(newid!>(cdr w)lst)))
       (t(newid!>(cdr w)lst))))

(de pushabbr!> (w)
  (prog2
    (cond((flagp w '!+abbr) (forget1!> w)))
    (setq ![abbr!] (consmem!> w ![abbr!]))))


%----- Unload/Write ... To/In file ---------------------------------------

(de tofile!> (lst wb) % wb=write/unload
  (proc(w)
    (while!>(and lst(not(memqs!> (car lst) '( !> to )))) % word!!!
      (setq w(cons(car lst)w))(setq lst(cdr lst)))
    (cond
      ((and lst(eqn(length lst)2))
        (progn
          (setq lst(rdsio!> (cadr lst) 'output wb))
          (cond((eq lst !!er!!)(return !!er!!)))
          (cond((null w)(return nil))            % just file...
               (t(return(cons(reverse w) t)))))) % file and data...
      (lst(prog2(setq ![er!] 6301)(return !!er!!)))
      (t(return(cons(reverse w) nil))))))        % just data...


%------ Show File "..."; command -----------------------------------------

(de showfil!> (lst)
  (proc (w wf wt wss wi wd wx)
    (cond((null lst)(return nil)))
    (setq wf t)
    (cond((cdr lst)(prog2(setq ![er!] 6301)(return !!er!!))))
    (setq lst(rdsio!>(car lst) 'input nil))
    (cond((eq lst !!er!!)(return !!er!!)))
    (loop!>
      (setq w(errorset '(read) nil nil))
      (cond((atom w) % unexpected data
             (progn(cload!>)(setq ![er!] 7720)(return !!er!!)))
           ((or(equal w (ncons !$eof!$))
               (equal w '(t))
               (atom w)) % eof encountered
             (progn(cload!>)(copar!>)(return t)))
           ((and wf(not(equal w '((cout!>))))) % not .loa file format
             (progn(cload!>)(setq ![er!] 7200)(return !!er!!))))
      (setq w (car w))
      (cond((or (null w) (atom w)) nil)
           ((and (pairp w) (null wx) (eq (car w) 'sgn!>))
              (setq wx t) (shsgndim!> (cadadr w)))
           ((eq(car w) 'setq)
             (progn (setq w(cadr w))
                    (cond((flagp w '!+ivar)
                      (prog2(pn!> w)(gterpri!>))))  ))
           ((eq(car w) 'pushabbr!>)
              (setq w (cadadr w))
	      (cond
                ((not (flagp w '!+abbr))
                    (setq w (cdr (explode2 w)))
                    (mapc w 'prin2)
                    (terpri))))
           ((eq (car w) 'comin!>)
             (comin!> (cadadr w)))
           ((eq (car w) 'putpn!>)
             (progn (setq wt (cadadr(cddddr w)))
                    (setq w (cadr(caddr w)))
                    (algpri!>
                      (cond((eqn wt 1) "Coordinates: ")
                           ((eqn wt 2) "Constants: ")
                           ((eqn wt 3) "Functions: ")) )
                    (algprinwb!> w)
                    (algterpri!>))))
      (setq wf nil))
    ))

(de shsgndim!> (w)
  (proc nil
    (prin2 "Dimension is ") (prin2 (length w))
    (prin2 " with Signature (")
    (while!> w
       (cond ((eqn (car w) 1) (prin2 "+"))
	     (t               (prin2 "-")))
       (cond ((cdr w) (prin2 ",")))
       (setq w (cdr w)))
    (prin2 ")")
    (terpri)))


%----- Line Length ; command ---------------------------------------------

(de setlinel!> (lst)
  (cond((null lst) (progn
         (prin2 "Line Length is ")
         (prin2 (linelength nil))
         (prin2 ".")(terpri) ))
       ((or(cdr lst)(not(numberp(car lst)))(lessp(car lst)0))
         (prog2 (setq ![er!] 1100) !!er!!))
       (t(linelength (car lst)))))


%-------- Show Switch ...; command 20.02.94 ------------------------------

(de sflag!> (w)
    (prog (wa)
      (cond ((null w) (return nil))
            ((or (cdr w) (not(idp(car w))))
              (prog2 (setq ![er!] 1100) (return !!er!!))) )
      (setq w (idtostcase!> (car w)))
      (cond ((and (not (flagp w 'switch))
                  (not (flagp w '!+switch)))
              (progn (setq ![er!] 6402) (doub!> w) (return !!er!!))))
      (setq wa (incom!> (cons '!* (explode2 w))))
      (prin2 w) (prin2 " is ")
      (prin2 (cond ((eval wa) "On.")(t "Off."))) (terpri)
      (return t)))


%------- Show Status; command 06.94 --------------------------------------

(de shstatus!> nil  % 05.96
  (progn
    % REDUCE version ...
      (prin2 "Running with ")
      (cond ((boundp!> 'version!*) (prin2 (eval 'version!*)))
            (t                     (prin2 "REDUCE 3.3")))
      (cond ((iscsl!>) (prin2 " [CSL"))
            (t         (prin2 " [PSL")))
      (cond ((islowercase!>) (prin2 " Lower-Case]"))
            (t               (prin2 " Upper-Case]")))
      (cond ((os!>) (prin2 " under ") (prin2 (os!>))))
      (terpri)
    % System Directory ...
      (cond (![grgdir1!] (progn
	(prin2 "System directory: ")
        (prin2 ![grgdir1!])
        (terpri))))
    % System case ...
      (showcase!>)
    % Dimension and Signature ...
      (sdimsgn!>)
    % Metric ...
      (cond (!#!G (progn
	(prin2 "  Metric: ")
	(prin2 (cond ((eqn ![mtype!] 1) "null")
	             ((eqn ![mtype!] 2) "diagonal")
		     ((eqn ![mtype!] 3) "general")
		     (t "unknown type")))
	(prin2 (cond ((and (eqn ![dtype!] 1)
			   (not(eqn ![mtype!] 1))) " and constant")
		     (t " ")))
	(terpri))))
    % Frame ...
      (cond (!#!T (progn
	(prin2 "  Frame: ")
	(prin2 (cond ((eqn ![ftype!] 1) "holonomic")
	             ((eqn ![ftype!] 2) "diagonal")
		     ((eqn ![ftype!] 3) "general")
		     (t "unknown type")))
	(terpri))))
    % Basis ...
      (cond (![umod!] (progn
        (prin2 "  Basis: anholonomic")
        (terpri))))
    t))

(de sdimsgn!> nil  % 05.96
  (proc (w)
    (prin2 "Dimension is ") (prin2 ![dim!])
    (prin2 " with Signature (")
    (setq w ![sgn!])
    (while!> w
       (cond ((eqn (car w) 1) (prin2 "+"))
	     (t               (prin2 "-")))
       (cond ((cdr w) (prin2 ",")))
       (setq w (cdr w)))
    (prin2 ")")
    (terpri)))


%------- Show All; command -----------------------------------------------

(de shall!> nil
  (proc (w)
    (setq w (alldata!>))
    (cond ((null w) (progn (prin2 "Nothing is known.")
                           (terpri)
                           (return nil))))
    (prin2 "Value of the following objects is known:") (terpri)
    (gprinreset!>)
    (while!> w
      (gprin!> " ") (pn0!>(car w)) (gterpri!>)
      (setq w (cdr w))) ))

(de shallbuilt!> (ww)
  (proc (w wc wn wx)
    (cond ((eq (car ww) '!*) (setq wc nil))
	  ((liter (car ww))  (setq wc (tostcase!> (car ww))))
	  (t (return nil)))
    (setq w ![datl!])
    (gprinreset!>)
    (while!> w
      (setq wn (car (explode (caaar w))))
      (cond
	((or (null wc) (eq wc wn))
	   (cond ((null wx) (setq wx t)
			    (prin2 "Built-in objects:")
			    (terpri)))
           (gprin!> " ")
           (gprils0!> (lowertxt!> (caar w)))
           (gterpri!>) ))
      (setq w (cdr w)))
    (cond ((null wx) (prin2 "No such built-in objects.")
		     (terpri)))))

%------- Evaluate ...; command -------------------------------------------

(de evalcomm!> (w fun) % o5.96
 (proc (we wb wc)
  (cond ((null w) (return nil)))
  (cond ((eq (setq w (dgood!> w)) !!er!!) (return !!er!!)))
  (setq w (altdata!> w))
  (while!> w
    (setq wc (car w))
    (cond((memq wc '(![cord!] ![const!] ![fun!] ![apar!])) nil)
         ((null (setq wb (eval wc))) (abse!> wc))
         (t(set wc
             (allcoll!> wb wc nil
                        (cond((get wc '!=idxl)(get wc '!=idxl))
                              (t '(0)))
                        fun))  ))
    (cond
      ((eq wc '!#!G   ) (mtype!>))
      ((eq wc '!#!G!I ) (mitype!>))
      ((eq wc '!#!T   ) (ftype!>))
      ((eq wc '!#!D   ) (fitype!>)) )
    (setq w (cdr w)))
  (return t)))

% Evaluation of expression of equality ...
(de evel!> (lst wi wn)
  (cond((null lst) nil)
       ((and (zerop(gettype!> wn))(not (flagp wn '!+equ)))
         (evalalg!> lst))
       ((and (not(zerop(gettype!> wn)))(not (flagp wn '!+equ)))
         (evalform!> lst))
       ((and (not(zerop(gettype!> wn))) (flagp wn '!+equ))
         (equationf!> (cadr lst) (caddr lst)))
       ((and (zerop(gettype!> wn))(flagp wn '!+equ))
         (equationa!> (cadr lst) (caddr lst)))))

(de normel!> (lst wi wn)
  (cond((null lst) nil)
       ((and (zerop(gettype!> wn))(not (flagp wn '!+equ)))
         (evalalg!> lst))
       ((and (not(zerop(gettype!> wn)))(not (flagp wn '!+equ)))
         (evalform!> lst))
       ((and (not(zerop(gettype!> wn))) (flagp wn '!+equ))
         (equationf1!> (cadr lst) (caddr lst)))
       ((and (zerop(gettype!> wn))(flagp wn '!+equ))
         (equationa1!> (cadr lst) (caddr lst)))))


%---------- Package ...; command 25.02.94 --------------------------------

(de loadpack!> (lst bool) % bool=t - message, bool=nil - silence
  (proc (w ww wu wl)
    (cond
      ((null lst) (return nil))
      ((or (cdr lst) (not(idp(car lst))))
        (setq ![er!] 8100) (return !!er!!)))
    (setq ww (car lst))
    (setq w (explode2 ww))
    (setq wu (incom!> (mapcar w 'touc!>)))
    (setq wl (incom!> (mapcar w 'tolc!>)))
    % already loaded ...
    (cond((or (memq ww (eval 'loaded!-packages!*))
              (memq wu (eval 'loaded!-packages!*))
              (memq wl (eval 'loaded!-packages!*)))
      (cond (bool (msg!> 8101) (return t))
	    (t                 (return t)))))
    % trying name as it is ...
    (setq w (errorset (list 'evload (list 'quote (ncons ww)))
                      ![erst1!] ![erst2!]))
    (cond ((not(atom w)) (progn
        (set 'loaded!-packages!* (cons ww (eval 'loaded!-packages!*)))
        (set 'loaded!-packages!* (cons wu (eval 'loaded!-packages!*)))
        (set 'loaded!-packages!* (cons wl (eval 'loaded!-packages!*)))
        (return t))))
    % trying uppercase name ...
    (setq w (errorset (list 'evload (list 'quote (ncons wu)))
                      ![erst1!] ![erst2!]))
    (cond ((not(atom w)) (progn
        (set 'loaded!-packages!* (cons ww (eval 'loaded!-packages!*)))
        (set 'loaded!-packages!* (cons wu (eval 'loaded!-packages!*)))
        (set 'loaded!-packages!* (cons wl (eval 'loaded!-packages!*)))
        (return t))))
    % trying lowercase name ...
    (setq w (errorset (list 'evload (list 'quote (ncons wl)))
                      ![erst1!] ![erst2!]))
    (cond ((not(atom w)) (progn
        (set 'loaded!-packages!* (cons ww (eval 'loaded!-packages!*)))
        (set 'loaded!-packages!* (cons wu (eval 'loaded!-packages!*)))
        (set 'loaded!-packages!* (cons wl (eval 'loaded!-packages!*)))
        (return t))))
    (setq ![er!] 8102)
    (return !!er!!)))



%---------- Solve ...; command 16.03.94 ----------------------------------

(de solvei!> (lst)
  (prog (we wv w wr)
    (setq lst (seek1q!> lst 'for)) % word!!!
    (cond((or(null lst)(null(car lst))(null(cdr lst)))
            (prog2(setq ![er!] 2300)(return !!er!!))))
    (setq wv (memlist!> '!, (cdr lst)))
    (setq we (memlist!> '!, (reverse(car lst))))
    (setq wv (mapcar wv 'solvev!>))
    (cond((memq !!er!! wv)(return !!er!!)))
    (setq we (mapcar we 'solvee!>))
    (cond((memq !!er!! we)(return !!er!!)))
    (setq ![solveq!] nil)
    (solveprep!> we)
    (setq we ![solveq!])
    (setq ![solveq!] nil)
    (cond((null we)(prog2(setq ![er!] 2304)(return !!er!!))))
    (setq w (list 'eval!> (list 'quote
               (list 'solve (cons 'list we) (cons 'list wv)))))
    (setq w (errorset w ![erst1!] ![erst2!]))
    (cond((atom w)(prog2(setq ![er!] 2301)(return !!er!!))))
    (solveres!> (car w))
    (setq wr ![solveq!])
    (setq ![solveq!] nil)
    (cond(wr (setq ![sol!] (append wr ![sol!])))
	 (t (msg!> 2302)))
    (return t)))

(de solvev!> (w) (nz!>(translata!> w)))

(de solveprep!> (w)
  (cond((atom w) nil)
       ((eq (car w) 'equal)
          (setq ![solveq!] (cons (solveprep1!> w) ![solveq!])))
       (t(mapc w 'solveprep!>))))

(de solveprep1!> (w) (mapcar w 'nz!>))

(de solveres!> (w)
  (cond((atom w) nil)
       ((eq (car w) 'equal)
          (setq ![solveq!] (cons (solveres1!> w) ![solveq!])))
       (t(mapc w 'solveres!>))))

(de solveres1!> (w) (mapcar w 'evalalg!>))

(de solvee!> (w)
  (cond((memq '!= w)(solveeq!> w))
       (t(prog (ww wi)
	   (setq ww (dgood!> w))
	   (cond((not(eq ww !!er!!))(return(solveeo!>(altdata!> ww)))))
	   (cond
	     ((idp(car w))(progn
		(setq wi (explode2(car w)))
		(selid!> wi nil)
		(setq wi (incomiv!> wi))
		(cond((not(flagp wi '!+equ))
		  (prog2(setq ![er!] 2300)(return !!er!!))))
		(return(solveeq!>(list '!L!H!S w '!= '!R!H!S w)))))
	     (t(prog2(setq ![er!] 2300)(return !!er!!))))))))

(de solveeq!> (w)
  (proc (wa wr)
    (setq wa (seek1!> w '!=))
    (cond((or(null(car wa))(null(cdr wa)))
      (prog2(setq ![er!] 2300)(return !!er!!))))
    (setq w (list (reverse(car wa)) '!- (cdr wa)))
    (setq ![extvar!] nil)
    (setq w (translate!> w))
    (cond((or(null w)(eq w !!er!!)) (return w)))
    (cond((zerop(car w)) (return(ncons(list 'equal (cdr w) nil)))))
    (setq w (cdr w))
    (while!> w
      (setq wr (cons (list 'equal (caar w) nil) wr))
      (setq w (cdr w)))
    (return wr)))

(de solveeo!> (w)
  (cond((null w) (prog2 (setq ![er!] 2304) !!er!!))
       (t(proc (wr)
	   (while!> w
	     (cond((not(flagp (car w) '!+equ))
	       (prog2(setq ![er!] 2303)(return !!er!!))))
	     (setq ![solveq!] nil)
	     (put '![solveq!] '!=typ (gettype!> (car w)))
	     (soexp!> (eval(car w)))
	     (setq wr (append ![solveq!] wr))
	     (setq ![solveq!] nil)
	     (setq w (cdr w)))
	   (return wr)))))

(de soexp!> (w)
  (cond((atom w) nil)
       ((eq (car w) 'equal) (soexp1!> w))
       (t (mapc w 'soexp!>))))

(de soexp1!> (w)
  (cond((zerop(get '![solveq!] '!=typ))
         (setq ![solveq!] (cons w ![solveq!])))
       (t(proc nil
	   (setq w (dfsum!> (list (cadr w)
				  (chsign!> t (caddr w)))))
	   (while!> w
	     (setq ![solveq!] (cons (list 'equal (caar w) nil) ![solveq!]))
	     (setq w (cdr w)))))))

%----- Object Declaration Command  11.94, 05.96 --------------------------

(de obdec!> (lst type) % type=0 object, 1 equation, 2 connection ...
  (cond((null lst) nil) (t
    (proc (wn wt wi wy wd  wa wb wc)
      % wn - internal id
      % wt -  =type
      % wi -  =idxl
      % wy -  =sidxl
      % wd -  =dens
      (setq wt 0) % default type is scalar ...
      (setq wn (idtra!> (car lst))) % identifier ...
      (cond ((eq wn !!er!!) (return !!er!!))
	    ((null(setq lst (cdr lst))) (return
              (formnew!> wn (cond ((eqn type 2) 1) (t wt)) wi wy wd type))))
      % splitting lst into parts ...
      (setq lst (splitparts!> lst))
      (setq wa (car lst))    % indices
      (setq wb (cadr lst))   % type
      (setq wc (caddr lst))  % symmetries
      % indices ...
      (cond ((null wa) (go lab1)))
      (setq wi (indtrac!> wa))
      (cond ((eq wi !!er!!) (setq ![er!] 8602) (return !!er!!)))
      lab1
      % type ...
      (cond ((and (eqn type 2) (null wb)) (setq wt 1)))
      (cond ((null wb) (go lab2)))
      (setq wt (typetrac!> wb))
      (cond ((eq wt !!er!!) (setq ![er!] 8601) (return !!er!!)))
      (setq wd (cdr wt))
      (setq wt (car wt))
      lab2
      % symmetries ...
      (cond ((null wc) (go lab3)))
      (setq wy (symtrac!> wc wi))
      (cond ((eq wy !!er!!) (setq ![er!] 8606) (return !!er!!)))
      lab3
      (return (formnew!> wn wt wi wy wd type)) ))))

% Forms new object by assigning appropriate flags and props ...
(de formnew!> (wn wt wi wy wd type) % 05.96
  (proc nil
    (cond
      ((eqn type 2) % connection
        (cond ((not(eqn wt 1)) (setq ![er!] 3002) (return !!er!!)))
        (cond ((equal wi '(t nil))   (flag (ncons wn) '!+fconn)
				     (flag (ncons wn) '!+noncov))
	      ((equal wi '(1 0))     (flag (ncons wn) '!+hconn)
				     (flag (ncons wn) '!+noncov))
	      ((equal wi '((u . 2))) (flag (ncons wn) '!+uconn)
				     (flag (ncons wn) '!+noncov))
	      ((equal wi '((d . 2))) (flag (ncons wn) '!+dconn)
				     (flag (ncons wn) '!+noncov))
	      ((null wi)             (setq wi '(t nil))
                                     (flag (ncons wn) '!+fconn)
				     (flag (ncons wn) '!+noncov))
	      (t (setq ![er!] 3001) (return !!er!!)))))
    (global (ncons wn))
    (flag (ncons wn) '!+ivar)
    (flag (ncons wn) '!+abbr)
    (setq ![abbr!] (cons wn ![abbr!]))
    (put wn '!=type wt)
    (cond (wi (put wn '!=idxl  wi)))
    (cond (wy (put wn '!=sidxl wy)))
    (cond (wd (put wn '!=dens  wd)))
    (cond ((eqn type 1) (flag (ncons wn) '!+equ))) % equation
    (while!> wi
      (cond ((spinp!>(car wi)) (put wn '!=constr '((sp!>)))))
      (setq wi (cdr wi)))
    (return t)))

% ID translation ...
(de idtra!> (w) % 05.96
 (prog (we wv)
   (cond
     ((not(idp w)) (prog2 (setq ![er!] 8600) (return !!er!!)))
     ((flagp w '!+grg) (prog2 (doub!> w) (msg!> 8603))))
   (setq we (explode2 w))
   (cond((badchar!> we)
     (progn (doub!> w) (setq ![er!] 8604) (return !!er!!))))
   (setq wv (incomiv!> we))
   (cond
     ((or (flagp wv '!+ivar) (flagp w '!+grgmac) (gettype!> wv))
       (progn (doub!> w) (setq ![er!] 3000) (return !!er!!))))
   (return wv)))

(de badchar!> (lst) % 05.96
  (cond ((null lst) nil)
        ((or (digit(car lst)) (eq (car lst) '!~)) t)
        (t (badchar!>(cdr lst)))))

% Split command in parts ....
(de splitparts!> (lst) % 05.96
  (proc (w wr)
    (while!> (and lst (not (memqs!> (car lst) '(is with)))) % word!!!
      (setq w (cons (car lst) w))
      (setq lst (cdr lst)))
    (setq w (reverse w))
    (cond ((null lst) (return (list w nil nil)))
	  ((memqs!> (car lst) '(with)) % word!!!
                      (return (list w nil (cdr lst)))))
    (setq lst (cdr lst))
    (setq wr w)
    (setq w nil)
    (while!> (and lst (not (memqs!> (car lst) '(with)))) % word!!!
      (setq w (cons (car lst) w))
      (setq lst (cdr lst)))
    (cond ((null lst) (return (list wr (reverse w) nil)))
	  (t (return (list wr (reverse w) (cdr lst)))))
    ))

% Indices translation ...
(de indtrac!> (w)  % 05.96
  (proc (wr wp wt)
    (cond ((not(zerop(remainder (length w) 2))) (return !!er!!)))
    (while!> w
      (setq wp (car w))
      (cond ((not(memq wp '( !_ !. !' !^ ))) (return !!er!!)))
      (setq wt (cadr w))
      (setq wt (indtra1!> wt wp))
      (cond ((eq wt !!er!!) (return !!er!!)))
      (setq wr (cons wt wr))
      (setq w (cddr w)))
    (return(reversip wr)) ))

% One index translation ...
(de indtra1!> (w wp) % 05.96
  (cond
    ((not(idp w)) !!er!!)
    ((get w '!=uc) % single lc letter => holonomic or frame
      (cond ((eq wp '!')  t  )
	    ((eq wp '!.) nil )
	    ((eq wp '!^)  1  )
	    ((eq wp '!_)  0  )))
    (t(prog (ww wd wl www)
	(setq ww (explode2 w))
	(cond
	  ((get (car ww) '!=lc) % spinorial
	    (cond ((eq (car(reverse ww)) '!~) (setq wd t)))
	    (return (cons
		      (cond
                        ((memq wp '(!' !^)) (cond (wd 'ud) (t 'uu)))
			(t                  (cond (wd  'd) (t  'u))))
		      (cond
                        (wd (sub1(length ww)))
                        (t  (length ww))))))
	  ((get (car ww) '!=uc) % enumerating
	    (setq www (compress (cdr ww)))
	    (cond
	      ((idp www)
		(cond ((equal (cdr ww) '(!d !i !m)) (return '(n)))
		      (t                  (return !!er!!))))
	      ((zerop www) (return !!er!!))
	      (t (return (cons 'n www)))))
	  (t (return !!er!!)))))))

% Type and Density translation ...
(de typetrac!> (wb) % 05.96
  (prog (wt wd)
    (setq wb (splitpartsd!> wb))
    (setq wt (typetra1!> (car wb)))
    (setq wd (denstra1!> (cdr wb)))
    (cond ((or (eq wt !!er!!) (eq wd !!er!!)) (return !!er!!))
	  (t (return (cons wt wd))))))

(de splitpartsd!> (lst) % 05.96
  (proc (w)
    (while!> (and lst (not (memqs!> (car lst) '(density)))) % word!!!
      (setq w (cons (car lst) w))
      (setq lst (cdr lst)))
    (setq w (reverse w))
    (cond ((null lst) (return (cons w nil)))
	  (t (return (cons w (cdr lst)))))))

% Type translation ...
(de typetra1!> (w) % 05.96
  (cond ((null w) 0)
        ((eqs!> w '(vector)) -1) % word!!!
	((eqs!> w '(scalar))  0) % word!!!
	((eqs!> (cdr w) '(!- form)) (pformtra1!>(car w))) % word!!!
        (t !!er!!)))

(de pformtra1!> (w) % 05.96
  (prog2
    (setq w (ntranslata!> w))
    (cond
      ((eq w !!er!!) !!er!!)
      ((lessp w 0)   !!er!!)
      (t w))))

% Density translation ...
(de denstra1!> (w) % 05.96
  (proc (w1 w2 w3 w4 wc)
    (cond ((null w) (return nil)))
    (setq w (memlist!> '!* w))
    (cond ((eq w !!er!!) (return !!er!!)))
    (while!> w
      (setq wc (car w))
      (cond
	((equal wc '(!s!g!n!D)) (setq w1 t))
	((equal wc '(!s!g!n!L)) (setq w3 t))
	((equal wc '(!D)) (setq w2 1))
	((equal wc '(!L)) (setq w4 1))
	((and (eq (car wc) '!D) (cdr wc) (eq (cadr wc) '!^) (cddr wc))
	  (setq wc (ntranslata!>(cddr wc)))
	  (cond ((eq wc !!er!!) (return !!er!!)))
	  (setq w2 wc))
	((and (eq (car wc) '!L) (cdr wc) (eq (cadr wc) '!^) (cddr wc))
	  (setq wc (ntranslata!>(cddr wc)))
	  (cond ((eq wc !!er!!) (return !!er!!)))
	  (setq w4 wc))
	(t (return !!er!!)))
      (setq w (cdr w)))
    (cond ((or w1 w2 w3 w4) (return (list w1 w2 w3 w4)))
	  (t                (return nil)))))

% Symmetries translation ...
(de symtrac!> (wy wi) % 05.96
  (cond
    (t(proc (wr w)
        (cond ((eqs!> (car wy) 'symmetries) % word!!!
                 (setq wy (cdr wy))))
	(cond ((null wy) (return nil)))
	(setq wy (memlist!> '!, wy))
	(cond ((eq wy !!er!!) (return !!er!!)))
	(while!> wy
	  (setq w (symspec1!> (car wy) wi))
	  (cond ((eq w !!er!!) (return !!er!!)))
	  (setq wr (cons w wr))
	  (setq wy (cdr wy)))
	(cond((overlapp!> wr)
	  (prog2 (msg!> 8607) (return !!er!!))))
	(return(reversip wr))))))

% One symmetry item:  W = (s ( , , ))
(de symspec1!> (w wi) % 05.96
  (cond
    ((or (null(cdr w)) (not(memq (car w) '(!a !s !c !h !A !S !C !H)))) !!er!!)
    (t(prog (wt wr)
        (setq wt (tostcase!> (car w)))
	(setq w (symspecl!> (cadr w) wi))
	(cond ((eq w !!er!!) (return !!er!!))
	      ((null(cdr w)) (return !!er!!))) % length must be 2 or greater
	(cond
	  ((memq wt '(!h !H))
            (cond ((or (not (eqn (length w) 2))
                       (not (hequal!> w wi)))
                     (return !!er!!))
                  (t (return (cons wt w)))))
	  ((not(allequal!> w wi)) (return !!er!!))
	  (t (return (cons wt w))))))))

% List of symmetries or indices: W = ( , , )
(de symspecl!> (w wi) % 05.96
  (proc (wr wa)
    (setq w (memlist!> '!, w))
    (cond ((eq w !!er!!) (return !!er!!)))
    (while!> w
      (setq wa (symspec2!> (car w) wi))
      (cond ((eq wa !!er!!) (return !!er!!)))
      (setq wr (cons wa wr))
      (setq w (cdr w)))
    (return(reversip wr))))

% General translation ...
(de symspec2!> (w wi)
  (cond ((cdr w) (symspec1!> w wi))              % something general: s( , )
	((atom(car w)) (symspec0!> (car w) wi))  % one index: 1
	(t (symspecl!> (car w) wi))))            % list: ( , , )

% Just one index number ...
(de symspec0!> (w wi)
  (cond ((and (numberp w) (leq w (length wi))) w)
	(t !!er!!))) % out of range

(de overlapp!> (wr)
  (proc (w wa)
    (while!> wr
      (setq wa (iron1!>(car wr)))
      (cond ((intersecl!> wa w) (return !!er!!)))
      (setq w (append wa w))
      (setq wr (cdr wr)))
    (return nil)))

% Forms list of all numbers ...
(de iron1!> (wr)
  (cond ((null wr) nil)
	((idp(car wr)) (iron1!>(cdr wr)))
	((atom(car wr)) (cons (car wr) (iron1!>(cdr wr))))
	(t (append (iron1!>(car wr))
		   (iron1!>(cdr wr))))))

% Replaces ind numbers by their types ...
(de itypes!> (w wi)
  (cond ((null w) nil)
	((idp w) w)
	((numberp w) (getn!> wi w))
	(t (cons (itypes!> (car w) wi) (itypes!> (cdr w) wi)))))

% All symmetries in the list are identical ...
(de allequal!> (w wi)
  (cond ((null(cdr w)) t)
	((equal (itypes!> (car w) wi) (itypes!> (cadr w) wi))
          (allequal!> (cdr w) wi))
	(t nil)))

(de hequal!> (w wi)
  (prog (w1 w2)
    (setq w1 (itypes!> (car w) wi))
    (setq w2 (cotype!>(itypes!> (cadr w) wi)))
    (return(equal w1 w2))))

(de cotype!> (w)
  (cond
    ((pairp w)
      (cond
        ((eq (car w) 'u)  (cons 'd  (cdr w)))
        ((eq (car w) 'd)  (cons 'u  (cdr w)))
        ((eq (car w) 'uu) (cons 'ud (cdr w)))
        ((eq (car w) 'ud) (cons 'uu (cdr w)))
	(t (mapcar w 'cotype!>))))
    (t w)))

%========== End of GRGcomm.sl =============================================%

