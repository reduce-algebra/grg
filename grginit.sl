%==========================================================================%
%   GRGinit.sl      Useful Functions, Cord Const Fun Declarations, Scanner %
%==========================================================================%
%   GRG 3.2 Standard Lisp Source Code     (C) 1988-2000 Vadim V. Zhytnikov %
%==========================================================================%
% This file is distributed without any warranty. You may modify it but you %
% are not allowed to remove author's name and/or distribute modified file. %
%==========================================================================%

%--------- Debuggin 05.96 --------------------------------------------------

% Switch otaldka
(de swotladka!> (bool)
  (progn
    (cond ((not(iscsl!>)) (eval '(load debug))))
    (setq ![erst1!] t)
    (setq ![erst1!] t) ))

% Command otladka
(de otladka!> nil
  (progn
    (swotladka!> t)
    (lisp!>) ))

% lisp interpreter
(de lisp!> nil
  (prog (w)
    (setq promptstring!* "<= ")
    (prin2 "Entering LISP ...")(terpri)
    loop
    (cond ((iscsl!>) (prog2 (printprompt promptstring!*)
                            (setpchar promptstring!*))))
    (setq w (read))
    (cond ((or (eq w '!e!x!i!t) (eq w '!E!X!I!T))
             (prin2 "Exiting LISP ...")(terpri)
             (setq promptstring!* "<- ")
             (cond ((iscsl!>) (setpchar promptstring!*)))
             (return nil)))
    (setq w (errorset w t nil))
    (cond ((atom w) (print 'error))
          (t        (print (car w))))
    (go loop)))

%-------- Reduce version and OS auto detection ---------------------------

% We use this since in some old versions of PSL BOUND is absent
(de boundp!> (w)
  (cond ((getd 'boundp) (boundp w))
	(t (setq w (errorset w nil nil))
	   (cond ((atom w) nil)
		 (t t)))))

% Check w into LISPSYSTEM* or not
(de yes!> (w) (memq w (eval 'lispsystem!*)))

% Is background PSL internally lowercase or not
(de islowercase!> nil
  (cond ((getd '!c!a!r) t) (t nil)))

(de showcase!> nil
  (cond
    ((islowercase!>)
       (prin2 "System variables are lower-cased: e i pi sin ...")
       (terpri))
    (t (prin2 "System variables are upper-cased: E I PI SIN ...")
       (terpri))))

% This function is called at the very start of GRG and tryes
% to set appropriate values for [dirsep] and [syscall].
% If something wrong this values can be overriden in grg.cfg
(de tuneos!> nil
  (cond
    % We can tune. Works for R 3.5 and later ...
    ((boundp!> 'lispsystem!*)
	% [dirsep]
        (setq ![dirsep!] nil) % We prefer to have GRG with trailing /
                              % for the sake of definiteness
        %(cond ((yes!> 'vms)  (setq ![dirsep!] '!:))  % VMS (?)
        %      ((yes!> 'unix) (setq ![dirsep!] '!/))  % UNIX
        %      (t             (setq ![dirsep!] '!\))) % Others
        % loaddirectories* under UNIX ...
        (cond ((and (yes!> 'unix) (boundp!> 'loaddirectories!*))
           (set 'loaddirectories!*
             (cons "$reduce/xr/bin/" (eval 'loaddirectories!*)))))
	% [syscall]
	(cond ((yes!> 'vms)  (setq ![syscall!] 2))   % VMS via quit (?)
	      (t             (setq ![syscall!] 1)))) % Other via system
    % No information for tuning is available. R 3.3 and 3.4 ...
    (t (setq ![dirsep!] nil) % In this case GRG env.var. must include
			     % trailing \ or / or ...
       (setq ![syscall!] 1)  % We allways trying system
       )))

% Is this CSL or PSL ?
(de iscsl!> nil
  (cond ((and (boundp!> 'lispsystem!*) (yes!> 'csl))    t   )
        ((and (boundp!> 'lispsystem!*) (yes!> 'psl))    nil )
        ((getd 'dskin)                                  nil )
        (t                                              t   )))

% OS
(de os!> nil
  (cond ((boundp!> 'lispsystem!*)
           (cond ((yes!> 'dos)     "DOS"        )
                 ((yes!> 'unix)    "UNIX"       )
                 ((yes!> 'winnt)   "Windows NT" )
                 ((yes!> 'os2)     "OS/2"       )
                 ((yes!> 'vms)     "VMS"        )
                 (t nil)))
         (t nil) ))

%-------- General Useful Functions ----------------------------------------

%(de copy (lst)
%  (cond ((atom lst) lst)
%        ((null(cdr lst)) (cons (copy(car lst)) nil))
%        ((null(car lst)) (cons nil (copy(cdr lst))))
%        (t (cons (copy(car lst)) (copy(cdr lst))))))

% De in CSL explode2 is buggy so we provide replacement ...
(de explode2!> (lst)
  (proc (wr wc)
    (setq lst (explode lst))
    (while!> lst
      (setq wc (car lst))
      (setq lst (cdr lst))
      (cond ((eq wc '!!)
               (cond ((and lst (eq (car lst) '!!))
                       (setq wr (cons wc wr))
                       (setq lst (cdr lst)))))
            (t (setq wr (cons wc wr)))) )
    (return (reversip wr))))

% Makes loop list from a list ...
(de makeloop!> (lst)
  (proc (w)
    (setq w lst)
    (while!> (cdr w) (setq w (cdr w)))
    (rplacd w lst)))

(de cout!> nil nil)
(de rout!> nil nil)

(de factorial!> (w)
  (cond ((zerop w) 1)
	(t (times w (factorial!> (sub1 w))))))

(de binom!> (wk wn)
  (quotient (factorial!> wn)
            (times (factorial!> wk)
                   (factorial!> (difference wn wk)))))

% Upper<->Lower Case conversion for letters ...
(de tolc!> (w) (cond ((get w '!=lc) (get w '!=lc)) (t w)))
(de touc!> (w) (cond ((get w '!=uc) (get w '!=uc)) (t w)))

% To default case ...
(de tostcase!> (w)
  (cond ((not(liter w))        w)
        (![lower!]     (tolc!> w))
	(t             (touc!> w))))

% Error Interrupt ...
(de err!> (we)
  (progn
    (setq ![er!] we)
    (error we nil)))
%(de err!> (we) (throw '!$error!$ we))
(de errorset!> (we wa wb)
  (prog (wr)
    (setq wr (errorset we wa wb))
    (cond ((null wr) (return ![er!]))
          (t (return wr)))))

% Menu function ...
% WQ - questions, WA - answers
(de asker!> (wq wa)
  (proc (w)
    (while!> wq (prin2 (car wq)) (terpri) (setq wq (cdr wq)))
    (terpri)
    (cond
      ((or (getd 'x!-pr!!) (getenv "redfront")) (prog2
         (prin2 "  Type 0, 1 or 2")
         (setq promptstring!* (compress (list
           '!" (int2id 1) '!  '!: (int2id 2) '!" )))))
      (t (setq promptstring!* "  Type 0, 1 or 2: ")))
    (cond ((iscsl!>) (prog2 (printprompt promptstring!*)
                            (setpchar promptstring!*))))
    (loop!> (exitif (memq (setq w (intern(readch))) wa)))
    (return w)))

% Is there any number in the lst ? ...
(de memnum!> (lst)
  (cond ((null lst) nil)
        ((numberp (car lst)) t)
        (t (memnum!> (cdr lst)))))

% Cut the tail of lst. Side Effect! ...
(de wipl12!> (lst)
  (prog2 (wipl12r!> lst) lst))

(de wipl12r!> (lst)
  (cond((null(cddr lst)) (rplacd lst nil))
       ((eq (cadr lst) '!!) (rplacd lst nil))
       (t(wipl12r!> (cdr lst)))))

% Produces d x^wn 1-form wn=0,1,2,3,4...
(de mkdx!> (wn)
   (proc (w wc)
     (setq wn (add1 wn))
     (setq wc wn)
     (while!> (lessp 1 wc)
       (setq wc (sub1 wc))
       (setq w (cons '(nil . t) w)) )
     (return (ncons (cons 1 (cons (expt 2 wn)
                                  (reversip (cons '(t . t) w))))))))

% LESSP for lists ...
(de lessl!> (w1 w2)
  (cond ((null w1) nil)
	((atom w1) (lessp w1 w2))
	((equal (car w1) (car w2)) (lessl!> (cdr w1) (cdr w2)))
	(t (lessl!> (car w1) (car w2)))))

% LEQ for lists ...
(de leql!> (w1 w2)
  (cond ((null w1) t)
	((atom w1) (leq w1 w2))
	((equal (car w1) (car w2)) (leql!> (cdr w1) (cdr w2)))
	(t (lessl!> (car w1) (car w2)))))

% WN=2^N -> N
(de log2!> (wn)
  (cond ((eqn wn 2) 1)
        (t (add1(log2!>(quotient wn 2))))))

% Absolute value ...
(de abs!> (w)
  (cond ((lessp w 0) (minus w)) (t w)))

% Like OR but with one argument ...
(de orl!> (lst)
  (cond ((null lst) nil)
        ((car lst) t)
        (t (orl!>(cdr lst)))))


%----- List Splitting and Analysis functions ------------------------------

% All ID=, in
%   (a b , c , k ...) -> ( (a b) (c) (k ...) )
(de memlist!> (id lst)
   (proc (wa wb)
      (setq lst (cons id lst))
      (while!> lst
            (setq lst (cdr lst))
            (while!> (and lst (not(eq (car lst) id)))
                (setq wa (cons (car lst) wa))
                (setq lst (cdr lst)))
            (cond
               ((null wa)
                 (prog2 (setq ![er!] 913) (return !!er!!)))
               (t(prog2
                 (setq wb (cons (reversip wa) wb))
                 (setq wa nil)))))
      (return(reversip wb))))

(de memlistbr!> (id lst)
   (proc (wa wb wl)
      (setq wl 0)
      (setq lst (cons id lst))
      (while!> lst
            (setq lst (cdr lst))
            (while!> lst
		(exitif (and (eq (car lst) id) (leq wl 0)))
		(cond ((eq (car lst) '![) (setq wl (add1 wl)))
		      ((eq (car lst) '!]) (setq wl (sub1 wl))) )
                (setq wa (cons (car lst) wa))
                (setq lst (cdr lst)))
            (cond
               ((null wa)
                 (prog2 (setq ![er!] 913) (return !!er!!)))
               (t(prog2
                 (setq wb (cons (reversip wa) wb))
                 (setq wa nil)))))
      (return(reversip wb))))


% All IDL=(+ - ) in
%   (a b - c + k ... ) -> ( ((a b).-) ((c).+) ...)
(de mems!> (idl lst bool)
  (proc(w wa wss)
    (cond((null lst)(return nil)))
    (while!> lst
      (cond((setq wss(memq(car lst)idl))
            (cond(wa (prog2 (setq w(cons(cons(cond(bool(reversip wa))
						  (t wa))
	                                     (car wss))w))
                            (setq wa nil)))
                 (t(return !!er!!))))
           ((memq(car lst)idl)(return !!er!!))
           (t(setq wa(cons(car lst)wa))))
        (setq lst(cdr lst))
        (cond((and(null lst)wa)(return !!er!!))) )
     (return(reversip w))))

% MEMBER with Synonymy ...
(de memqs!> (wi lst)
  (cond((null lst) nil)
       ((eqs!> wi (car lst)) t)
       (t (memqs!> wi (cdr lst)))))

% WA=(A B C 0 1) -> WA=(A B C) WD=(0 1)  Side Effect for WA!
(de selid!> (wa wd) % -> wd
  (cond((null(cdr wa)) nil)
       ((liter(cadr wa)) (selid!> (cdr wa) wd))
       (t(progn (setq wd (cdr wa))
		(rplacd wa nil)
		wd))))

% First WI=xxx with Synonymy in
%   (a b xxx m n ...) -> ( (b a) m n ...)
(de seek1q!> (lst wi)
  (proc (wa)
    (while!> lst
      (cond ((eqs!> (car lst) wi) (return(cons wa (cdr lst)))))
      (setq wa(cons(car lst)wa))
      (setq lst(cdr lst)))))

% First W=(xxx yyy) in
%   (a b xxx m n ...) -> ( (b a) xxx m n ...)
(de seek!> (lst w)
  (proc (wa)
    (while!> lst
      (cond ((memq (car lst) w) (return(cons wa lst))))
      (setq wa(cons(car lst)wa))
      (setq lst(cdr lst)))))

% First WI=xxx in
%   (a b xxx m n ...) -> ( (b a) m n ...)
(de seek1!> (lst wi)
  (proc (wa)
    (while!> lst
      (cond ((eq (car lst) wi) (return(cons wa (cdr lst)))))
      (setq wa(cons(car lst)wa))
      (setq lst(cdr lst)))))

% Special ASSOC with Synonymy ...
(de assocf!> (nm lst)
  (proc (w)
    (while!> lst
      (cond ((eqs!> (caar lst) nm)
               (return(cdar lst)))
            ((and (idp(caar lst)) (pairp nm) (setq w(layf!> nm(car lst))))
               (return w))
            (t(setq lst(cdr lst)))))))

% With Synonymy If WN=(a b) and LST=(a b c ...) -> (c ...)
% otherwise NIL ...
(de layf!> (wn lst)
  (proc nil
    (while!>(and wn lst)
      (cond
        ((eqs!> (car wn) (car lst))
           (prog2 (setq lst (cdr lst)) (setq wn (cdr wn))))
        (t (return nil))))
    (cond ((null wn) (return lst)))))

% Multy level ASSOCF> ...
(de assf!> (nms lst)
  (cond ((null(cdr nms))(assocf!>(car nms)lst))
        ((setq lst(assocf!>(car nms)lst))(assf!>(cdr nms)lst))
        (t nil)))

%----- Type of the Object may depend on the context ----------------------

(de gettype!> (u)  % 05.96
  (prog (w)
    (setq w (get u '!=type))
    (return (cond ((null w) nil)
		  ((numberp w) w)
		  (t (eval w))))))

(de algp!> (u) (zerop(gettype!> u)))

%----- Constructing Functions --------------------------------------------

% APPEND with LST1 reversed ...
(de app!> (lst1 lst2)
   (proc nil
      (while!> lst1
         (setq lst2(cons(car lst1)lst2))
               (setq lst1(cdr lst1)))
      (return lst2)))

% APPEND without repeated elements ...
(de appmem!> (wa wb)
  (prog2(while!> wa
          (cond((not(memq(car wa) wb))(setq wb(cons(car wa)wb))))
          (setq wa(cdr wa)))
        wb))

% CONS without repeated elements ...
(de consmem!> (w lst)
  (cond((memq w lst) lst)
       (t(cons w lst))))

% CONS if WA non NIL otherwise WD ...
(de consn!> (wa wd)
  (cond(wa (cons wa wd))
       (t wd)))

% CONS if WB non NIL otherwise NIL ...
(de consni!> (wa wb)
  (cond(wb (cons wa wb))(t nil)))

% Make List from linear list with !( !) ...
(de mklevel!> (any)
  (cond((atom any)(ncons any))
       (t(proc(wa wb wc)
          (loop!>
            (while!>(not(or(null any)
                         (eq(car any)(quote !) ))
		         (eq(car any)(quote !( )) ))
                (setq wa(cons(car any)wa))
                (setq any(cdr any)) )
            (exitif (or(null any)(eq(car any)(quote !) ))))
            (setq wb(mklevel!>(cdr any)))
            (setq wa(cons(car wb)wa))
            (setq wc(nconc wa wc))
            (setq wa nil)
            (setq any(cddr wb)) )
       (return(cons(reversip(nconc wa wc)) any))))))

% Makes (TIMES a b c ...) if a,b,c... not NIL ...
(de mktimes!> (lst)
  (cond ((memq nil lst) nil)
        (t (cons 'times lst))))

% Makes (TIMES a b) if ab not NIL ...
(de mktimes2!> (wa wb)
  (cond ((and wa wb) (list 'times wa wb))
        (t nil)))


%----- Open With System Directory ----------------------------------------

(de grgopeninput!> (w)
  (prog (wc ww)
    (setq wc (errorset (list 'open w (list 'quote 'input)) nil nil))
    (cond
      ((and (atom wc) ![grgdir!]) % Trying from system directory ...
	(progn
	  (setq ww (compress (app!> ![grgdir!] (cdr(explode w)))))
          (setq wc
            (errorset (list 'open ww (list 'quote 'input)) nil nil))
	  (cond((not(atom wc))
	    (cond((equal w "grg.cfg") (msg!> 8902))
		 (t (msg!> 8901)))))
	  (return wc)))
      (t(return wc)))))


%-------- Types of Indices -----------------------------------------------

% Index Type Predicates ...
(de holp!>   (w) (numberp w))                           % Holonomic
(de holpu!>  (w) (eqn w 1))                             % Holonomic Up
(de holpd!>  (w) (eqn w 0))                             % Holonomic Down
(de tetrp!>  (w) (or(eq w t)(null w)))                  % Tetrad
(de tetrpu!> (w) (eq w t))                              % Tetrad Up
(de tetrpd!> (w) (null w))                              % Tetrad Down
(de spinp!>  (w) (and (pairp w) (not(eq (car w) 'n))))  % Spinorial
(de enump!>  (w) (and (pairp w) (eq (car w) 'n)))       % Enumarating
(de dotp!>   (w) (and (pairp w)                         % Dotted
		      (memq (car w) '(d ud))))
(de undotp!> (w) (and (pairp w)                         % Undotted
		      (memq (car w) '(u uu))))
(de upperp!> (w) (or (eq w 't) (eqn w 1)                % Upper
		     (and (pairp w)
                          (memq (car w) '(uu ud)))))

% The object W has a spinorial index ...
(de isspinor!> (w) (isspinor1!> (get w '!=idxl)))
(de isspinor1!> (wi)
  (cond ((null wi) nil)
	((spinp!>(car wi)) t)
	(t (isspinor1!>(cdr wi)))))

% The object has a holonomic index ...
(de hashol!> (w) (hashol1!> (get w '!=idxl)))
(de hashol1!> (wi)
  (cond ((null wi) nil)
	((holp!>(car wi)) t)
	(t (hashol1!>(cdr wi)))))

% The object has a frame index ...
(de hasfram!> (w) (hasfram1!> (get w '!=idxl)))
(de hasfram1!> (wi)
  (cond ((null wi) nil)
	((tetrp!>(car wi)) t)
	(t (hasfram1!>(cdr wi)))))

% Object has hol indices or frame which equals holonomic
%  in holonomic regime ...
(de holonomq!> (wi)
  (or (hashol1!> wi) (and (holonomicp!>) (hasfram1!> wi))))
% For one index ...
(de holonomq1!> (w)
  (or (holp!> w) (and (holonomicp!>) (tetrp!> w))))


% Gives Dimension of this index ...
(de dimid!> (w)
  (cond ((or (atom w) (null(cdr w))) ![dim1!])
	(t (cdr w))))


%----- intern-compress ---------------------------------------------------

(de incom!> (w)  (intern(compress w)))

(de incomiv!> (w)  (intern(compress (cons '!# w))))

(de idtostcase!> (w)
  (cond ((idp w) (intern (compress (mapcar (explode w) 'tostcase!>))))
	(t       w)))

%----- Make Boxes for Data Storing ---------------------------------------

% Sets empty Box for int.var. W ...
(de makebox!> (w) (set w (mkbox!> w)))

% Returns empty Box appropriate for int.var. W's storing ...
(de mkbox!> (w)
  (mkspace!> (cond ((get w '!=idxl) (get w '!=idxl))
                   (t '((n . 0))) )))

% Box for general case ...
(de mkspace!> (wi) % wi - idxl list
  (cond((null wi) nil)
       (t(mks1!> (dimid!>(car wi)) (mkspace!>(cdr wi))))))

% Makes list of WN+1 copyes of LST ...
(de mks1!> (wn lst)
  (prog (w)
    (for!> i (0 1 wn) do
      (setq w (cons (copy lst) w)))
    (return w)))

% Makes list of WN copyes of LST ...
(de mknlist!> (wn lst)
  (prog (w)
    (for!> i (1 1 wn) do
      (setq w (cons (copy lst) w)))
    (return w)))

% Box for scalar ...
(de mkskl!> nil (copy '(nil)))

% Box for 1-index tensor ...
(de mkt1!> nil (mks1!> ![dim1!] nil))

% Box for wn-index tensor ...
(de mkt!> (wn)
  (prog (w)
    (cond((eqn wn 1)(return(mkt1!>))))
    (setq w (mkt!>(sub1 wn)))
    (return (mks1!> ![dim1!] w))))

% Forms list (0 1 2 ... (SUB1 [!DIM!]))
(de dimlist!> (u)
  (cond ((eqn u ![dim!]) nil)
	(t (cons u (dimlist!> (add1 u))))))

% Forms list (1 2 ... [!DIM!])
(de dimlist1!> (u)
  (cond ((eqn u ![dim!]) (ncons ![dim!]))
	(t (cons u (dimlist1!> (add1 u))))))


%----- GET funstions for data components. 14.01.91 -----------------------

% Get with symmetry.
%  LST - box, W - numbers list, WSS - symmetries list,
%  WT - type 0, -1 or n , WE - equation
(de gets!> (lst w wss wt we)
  (cond(wss(cond((setq w (syaidx!> w wss))(progn
		   (setq w (cond(we(getelq!> lst w))(t(getel!> lst w))))
		   (cond(![cs!] (setq w (chsign!> (not(zerop wt)) w))))
		   (cond(![ch!] (setq w (coexpr!> wt w))))
		   w))
		(t nil)))
       (t(prog2 (cond((null w)(setq w '(0))))
                (cond(we(getelq!> lst w))(t(getel!> lst w)))))))

(de gets0!> (lst w wss wt)
  (cond(wss(cond((setq w (syaidx!> w wss))(progn
		   (setq w (getel!> lst w))
		   (cond(![cs!] (setq w (chsignx!> (not(zerop wt)) w))))
		   (cond(![ch!] (setq w (coexprx!> wt w))))
		   w))
		(t nil)))
       (t(prog2 (cond((null w)(setq w '(0))))
                (getel!> lst w)))))

% Automatic Get with Symmetry. -> expr
% WI - Internal Variable, W - Index List,
(de getsa!> (wi w)
  (cond((flagp wi '!+macros2) (eval (cons (get wi '!=evf) w)))
    (t(gets!> (eval wi) w
              (get wi '!=sidxl)
              (gettype!> wi)
	      (flagp wi '!+equ)
              ))))

(de getsa0!> (wi w)
  (cond((flagp wi '!+macros2) (eval (cons (get wi '!=evf) w)))
    (t(gets0!> (eval wi) w
               (get wi '!=sidxl)
               (gettype!> wi)
	       ))))

% Cvalified Version of GETSA!> -> (type . expr)
(de getsac!> (wi w)
  (consni!> (gettype!> wi)
    (cond((flagp wi '!+macros2) (eval (cons (get wi '!=evf) w)))
      (t(gets!> (eval wi) w
                (get wi '!=sidxl)
                (gettype!> wi)
	        (flagp wi '!+equ)
                )))))

% Get 1-index. LST - box, WN - number ...
(de getel1!> (lst wn)
  (cond ((eqn wn 0) (car lst))
        ((eqn wn 1) (cadr lst))
        ((eqn wn 2) (caddr lst))
        ((eqn wn 3) (cadddr lst))
        (t (getel1!> (cddddr lst) (difference wn 4)))))

% Get 2-index. LST - box, WA,WB - numbers ...
(de getel2!> (lst wa wb)
  (getel1!>(getel1!> lst wa)wb))

% Symmetric 2-index GETEL ...
(de getel2s!> (lst wa wb)
  (cond((lessp wa wb) (getel2!> lst wa wb))
       (t             (getel2!> lst wb wa))))

% Hermitian sclar valued object ...
(de getel2h!> (lst wa wb)
  (cond((leq wa wb) (getel2!> lst wa wb))
       (t           (coalg!> (getel2!> lst wb wa)))))

% Antisymmetric 2-index GETEL ...
(de getasy2!> (lst wa wb bool)
  (cond((eqn wa wb) nil)
       ((lessp wa wb) (getel2!> lst wa wb))
       (t(chsign!> bool (getel2!> lst wb wa)))))

% General Get. LST - box, W - numbers list ...
(de getel!> (lst w)
  (cond((null(cdr w))(getel1!> lst(car w)))
       (t(getel!>(getel1!> lst(car w))(cdr w)))))

% General Get for equations ...
(de getelq!> (lst w) (get1equ!>(getel!> lst w)))

% Get LHS or RHS of the equation ...
(de get1equ!> (w)
  (cond ((null w)  nil)
        (![lsrs!] (caddr w))
        (t        (cadr w))))

% Get F in F*d x^WN element of 1-form, WN=0,1, ... 05.96
(de getfdx!> (w wn)
  (prog2 (setq wn (expt 2 (add1 wn)))
         (while!> w
           (cond ((eqn wn (cadar w)) (return(caar w)))
                 (t (setq w (cdr w)))))))
% Same but for d x/\d x ...
(de getfdxdx!> (w wl)
  (progn (setq wl (mapcar wl 'add1))
	 (setq wl (mapcar wl 'expt2!>))
	 (setq wl (eval (cons 'plus wl)))
         (while!> w
           (cond ((eqn wl (cadar w)) (return(caar w)))
                 (t (setq w (cdr w)))))))

(de expt2!> (w) (expt 2 w))

% Get 1-lower-index form with raised index ...
(de getup!> (w wa)
  (cond ((imotop!>)
           (fndfpr!> (diagmi!> wa) (getel1!> w (ai!> wa))))
        (t (dfsum!> (foreach!> m in (dimlist!> 0) collect
             (fndfpr!> (getimetr!> wa m) (getel1!> w m)))))))

% Get 1-upper-index form with index lowered ...
(de getlo!> (w wa)
  (cond((motop!>)
           (fndfpr!> (diagm!> wa) (getel1!> w (ai!> wa))))
       (t(dfsum!> (foreach!> m in (dimlist!> 0) collect
           (fndfpr!> (getmetr!> wa m) (getel1!> w m)))))))

% Get 1-upper-index alg with index lowered ...
(de getloa!> (w wa)
  (cond((motop!>)
           (mktimes2!> (diagm!> wa) (getel1!> w (ai!> wa))))
       (t(cons 'plus (foreach!> m in (dimlist!> 0) collect
           (mktimes2!> (getmetr!> wa m) (getel1!> w m)))))))

% Get WN'th element in the LST ...
(de getn!> (lst wn)
  (cond ((eqn wn 1) (car lst))
        ((eqn wn 2) (cadr lst))
        ((eqn wn 3) (caddr lst))
        ((eqn wn 4) (cadddr lst))
        (t (getn!> (cddddr lst) (difference wn 4) ))))


%--------- Specialized Gets ----------------------------------------------

% Frame ...  05.96
(de getframe!>  (w)  (getel1!> !#!T w))
(de getiframe!> (w)  (getel1!> !#!D w))

% Components of Frame/Inverse Frame ... 05.96
% In basis mode gives h^a_i with i-basis index
(de ham0!>  (wa wm) (getfdx!> (getel1!> !#!T wa) wm)) % h^a_m
(de hiam0!> (wa wm) (getfdx!> (getel1!> !#!D wa) wm)) % h_a^m

% Metric ... 05.96
(de getmetr!> (wa wb)
  (cond ((lessp wa wb) (getel2!> !#!G wa wb))
        (t             (getel2!> !#!G wb wa))))

% Inv Metric ... 05.96
(de getimetr!> (wa wb)
  (cond ((lessp wa wb) (getel2!> !#!G!I wa wb))
        (t             (getel2!> !#!G!I wb wa))))

% Riemann Tensor ...
(de getrim!> (wa wb wc wd)
  (cond ((eqn wc wd) nil)
	((lessp wc wd) (getel!> !#!R!I!M (list wa wb wc wd)))
	(t (chsigna!>  (getel!> !#!R!I!M (list wa wb wd wc))))))


%----- PUT funstions for data components. 14.01.91 -----------------------

% Put general. WE - data component, LST - box, W - numbers list ...
(de putel!> (we lst w)
  (prog2 (setq w (pgetel!> lst w))
	 (rplaca w we)))

% Put 1-index. WE - data component, LST - box, WN - number ...
(de putel1!> (we lst wn)
  (prog2 (setq wn (pgetel1!> lst wn))
	 (rplaca wn we)))

(de pgetel!> (lst w)
  (cond((null(cdr w))(pgetel1!> lst(car w)))
       (t(pgetel!>(car(pgetel1!> lst(car w)))(cdr w)))))

(de pgetel1!> (lst wn)
  (cond ((eqn wn 0) lst)
        (t(getel0!>(cdr lst)(sub1 wn)))))

(de getel0!> (lst wn)
  (cond ((eqn wn 0) lst)
        (t(getel0!>(cdr lst)(sub1 wn)))))

%--------- Symmetry ------------------------------------------------------

% Index list -> Index list in standard order,
% sign changing in [CS] and comlex conjugation in [CH]
% W - index list, WSS - symmetry list ...
(de syaidx!> (w wss)
  (progn
    (setq ![cs!] nil)
    (setq ![ch!] nil)
    (cond((null wss) w)
	 (t(prog (wr wa wb wc)
	     (setq wb wss)
	     lab
	     (setq wa (syaidx1!> w (car wss)))
	     (cond ((null wa) (return nil)))
	     (setq wr (cons wa wr))
	     (setq wss (cdr wss))
	     (cond (wss (go lab)))
	     (setq wr (reverse wr))
	     (setq wc (copy w))
	     (newidx!> wc wr wb)
	     (return wc)
             )))))

% For one groop of symmetries ...
(de syaidx1!> (w wss)
  (cond((numberp wss) (getn!> w wss))
       ((numberp(car wss)) (syaidxl!> w wss))
       ((eq (car wss) 'h) (prog (w1 w2) % Hermitian ...
	  (setq w1 (syaidx1!> w (cadr wss)))
	  (setq w2 (syaidx1!> w (caddr wss)))
	  (cond((or(null w1)(null w2)) (return nil)))
	  (cond((lessl!> w2 w1)
                 (prog2 (setq ![ch!] (not ![ch!]))
                        (return(list w2 w1)))))
	  (return(list w1 w2))))
       ((eq (car wss) 's) (prog (w1 wr wb wa wx) % Symmmetric ...
	  (setq wss (cdr wss))
	  lab1
	    (setq w1 (syaidx1!> w (car wss)))
	    (cond((null w1) (return nil)))
	    (setq wr (cons w1 wr))
	    (setq wss (cdr wss))
	  (cond(wss(go lab1)))
	  lab3
	  (setq wa nil)
	  (setq wr (reverse wr))
	  (setq wb nil)
	  lab2
	    (cond
              ((and wa (lessl!> (car wr) (car wa)))
	        (progn (setq wb t)
	               (setq wx (car wa))
		       (setq wa (cons wx (cons (car wr) (cdr wa))))
		       (setq wr (cdr wr)) ))
	      (t(progn (setq wa (cons (car wr) wa))
		       (setq wr (cdr wr)))))
	  (cond(wr(go lab2)))
	  (cond(wb (prog2 (setq wr wa) (go lab3))))
	  (return(reverse wa))))
       ((eq (car wss) 'a) (prog (w1 wr wb wa wx) % Antisymmmetric ...
	  (setq wss (cdr wss))
	  lab1
	    (setq w1 (syaidx1!> w (car wss)))
	    (cond((or (null w1) (member w1 wr)) (return nil)))
	    (setq wr (cons w1 wr))
	    (setq wss (cdr wss))
	  (cond(wss(go lab1)))
	  lab3
	  (setq wa nil)
	  (setq wr (reverse wr))
	  (setq wb nil)
	  lab2
	    (cond
              ((and wa (lessl!> (car wr) (car wa)))
	        (progn (setq wb t)
		       (setq ![cs!] (not ![cs!]))
	               (setq wx (car wa))
		       (setq wa (cons wx (cons (car wr) (cdr wa))))
		       (setq wr (cdr wr)) ))
	      (t(progn (setq wa (cons (car wr) wa))
		       (setq wr (cdr wr)))))
	  (cond(wr(go lab2)))
	  (cond(wb (prog2 (setq wr wa) (go lab3))))
	  (return(reverse wa))))
       ((eq (car wss) 'c) (prog (w1 wr wb wa wx) % Cyclic ...
	  (setq wss (cdr wss))
	  lab1
	    (setq w1 (syaidx1!> w (car wss)))
	    (cond((null w1) (return nil)))
	    (setq wr (cons w1 wr))
	    (setq wss (cdr wss))
	  (cond(wss(go lab1)))
	  (setq wr (reverse wr))
	  (setq wb (cdr wr))
	  (setq wa (ncons(car wr)))
	  lab2
	    (setq wx (append wb (reverse wa)))
	    (cond((lessl!> wx wr)(setq wr wx)))
	    (setq wa (cons (car wb) wa))
	    (setq wb (cdr wb))
	  (cond(wb(go lab2)))
	  (return wr)))   ))

% List of indices ...
(de syaidxl!> (w wss)
  (cond ((null wss) nil)
	(t (prog (wa wd)
	   (setq wa (syaidx1!> w (car wss)))
	   (cond((null wa)(return nil)))
           (setq wd (syaidxl!> w (cdr wss)))
	   (cond((and (null wd) (cdr wss)) (return nil)))
	   (return(cons wa wd))))))

% Forms final list of indices in standard order ...
(de newidx!> (w wr wss)
  (cond((null wss) nil)
       ((idp(car wss)) (newidx!> w wr (cdr wss)))
       ((numberp(car wss)) (prog2
	 (putel1!> (car wr) w (sub1(car wss)))
	 (newidx!> w (cdr wr) (cdr wss))))
       (t(prog2
	 (newidx!> w (car wr) (car wss))
	 (newidx!> w (cdr wr) (cdr wss)) ))))

% Predicate of standard oredering for Index list.
%  W - index list, WSS - Symmetry list ...
(de syaidxp!> (w wss)
  (cond(wss(prog nil
	      lab
	      (cond((null(syaidxp1!> w (car wss))) (return nil)))
	      (setq wss (cdr wss))
	      (cond(wss(go lab)))
	      (return t)))
       (t t)))

% For one symmetry groop ...
(de syaidxp1!> (w wss)
  (cond((numberp wss) (getn!> w wss))
       ((numberp(car wss)) (syaidxlp!> w wss))
       ((or (eq (car wss) 's) (eq (car wss) 'h)) (prog (w1 w2 wr)
	  lab
	  (setq wss (cdr wss))
	  (setq w2 (syaidxp1!> w (car wss)))
	  (cond((null w2)(return nil)))
	  (setq wr (cons w2 wr))
	  (cond((and w1 (not(leql!> w1 w2))) (return nil))
	       ((null(cdr wss)) (return(reversip wr))))
	  (setq w1 w2)
	  (go lab)))
       ((eq (car wss) 'a) (prog (w1 w2 wr)
	  lab
	  (setq wss (cdr wss))
	  (setq w2 (syaidxp1!> w (car wss)))
	  (cond((null w2)(return nil)))
	  (setq wr (cons w2 wr))
	  (cond((and w1 (not(lessl!> w1 w2))) (return nil))
	       ((null(cdr wss)) (return(reversip wr))))
	  (setq w1 w2)
	  (go lab)))
       ((eq (car wss) 'c) (prog (wr w1 w2)
	  lab
	  (setq wss (cdr wss))
	  (setq w2 (syaidxp1!> w (car wss)))
	  (cond((null w2)(return nil)))
	  (setq wr (cons w2 wr))
	  (cond((cdr wss) (go lab)))
	  (setq wr (reverse wr))
	  (setq w2 wr)
	  lab1
	  (setq w1 (cons (car w2) w1))
	  (setq w2 (cdr w2))
	  (cond((lessl!> (append w2 (reverse w1)) wr) (return nil))
	       ((cdr w2)(go lab1)))
	  (return wr)))))

(de syaidxlp!> (w wss)
  (cond(wss(prog (wr ww)
	      lab
	      (cond((null(setq ww (syaidxp1!> w (car wss)))) (return nil)))
	      (setq wr (cons ww wr))
	      (setq wss (cdr wss))
	      (cond(wss(go lab)))
	      (return(reversip wr))))
       (t t)))

%------ Synonymy ---------------------------------------------------------

% Defines New Synonymy ...
(dm synonymous!> (u) (list 'synonymous0!> (list 'quote (cdr u))))
(de synonymous0!> (u)
  (proc nil
    (setq ![lower!] (islowercase!>))
    (while!> u
      (synonymous1!> (car u))
      (setq u(cdr u)))))

(de synonymous1!> (u)
  (proc (w)
    (setq w (gensym))
    (cond ((iscsl!>) (setq u (mapcar u 'idtostcase!>))))
    (while!> u
      (put (car u) 'grgsyn w)
      (setq u (cdr u)))))

% Equal with synonymy ...
(de eqs!> (id1 id2)
  (cond ((and (idp id1) (idp id2))
	   (setq id1 (idtostcase!> id1))
	   (setq id2 (idtostcase!> id2))
	   (eqs0!> id1 id2))
	(t (eqs0!> id1 id2))))

(de eqs0!> (id1 id2)
    (or (eq id1 id2)
        (eqn id1 id2)
        (and (idp id1) (idp id2) (get id1 'grgsyn)
             (eq (get id1 'grgsyn) (get id2 'grgsyn)))
        (and (pairp id1) (pairp id2)
             (eqs!> (car id1) (car id2))
             (eqs!> (cdr id1) (cdr id2)))))


%----- Dimension ; declaration 05.05.96 ----------------------------------

(de dimension!> (w)
  (proc (wd wss)
    (cond ((not ![firsti!]) (setq ![er!] 88012) (return !!er!!))
          ((null w) (setq ![er!] 88011) (return !!er!!)))
    (setq wd (car w))
    (setq w (cdr w))
    (cond ((or (not(numberp wd)) (lessp wd 2))
             (setq ![er!] 8800) (return !!er!!)))
    (cond ((or (null w)
	       (not(eqs!> (car w) 'with)) % word!!!
	       (null (cdr w)))
             (setq ![er!] 88011) (return !!er!!)))
    (setq w (cdr w))
    (cond ((eqs!> (car w) 'signature) (setq w (cdr w)))) % word!!!
    (cond ((or (null w) (cdr w)) (setq ![er!] 88011) (return !!er!!))
	  (t (setq w (car w))))
    (setq w (memlist!> '!, w))
    (cond ((eq w !!er!!) (setq ![er!] 88011) (return !!er!!)))
    (setq w (expandnum!> w))
    (while!> w
      (cond
        ((equal (car w) '( !+ )) (setq wss (cons 1 wss)))
        ((equal (car w) '( !- )) (setq wss (cons -1 wss)))
	(t (setq ![er!] 88011) (return !!er!!)))
      (setq w (cdr w)))
    (cond ((not(equal wd (length wss)))
      (setq ![er!] 8801) (return !!er!!)))
    (setq ![dim!] wd)
    (setq ![sgn!] (reverse wss))
    (tunedim!>)
    (return t)))

(de expandnum!> (w)
  (cond ((null w) nil)
	((cdar w) (append (expandnum1!> (car w))
			  (expandnum!>  (cdr w))))
	(t (cons (car w) (expandnum!> (cdr w))))))

(de expandnum1!> (w)
  (cond ((numberp(car w))  (mknlist!> (car w) (ncons(cadr w))))
	((numberp(cadr w)) (mknlist!> (cadr w) (ncons(car w))))
	(t (ncons w))))

%----- Coordinates ; and Constants ; declaration 20.02.94 ----------------

% LST - Text, WN  - Internal variable, WD - Dimension ...
(de datrc!> (lst wn wd)
  (proc (w wc)
    (cond((null lst)(return nil))
         ((and wd ![cord!])(prog2(setq ![er!] 1101)(return !!er!!))))
    (setq lst(memlist!> '!, lst))
    (cond((eq lst !!er!!)(prog2(setq ![er!] 2202)(return lst))))
    (while!> lst
      (cond((or(cdar lst)(not(idp(caar lst))))
              (prog2(setq ![er!] 2201)(return !!er!!)))
           ((flagp (caar lst) '!+grg)
              (progn(setq ![er!] 5013)(doub!>(caar lst))(return !!er!!)))
           ((redused!>(caar lst))
              (progn(setq ![er!] 50130)(doub!>(caar lst))(return !!er!!)))           )
       (setq w(cons(caar lst) w))
       (setq lst(cdr lst)))
    (cond((and wd(not(eqn(length w) wd)))
      (prog2 (setq ![er!] 2203)(return !!er!!))))
    (setq wc 0)
    (setq w(reversip w))
    (set wn (append w (eval wn)))
    (flag w 'used!*)
    (flag w '!+grgvar)
    (flag w '!+grg)
    (cond ((null wd) (flag w 'constant)))
    (cond (wd
      (foreach!> x in w do (prog2
        (put x '!=cord wc)
        (setq wc (add1 wc)) ))))
    (copar1!> (ncons(eval wn)))
    (return t)   ))

%----- Affine Parameter Declaration 10.96 --------------------------------

(de affpar!> (lst)
  (cond ((null lst) nil)
	(t (prog (w)
	     (cond ((or (cdr lst) (not(idp(car lst))))
		      (setq ![er!] 1100) (return !!er!!)))
	     (setq lst (car lst))
	     (cond
               ((flagp lst '!+grg)
                 (progn (setq ![er!] 5013) (doub!> lst) (return !!er!!)))
               ((redused!> lst)
                 (progn (setq ![er!] 50130) (doub!> lst) (return !!er!!)))))
	     (setq lst (ncons lst))
	     (flag lst '!+grg)
	     (flag lst '!+grgvar)
	     (flag lst 'used!*)
	     (flag lst 'constant)
	     (setq ![apar!] lst)
	     (foreach!> x in ![cord!] do (depend (cons x lst))))))


%-----  Asy, Sy, Odd, Even declarations 20.02.94 -------------------------

(de funsym!> (lst wn)
  (proc (w)
    (cond((null lst)(return nil)))
    (setq lst(memlist!> '!, lst))
    (cond((eq lst !!er!!)(prog2(setq ![er!] 2202)(return lst))))
    (while!> lst
      (cond((or(cdar lst)(not(idp(caar lst))))
              (prog2(setq ![er!] 1100)(return !!er!!)))
           ((not(or(flagp (caar lst) '!+fun)(redgood!>(caar lst))))
              (progn(setq ![er!] 2022)(doub!>(caar lst))(return !!er!!))))
       (setq w(cons(caar lst) w))
       (setq lst(cdr lst)))
    (cond
      ((eqn wn 0) (flag w 'symmetric))
      ((eqn wn 1) (flag w 'antisymmetric))
      ((eqn wn 2) (flag w 'odd))
      ((eqn wn 3) (flag w 'even)) )
    (return t)   ))


%--------- Functions ; declaration ---------------------------------------

(de fun!> (w)
  (proc (wi wa wb wss)
    (cond ((null w) (return nil)))
    (setq w (memlist!> '!, w))
    (cond ((eq w !!er!!) (prog2(setq ![er!] 5012)(return w))))
    (setq wi (mapcar w 'car))
    (setq wa (mapcar wi 'idp))
    (cond ((memq nil wa) (prog2 (setq ![er!] 5012) (return !!er!!))))
    (setq wi (mapcar wi 'fun1!>)) % ids list ...
    (cond ((memq !!er!! wi) (return !!er!!)))
    (setq wss (remnil!> wi))
    (setq w (mapcar w 'cdr))
    (setq w (mapcar w 'fundep!>)) % dep list ...
    (cond ((memq !!er!! w) (return !!er!!)))
    (while!> w
        (flag (ncons (car wi)) '!+grgvar)
        (flag (ncons (car wi)) 'used!*)
        (setq wb (car w)) % dep list
        (cond (wb (setq wb (cons (car wi) wb))
                  (depend wb)
                  (put (car wi) '!=depend wb) ))
        (setq w  (cdr w))
        (setq wi (cdr wi)))
    (flag wss '!+fun)
    (flag wss '!+grg)
    (operator wss)
    (setq ![fun!] (append wss ![fun!]))
    (copar1!> (ncons ![fun!]))
    (return t)))

% Removes all NIL from the list W ...
(de remnil!> (w)
  (cond ((eq w nil) nil)
        ((car w) (cons(car w)(remnil!>(cdr w))))
        (t (remnil!>(cdr w)))))

% Check that W can be declared as new function ...
(de fun1!> (w)
  (cond
    ((flagp w '!+grg)  % Already used in GRG
      (progn (doub!> w) (setq ![er!] 5013) !!er!!))
    ((redbad!> w)      % Known to reduce and cannot be used in GRG
      (progn (doub!> w) (setq ![er!] 50130) !!er!!))
    ((redgood!> w)     % Known to reduce and transparent for GRG
      (progn (doub!> w) (msg!> 50131) nil))
    (t w)))

% Functions Dependence List ...
(de fundep!> (lst)
  (cond((null lst) nil)
       ((equal lst '( ( !* ) )) (copy ![cord!]))
       ((or (cdr lst) (atom(car lst)))(prog2 (setq ![er!] 5016) !!er!!))
       (t(prog nil
           (setq lst(car lst))
           (setq lst (memlist!> '!, lst))
           (cond((eq lst !!er!!)(prog2(setq ![er!] 5016)(return lst))))
           (setq lst (mapcar lst 'fundep1!>))
           (cond((memq !!er!! lst)(prog2(setq ![er!] 5016)(return !!er!!))))
           (return lst)))))

(de fundep1!> (w)
  (cond
    ((or (cdr w) (not(idp(car w))) (not(flagp (car w) '!+grgvar))) !!er!!)
    (t(car w))))

%--------- Generic Functions ; declaration -------------------------------

(de genfun!> (w)
  (proc (wi wa wss wsss wx)
    (cond ((null w) (return nil))
	  ((eq (loadpack!> '(dfpart) nil) !!er!!)
	     (setq ![er!] 5100) (return !!er!!)))
    (setq w (memlist!> '!, w))
    (cond ((eq w !!er!!) (prog2(setq ![er!] 5012)(return w))))
    (setq wi (mapcar w 'car))
    (setq wa (mapcar wi 'idp))
    (cond ((memq nil wa) (prog2 (setq ![er!] 5012) (return !!er!!))))
    (setq wi (mapcar wi 'fun1!>)) % Ids list ...
    (cond ((memq !!er!! wi) (return !!er!!)))
    (setq wss (remnil!> wi))
    (setq w (mapcar w 'cdr))
    (setq w (mapcar w 'genfundep!>)) % Dep list ...
    (cond ((memq !!er!! w) (return !!er!!)))
    (while!> w
	(setq wsss (cons (cons (car wi) (car w)) wsss))
        (setq w  (cdr w))
        (setq wi (cdr wi)))
    (setq wsss (reverse wsss))
    (flag wss 'used!*)
    (setq wx (errorset (list 'generic!_function (list 'quote wsss))
		       ![erst1!] ![erst2!]))
    (cond ((atom wx) (return !!er!!)))
    (cond (!*dfpcommute (dfp!_commute wsss)))
    (flag wss '!+grgvar)
    (flag wss '!+fun)
    (flag wss '!+grg)
    (setq wx wsss)
    (while!> wsss
      (put (caar wsss) '!=depend (car wsss))
      (setq wsss (cdr wsss)))
    (setq ![fun!] (append wss ![fun!]))
    (setq ![gfun!] (append wx ![gfun!]))
    (copar1!> (ncons ![fun!]))
    (copar1!> (ncons ![const!]))
    (return t)))

% Generic Functions Dependence List ...
(de genfundep!> (lst)
  (cond((null lst) !!er!!)
       ((or (cdr lst)(atom(car lst)))(prog2 (setq ![er!] 5016) !!er!!))
       (t(prog nil
           (setq lst (car lst))
           (setq lst (memlist!> '!, lst))
           (cond((eq lst !!er!!)(prog2(setq ![er!] 5016)(return lst))))
           (setq lst (mapcar lst 'genfundep1!>))
           (cond((memq !!er!! lst)(prog2(setq ![er!] 5016)(return !!er!!))))
           (return lst)))))

(de genfundep1!> (w)
  (cond
    ((or (cdr w) (not(idp(car w)))) !!er!!)
    ((not (flagp (car w) '!+grgvar))
       (flag w 'constant)
       (flag w '!+grg)
       (flag w '!+grgvar)
       (flag w 'used!*)
       (setq ![const!] (cons (car w) ![const!]))
       (car w))
    (t (car w))))


%------- Reduce - GRG filter 16.02.96 ------------------------------------

% W already known to Reduce as some sort of operator ...
(de redused!> (w)
  (or (get w 'simpfn)
      (get w 'infix)
      (get w 'formfn)
      (get w 'boolfn)
      (get w 'psopfn)
      (get w 'polyfn)
      (flagp w 'opfn)
      (flagp w 'boolean)
      ))

% Known to Reduce but use in GRG prohibited ...
(de redbad!> (w)
  (or (flagp w '!+redbad)
      (get w 'infix)
      (get w 'formfn)
      (get w 'boolfn)
      (get w 'psopfn)
      % (get w 'polyfn)
      % (flagp w 'opfn)
      (flagp w 'boolean)
      ))

% Known to Reduce and transparent to GRG ...
(de redgood!> (w)
  (and (or (get w 'simpfn)
	   (get w 'polyfn)
           (flagp w 'opfn))
       (not (redbad!> w)) ))


%---- Conjugated pairs. 27.12.90 -----------------------------------------

% Conj. Par. For Cord, Const and Fun ...
(de copar!> nil (copar1!> (list ![cord!] ![const!] ![fun!])))

% Conj. Par. For WE only ...
(de copar1!> (we)
     (proc (w wc wa wb wd wt)
       (while!> we
         (setq wd(setq w(car we)))
         (while!> w
           (setq wc(explode2(car w)))
           (cond((and(eqlast!~!> wc)
                     (memq(setq wa(incom!>(wipl12!> wc))) wd)
                     (or(null(get wa '!=conj))(null(get (car w) '!=conj)))                 )
             (progn
               (cond((null wt)(terpri)))
               (setq wt t)
               (setq wb(car w))
               (put wa '!=conj wb)
               (put wb '!=conj wa)
               (prin2 wa)(prin2 " & ")(prin2 wb)
               (prin2 " - conjugated pair.")
               (terpri))))
           (setq w(cdr w)))
         (setq we(cdr we)))
       (cond(wt(terpri)))
       ))

% Predicate Last Element in LST is ~ ...
(de eqlast!~!> (lst)
  (cond ((cdr lst) (eqlast!~!> (cdr lst)))
        ((eq (car lst) '!~) t)
        (t nil)))


%---------- GRG Scaner. 17.09.91 ----------------------------------------

(de listok!> (wf)
  (prog (w wa wb wa1 wa2 wc)
    (setq wa(readch!>))
    lab
    (cond ((eq wa '! ) (prog2 (setq wa (readch!>)) (go lab))))
    (cond
      ((or (memq wa wf) (eq wa !$eof!$))  % End or EOF
         (cond(wc (progn (rds nil)        % in file
			 (close wc)
			 (setq wc nil)
                         (setq wa(readch!>))))
	      (t(return(reversip w)))))    % Normal end
      ((liter wa)(prog (wf)                % Identifyer
		    (setq wb nil)
		    lab1
		    (cond
		      (wf          (setq wf nil)
                                   (setq wb (cons wa wb))
                                   (setq wa (readch!>))
                                   (go lab1))
                      ((or (liter wa) (digit wa))
                                   (setq wb (cons wa wb))
                                   (setq wa (readch!>))
                                   (go lab1))
                      ((eq wa '!!) (setq wb (cons wa wb))
				   (setq wf t)
                                   (setq wa (readch!>))
                                   (go lab1)))
                    (cond((eq wa '!~)(prog2 (setq wb (cons '!~ (cons '!! wb)))
                                            (setq wa (readch!>)))))
		    (setq w(cons(intern(compress(reversip wb))) w))
		    (setq wb nil)
                    ))
      ((digit wa)(prog nil              % Number
		    lab2
		    (cond((digit wa)(progn
                                       (setq wb(cons wa wb))
                                       (setq wa(readch!>))
				       (go lab2))))
		    (setq w(cons(compress(reversip wb)) w))
		    (setq wb nil)))
      ((eq wa '!")(cond((eq !!er!!
                  (prog nil             % String
		    (setq wb (copy '(!")))
                    (setq wa(readch!>))
		    lab3
		    (cond((not(eq wa '!"))(progn
                                            (setq wb(cons wa wb))
                                            (setq wa(readch!>))
                                            (cond((eq wa !$eof!$)(progn
                                                    (setq ![er!] 9901)
						    (rds nil)
						    (cond(wc(close wc)))
						    (setq wc nil)
                                                    (return  !!er!!))))
				            (go lab3))))
		    (setq w(cons(compress(reversip(cons '!" wb))) w))
                    (setq wa(readch!>))
                    (setq wb nil))) (return !!er!!))))
      ((flagp wa '!=fc)(progn   % Possible double symbol
	 (setq wa1 wa)
         (setq wa(readch!>))
	 (cond((and(eq wa1 '!*)(eq wa '!*))
                 (prog2(setq w(cons '!*!* w))(setq wa(readch!>)))) % **
	      ((and(eq wa1 '!_)(eq wa '!|))
                 (prog2(setq w(cons '!_!| w))(setq wa(readch!>)))) % _|
	      ((and(eq wa1 '!/)(eq wa '!\))
                 (prog2(setq w(cons '!/!\ w))(setq wa(readch!>)))) % /\
	      ((and(eq wa1 '!|)(eq wa '!=))
                 (prog2(setq w(cons '!|!= w))(setq wa(readch!>)))) % |=
	      ((and(eq wa1 '!~)(eq wa '!~))
                 (prog2(setq w(cons '!~!~ w))(setq wa(readch!>)))) % ~~
	      ((and(eq wa1 '!.)(eq wa '!.))
                 (prog2(setq w(cons '!.!. w))(setq wa(readch!>)))) % ..
	      ((and(eq wa1 '!<)(eq wa '!=))
                 (prog2(setq w(cons '!<!= w))(setq wa(readch!>)))) % <=
	      ((and(eq wa1 '!>)(eq wa '!=))
                 (prog2(setq w(cons '!>!= w))(setq wa(readch!>)))) % >=
	      ((and(eq wa1 '!-)(eq wa '!>))
                 (prog2(setq w(cons '!-!> w))(setq wa(readch!>)))) % ->
	      (t(setq w                                    % Single symbol
                 (cons(intern wa1)w)
	            )))))
     (t(prog2                          % Single symbol
         (setq w (cons (intern wa) w))
         (setq wa (readch!>)))))       % Symbol
   (go lab)))

(de readch!> nil
  (carfile!> (errorset '(readch) nil nil)))

% Via SEPRP ...
(de carfile!> (w)
   (cond ((atom w) !$eof!$)
         ((seprp(car w)) '! )
         (t (car w))))

% With ECHO ....
%(de carfile!> (w)
%  (prog2
%    (cond ((and ![echo!] (not(or (atom w) (eq (car w) !$eof!$))))
%            (prin2(car w))))
%    (cond ((atom w) !$eof!$)
%          ((seprp(car w)) '! )
%          (t (car w)))))

% With direct TAB, CR and so on ...
%(de carfile!> (w)
%   (cond((atom w)!$eof!$)
%        ((eq(car w) !$eol!$) '! )
%        ((eq(car w) '!	) '! )  % tab here and cr below !
%        ((eq(car w) '!
%) '! )
%        (t(car w))))


%---- Sequence of Commands -> Internal Representation. 20.09.91 ----------

(de collect!> (lst)
  (proc (ww wa wb)
    (setq wb 0)
    (loop!>
      (cond
	((null lst)                     % End of list
	  (cond((not(eqn wb 0)) % Bad brackets.
                 (prog2(setq ![er!] 6100)(return !!er!!)))
	       (wa (return(reversip (cons(reversip wa)ww))))
               (t (return(reversip ww))) ))
	((memq (car lst) '(!;))      % End of command
	  (cond((null wa)  % ;; encountered.
                 (prog2(setq ![er!] 9602)(return !!er!!)))
               ((not(eqn wb 0)) % Bad brackets.
                 (prog2(setq ![er!] 6100)(return !!er!!)))
	       (t(progn (setq ww (cons (reversip wa) ww))
                        (cond(lst(setq lst(cdr lst))))
			(setq wa nil))))))
      (tohead(null lst))
      (cond((eq(car lst) '!( ) (setq wb(add1 wb)))
	   ((eq(car lst) '!) ) (setq wb(sub1 wb))))
      (cond((lessp wb 0)(prog2(setq ![er!] 6100)(return !!er!!))))
      (cond((and(null wa)(memq(car lst) '(!;)))
        (prog2(setq ![er!] 9602)(return !!er!!))))
      (setq wa (cons (car lst) wa))
      (setq lst (cdr lst)))))


%========= End of GRGinit.sl ==============================================%

