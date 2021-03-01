%==========================================================================%
%   GRGtrans.sl                                        Formula Translator  %
%==========================================================================%
%   GRG 3.2 Standard Lisp Source Code       (C) 1988-96 Vadim V. Zhytnikov %
%==========================================================================%
% This file is distributed without any warranty. You may modify it but you %
% are not allowed to remove author's name and/or distribute modified file. %
%==========================================================================%


%---------- General GRG Translator ---------------------------------------

% Translation with (ERROR ...) interruption ...
(de translate1!> (lst)
  (cond (lst (unievaluate!> (unitra!> lst)))
        (t nil)))

% Translation with !!ER!! return ...
(de translate!> (lst)
  (prog nil
    (cond((null lst)(return nil)))
    (setq ![lsrs!] nil)
    (setq lst
      (errorset!> (list2 'unitra!> (list2 'quote lst))
                  ![erst1!] ![erst2!]))
    (cond((atom lst)(prog2(setq ![er!] lst)(return !!er!!))))
    (setq lst
      (errorset!> (list2 'unievaluate!> (list 'quote(car lst)))
                  ![erst1!] ![erst2!]))
    (cond((atom lst)(prog2(setq ![er!] lst)(return !!er!!))))
    (return(car lst)) ))

% Translate for equations with !!ER!! return ...
(de translateeq!> (lst)
  (prog nil
    (cond((null lst)(return nil)))
    (setq ![lsrs!] nil)
    (setq lst
      (errorset!> (list2 'unitraeq!> (list2 'quote lst))
                  ![erst1!] ![erst2!]))
    (cond((atom lst)(prog2(setq ![er!] lst)(return !!er!!))))
    (setq lst
      (errorset!> (list2 'unievaluateeq!> (list 'quote(car lst)))
                  ![erst1!] ![erst2!]))
    (cond((atom lst)(prog2(setq ![er!] lst)(return !!er!!))))
    (return(car lst)) ))

% Pre-Translation with !!ER!! return ...
(de pretrans!> (lst)
  (prog nil
    (cond ((null lst) (return nil)))
    (setq ![lsrs!] nil)
    (setq lst
      (errorset!> (list2 'unitra!> (list2 'quote lst))
                  ![erst1!] ![erst2!]))
    (cond((atom lst)(prog2(setq ![er!] lst)(return !!er!!))))
    (return(car lst)) ))

% Pre-Translation for equations with !!ER!! return ...
(de pretranseq!> (lst)
  (prog nil
    (cond((null lst)(return nil)))
    (setq ![lsrs!] nil)
    (setq lst
      (errorset!> (list2 'unitraeq!> (list2 'quote lst))
                  ![erst1!] ![erst2!]))
    (cond((atom lst)(prog2(setq ![er!] lst)(return !!er!!))))
    (return(car lst)) ))

% Pre-Translation with !!ER!! return with external vars ...
(de pretransext!> (lst)
  (prog nil
    (cond ((null lst) (return nil)))
    (setq ![lsrs!] nil)
    (setq lst
      (errorset!> (list2 'unitraext!> (list2 'quote lst))
                  ![erst1!] ![erst2!]))
    (cond((atom lst)(prog2(setq ![er!] lst)(return !!er!!))))
    (return(car lst)) ))

% Final translation with !!ER!! return ...
(de fintrans!> (lst)
  (prog nil
    (setq lst
      (errorset!> (list2 'unievaluate!> (list 'quote lst))
                  ![erst1!] ![erst2!]))
    (cond((atom lst)(prog2(setq ![er!] lst)(return !!er!!))))
    (return(car lst)) ))

% Evaluation with simplification ...
(de unievaluate!> (lst)
  (prog2 (setq lst(unieval!> lst))
         (cond((null lst) lst)
              ((zerop(car lst))(cona!> 0 (cdr lst)))
              (t(conf!>(car lst)(cdr lst))))))

% Evaluation with simplification for equations ...
(de unievaluateeq!> (lst)
  (prog (wl wr)
    (setq wl (unievaluate!>(car lst)))
    (setq wr (unievaluate!>(cdr lst)))
    (cond((and(null wl)(null wr)) (return nil))
         ((and wl wr (not(eqn(car wl)(car wr))))(err!> 2209)))
    (return
      (cond((and wl wr)(cons(car wl)(list 'equal (cdr wl) (cdr wr))))
	   (wl (cons(car wl)(list 'equal (cdr wl) nil)))
	   (wr (cons(car wr)(list 'equal nil (cdr wr)))) ))))

% Evaluation ...
(de unieval!> (lst)
  (cond((atom lst) lst)
       ((or(numberp(car lst))(pairp(car lst))(null(car lst))) lst)
       ((flagp (car lst) '!+specexec) (apply (car lst) (cdr lst)))
       (t(apply (car lst) (mapcar (cdr lst) (function unieval!>))))))

% Final value predicate ...
(de concrp!> (w)
  (cond((or(null w)(numberp(car w))) t) (t nil)))

% Final valies list predicate ...
(de concrpl!> (lst)
  (cond((null lst) t)
       ((or(null(car lst))(numberp(caar lst))) (concrpl!>(cdr lst)))
       (t nil)))

% Pre-Translation with ERR interrupt ...
(de unitra!> (lst)
  (einstsum!> (unitra0!>(expandsym!> lst)) ![extvar!]))

% Pre-Translation with ERR and external variables ...
(de unitraext!> (lst)
  (cond (![extvar!] (unitra!> lst))
	(t (prog (w we)
             (setq w (einstsum!> (unitra0!>(expandsym!> lst)) nil))
	     (setq we (freevar!> w nil))
	     (setq ![extvara!] (reverse we))
	     (return w)))))

% Pre-Translation with ERR interrupt for equations ...
(de unitraeq!> (lst)
  (cond((or (null(setq lst (seek1!> lst '!=)))
            (null(cdr lst)) (null(car lst)) )
	 (err!> 2208))
       (t(cons(unitra!>(reverse(car lst)))(unitra!>(cdr lst))))))


%---------- Einstein Summation -------------------------------------------

% This is main function ...
(de einstsum!> (lst we)
  (cond((atom lst) lst)
       ((numberp(car lst)) lst)
       ((null(freevar!> lst we)) lst) % no any free variables
       % Spacial treatment for Sum and Prod since summation
       %   variables should not be treated as free variables ...
       ((memq (car lst) '(sumexec!> prodexec!>))
         (list3(car lst)(cadr lst)(einstsum!>(caddr lst)
                                             (consmem!>(caaadr lst)we))))
       % Product of two expressions A*B. We make summation if
       %   there is the same free variables in both A and B ...
       ((flagp(car lst) '!+multop2)(prog (w1 w2 w)
         (setq w1 (freevar!> (cadr lst) we))
         (setq w2 (freevar!> (caddr lst) we))
         (setq w (intersecl!> w1 w2))
         (cond((and(null w1)(null w2))(return lst)) % no any free vars
              ((null w)(return(list3 % empty intersection => no summation
                         (car lst)
                         (einstsum!>(cadr lst)we)
                         (einstsum!>(caddr lst)we))))
              (t(return(mkeinsum!> w lst we)))))) % make new sum
       % This is function f(A). We make summation if only
       %   some free variable appear in A at least twice ...
       ((eq(car lst) 'funapply!>)(prog (w)
         (setq w (freevar1!> (caddr lst) we))
         (setq w (errsingl!> w nil))
         (cond ((null w) (return lst))
               (t (return(mkeinsum0!> w lst))))))
       % This is sum of terms. Just apply EINSTSUM> to each
       %   term independently ...
       ((eq(car lst) 'plus!>)
         (list2 (car lst) (einstsum1!> (cadr lst) we)))
       % Others ...
       (t(cons (car lst) (einstsum1!> (cdr lst) we)))))

% Just apply EINSTSUM> to the each element of list ...
(de einstsum1!> (lst we)
  (cond((null lst) nil)
       (t(cons (einstsum!> (car lst) we)
               (einstsum1!> (cdr lst) we)))))

% Make Summation for Function ...
(de mkeinsum0!> (w lst)
  (cond((null(cdr w))
         (list3 'sumexec!> (ncons w) lst))
       (t(list3 'sumexec!> (ncons(ncons(car w)))
                (mkeinsum0!> (cdr w) lst)))))

% Make Summation for product ...
(de mkeinsum!> (w lst we)
  (cond((null(cdr w))
         (list3 'sumexec!> (ncons w)
           (list3 (car lst)
                  (einstsum!>(cadr lst)(consmem!>(car w)we))
                  (einstsum!>(caddr lst)(consmem!>(car w)we)))))
       (t(list3 'sumexec!> (ncons(ncons(car w)))
                (mkeinsum!> (cdr w) lst (consmem!> (car w) we))))))

% Collects all DUMMYVAR!> variables in expr LST ...
% WE - list of vars already excluded from consideration
% Takes into account special forms like Sum, Prod ...
(de freevar!> (lst we)
  (cond((atom lst) nil)
       ((numberp(car lst)) nil)
       ((eq(car lst) 'dummyvar!>)
         (cond((not(memq(cadr lst)we))(cdr lst))))
       ((memq (car lst) '(sumexec!> prodexec!>))
         (freevar!> (caddr lst) (consmem!>(caaadr lst)we)))
       (t(appmem!>(freevar!> (car lst) we)
                  (freevar!> (cdr lst) we)))))

% Like FREEVAR> but repeated vars can be collected twice ...
(de freevar1!> (lst we)
  (cond((atom lst) nil)
       ((numberp(car lst)) nil)
       ((eq(car lst) 'dummyvar!>)
         (cond((not(memq(cadr lst)we))(cdr lst))))
       ((memq (car lst) '(sumexec!> prodexec!>))
         (freevar1!> (caddr lst) (consmem!>(caaadr lst)we)))
       (t(append(freevar1!> (car lst) we)
                (freevar1!> (cdr lst) we)))))

% Produces Error if some var in the list present only once ...
%(de errsingl!> (w wr)
%  (cond ((null w) wr)
%        ((memq (car w) wr) (errsingl!> (cdr w) wr))
%        ((memq (car w) (cdr w)) (errsingl!> (cdr w) (cons (car w) wr)))
%        (t (prog2 (doub!>(car w)) (err!> 2018)))))
% This version just removes single var from the list ...
(de errsingl!> (w wr)
  (cond ((null w) wr)
        ((memq (car w) wr) (errsingl!> (cdr w) wr))
        ((memq (car w) (cdr w)) (errsingl!> (cdr w) (cons (car w) wr)))
        (t (errsingl!> (cdr w) wr))))

% Intersections of two lists ...
(de intersecl!> (w1 w2)
  (cond((or(null w1)(null w2)) nil)
    (t(proc (w)
        (while!> w1
          (cond((memq (car w1) w2)(setq w(cons(car w1)w))))
          (setq w1(cdr w1)))
        (return w)))))


%------- Main Operations Translation -------------------------------------

%  Main sum/difference translation with [,] ...
(de unitra0!> (lst)
  (cond ((atom lst) (atomtr!> lst))  % atom
        ((and(pairp lst)(null(cdr lst)))(unitra0!>(car lst))) % next level
        (t(proc (w)
            (cond((memq '![ lst)(setq lst(vbrctr!> lst)))) % [ , ] ?
            (cond((not(memq(car lst) '(!+ !-)))            % + - translation
                    (setq lst(cons '!+ lst))))
            (setq w(mems!> '(!+ !-) (reverse lst) nil))
            (cond((eq w !!er!!) (err!> 2017)))
            (setq w(listra!>(reversip w)))
            (return(cond((null(cdr w))(car w))
                        ((concrpl!> w) (plus!> w))
                        (t(list2 'plus!> w))))))))

% List of Expressions translation with ~~ treatment ...
(de listra!> (lst)
  (proc (w)
    (while!> lst
      (cond
        ((eq(caaar lst) '!~!~)
          (cond((null w) (err!> 2110))
               ((eq(cdar lst) '!+)
                 (setq w(cons(list2 're2!>(car w))(cdr w))))
               (t(setq w(cons(list2 'im2i!>(car w))(cdr w))))))
        ((eq(cdar lst) '!+)(setq w(cons(termtr!>(caar lst))w)))
        (t(setq w(cons(list2 'minus!> (termtr!>(caar lst)))w))))
      (setq lst(cdr lst)))
    (return w)))

% Atom translation ...
(de atomtr!> (w)
  (cond ((zerop w) nil)                                % zero
        ((stringp w)(prog2(doubs!> w) (err!> 2019)))
        ((or(numberp w)(flagp w '!+grgvar))(cons 0  w))% number or variable
        ((get w '!=subind) (prog2                      % tensorial index
                             (setq w (get w '!=subind))
                             (cond((zerop w) nil)
                                  (t(cons 0 w)))))
        (t(prog (wn wi wd wss wb)                      % data component
            (setq wss w)
            (setq w (explode2 w))
            (setq wb (selid!> w nil)) % w - id  wb - indices
            (setq wn (incomiv!> w))
            (cond
	      ((flagp wn '!+macros3) % it is macro 3 scalar
		(cond (wb (doub!> wss) (err!> 2018))
		      (t (require!> (get wn '!=ndl))
                         (return (getsac!> wn nil)))))
	      ((flagp wn '!+macros2) % it is macro 2 component
		(cond ((null wb) (doub!> wss) (err!> 2018))
		      (t (return
                           (funtr!> (list (incom!> w)
                                    (addcomm!> wb wss)) nil)))))
              ((not(flagp wn '!+ivar)) % it is not an object
                (cond ((eq wss '!~!~) (err!> 2110))
                      (t (return(list2 'dummyvar!> wss))))))
            (cond((and(null wb)(get wn '!=idxl))
              (return(list2 'dummyvar!> wss))))
            (setq wi(mapcar wb 'digorerr!>)) % indixes list
            (cond((memq !!er!! wi)
              (return(list2 'dummyvar!> wss))))
            (cond((eq(goodidxl!> wi (get wn '!=idxl)) !!er!!)
              (return(list2 'dummyvar!> wss))))
            (require1!> wn)
            (cond((and ![umod!] (memq wn '(!#!b !#!e)))
              (return(cons (cond((eq wn '!#!b) 1)(t -1))
                           (mkdx!> (car wi))))))
            (return(getsac!> wn wi)) % extracting value
            ))))

(de addcomm!> (w wss)
  (cond ((null (cdr w)) (ncons(addcomm1!> (car w) wss)))
	(t (cons (addcomm1!> (car w) wss)
              (cons '!, (addcomm!> (cdr w) wss))))))

(de addcomm1!> (w wss)
   (cond ((digit w) (compress(ncons w)))
	 (t (doub!> wss) (err!> 2018))))

%  * | /\  _|  translation ...
(de termtr!> (lst)
  (prog (w wss wo)
    (cond((null lst) (err!> 2016)))
    (setq w(seek!> lst '( !* !/!\ !_!| !| !. )))
    (cond((null w) (return(quotr!> lst))))
    (setq wo (get (cadr w) '!=op2))
    (setq wss (termtr1!> (cddr w)))
    (setq w (quotr!>(reverse (car w))))
    (return (cond((and(concrp!> w)(concrp!> wss))
                    (apply wo (list2 w wss)))
                 (t (list wo w wss))))))

(de termtr1!> (lst)
  (prog (wa wb wo)
    (cond((null lst) (err!> 2016)))
    (setq wa(seek!> lst '( !* !/!\ !_!| !| !. )))
    (cond((null wa) (return(quotr!> lst))))
    (setq wo (get (cadr wa) '!=op2))
    (setq wb (termtr1!> (cddr wa)))
    (setq wa (quotr!>(reverse(car wa))))
    (return (cond((and(concrp!> wa)(concrp!> wb))
                    (apply wo (list2 wa wb)))
                 (t (list wo wa wb))))))

% / translation ...
(de quotr!> (lst)
  (cond((null lst) (err!> 2016))
       ((not(memq '!/ lst))(exptr!> lst))
       (t(prog (w)
           (setq w(memlist!> '!/ lst))
           (cond((eq w !!er!!) (err!> 2016)))
           (setq w (mapcar w 'exptr!>))
           (return(quotmk!>(car w)(cdr w)))))))

(de quotmk!> (lst1 lst2)
  (cond((null lst2) lst1)
       ((and(concrp!> lst1)(concrp!>(car lst2)))
         (quotmk!> (quoti!> lst1 (car lst2))
                   (cdr lst2)))
       (t(quotmk!> (list 'quoti!> lst1 (car lst2))
                   (cdr lst2)))))

% ** or ^ translation ...
(de exptr!> (lst)
  (prog (w wb)
    (cond((null lst) (err!> 2016)))
    (setq w(seek!> lst '(!*!* !^) ))
    (cond((null w)(return(kertr!> lst))))
    (setq wb (exptr!> (cddr w)))
    (setq w (kertr!>(reverse(car w))))
    (return (cond((and(concrp!> w)(concrp!> wb))
                    (exp!> w wb))
                 (t(list 'exp!> w wb))))))

% d # ~ translation ...
(de kertr!> (lst)
  (cond((null lst) (err!> 2015))
       ((pairp(car lst))(cond((cdr lst) (err!> 2014))
                             (t(unitra0!>(car lst)))))
       ((not(cdr lst)) (atomtr!>(car lst)))
       ((get(car lst) '!=sysfun)
         (prog (w)
           (setq w (get (car lst) '!=sysfun))
           (setq lst (kertr!> (cdr lst)))
           (return (cond((concrp!> lst) (apply w (ncons lst)))
                        (t (list2 w lst))))))
       (t(funtr!> lst t))))

% [ , ] translation
(de vbrctr!> (lst)
  (prog (wa wd w)
    (setq lst(seek1!> lst '![ ))
    (cond((null(cdr lst)) (err!> 2001)))
    (setq wa(car lst)) (setq lst(cdr lst))
    (setq lst(seek1!> lst '!] ))
    (cond((or(null lst)(null(car lst))) (err!> 2001)))
    (setq wd(cdr lst)) (setq lst(car lst))
    (setq w(seek1!> lst '!, ))
    (cond((or(null w)(null(car w))(null(cdr w))(memq '!, (cdr w)))
            (err!> 2001)))
    (return(app!> wa (cons 'vbrc!> (cons(reverse lst) wd))))))

% Function translation ...
(de funtr!> (lst bool) % bool=t - einstein summation rule is allowed
  (cond((or(null lst)(atom lst)(not(eqn(length lst)2))
           (not(idp(car lst))))
           (err!> 2021))
       ((atom(cadr lst))(err!> 2021))
       ((get (car lst) '!=spectr) % Sum Prod LHS RHS SUB Lim ...
	 (apply (get (car lst) '!=spectr) (cdr lst)))
       (t(prog (w wt wm wx)
           (cond((not(or
                   (eq(car lst) 'vbrc!>)
                   (flagp (car lst) '!+fun)
                   (redgood!> (car lst))
                   (setq wt(get(car lst) '!=macros))
                   (gettype!> (setq wt (incomiv!>(explode(car lst)))) )))
                  (prog2(doub!>(car lst)) (err!> 2022))))
           (setq w(cond(wt wt)(t (car lst)))) % wt=t - internal variable
           (setq lst(cadr lst)) % parameters list
           (setq lst(memlist!> '!, lst))
           (cond((eq lst !!er!!) (err!> 2020)))
           (cond((and wt (get wt '!=idxl))(prog2 % if internal var =>
                   (setq wm (mapcar lst 'selmani!>)) % indices manipul.
                   (cond((setq wx(orl!> wm))
                     (setq lst (mapcar lst 'delmani!>)))))))
           (setq lst (mapcar lst (function unitra0!>)))
           (return(cond((concrpl!> lst)
                         (funapply!> w lst wm))
                       (t(list 'funapply!> w lst wm))))))))


%---------- Indices Manipulations ----------------------------------------

% Selects indices manipulation prefixes ...
(de selmani!> (w)
  (cond ((eq (setq w (car w)) '!') 1)
        ((eq w '!.) 2)
        ((eq w '!^) 3)
        ((eq w '!_) 4)
        (t nil)))

% Delets iddices manipulation prefixes from expression ...
(de delmani!> (w)
  (cond ((flagp (car w) '!+indexman)
          (cond ((null(cdr w)) (err!> 2020))
                (t (cdr w))))
        (t w)))

% Indices manipulations translation ...
(de manitr!> (wf wm) % wf - int.var., wm - manip. types list
  (cond ((null(orl!> wm)) nil)
        ((null(orl!>(setq wm (manitr1!> wm (get wf '!=idxl))))) nil)
        (t wm)))

% Manipulation for one index. Prepares action ...
(de manitr1!> (wm wi) %  wm - manip.types list, wi - idxl
  (cond ((null wm) nil)
        (t (cons (manitr2!> (car wm) (car wi))
                 (manitr1!> (cdr wm) (cdr wi)) ))))

(de manitr2!> (wm wi)
  (cond
    ((null wm)     nil)
    ((enump!> wi)  nil)
    ((eqn wm 1) % ' cvalificator - up
                  (cond
		    ((and (spinp!> wi) (not(upperp!> wi)))
                       (require!> '(!#!G)) (ncons(cdr wi))) % .s -> 's
                    ((holpd!> wi)                  % .g -> 't
                       (require!> '(!#!G!I !#!D)) 9)
                    ((tetrpd!> wi)                 % .t -> 't
                       (require!> '(!#!G!I)) 1)
                    ((holpu!> wi)                  % 'g -> 't
                       (require!> '(!#!T)) 5)
                    (t nil)))
    ((eqn wm 2) % . cvalificator - down
                  (cond
                    ((and (spinp!> wi) (upperp!> wi))
		       (require!> '(!#!G)) (ncons(minus(cdr wi)))) % 's -> .s
                    ((holpu!> wi)                  % 'g -> .t
                       (require!> '(!#!G !#!T)) 10)
                    ((tetrpu!> wi)                 % 't -> .t
                       (require!> '(!#!G)) 2)
                    ((holpd!> wi)                  % .g -> .t
                       (require!> '(!#!D)) 6)
                    (t nil)))
    ((eqn wm 3) % ^ cvalificator - g up
                  (cond
                    ((spinp!> wi) (err!> 9913))
                    ((holpd!> wi)                  % .g -> 'g
                       (require!> '(!#!G!I !#!D)) 3)
                    ((tetrpd!> wi)                 % .t -> 'g
                       (require!> '(!#!G!I !#!D)) 11)
                    ((tetrpu!> wi)                 % 't -> 'g
                       (require!> '(!#!D)) 7)
                    (t nil)))
    ((eqn wm 4) % _ cvalificator - g down
                  (cond
                    ((spinp!> wi) (err!> 9913))
                    ((holpu!> wi)                  % 'g -> .g
                       (require!> '(!#!G !#!T)) 4)
                    ((tetrpu!> wi)                 % 't -> .g
                       (require!> '(!#!G !#!T)) 12)
                    ((tetrpd!> wi)                 % .t -> .g
                       (require!> '(!#!T)) 8)
                    (t nil)))
    ))

% Qualified GET data component with ind. manipulations ...
(de getmc!> (w wi wa wm)
  (cond ((zerop(gettype!> w))
           (cona1!> 0 (getm!> w wi wa wm)))
        (t (conf1!> (gettype!> w) (getm!> w wi wa wm)))))

% GET dat component with ind. manipulation ...
(de getm!> (w wi wa wm) % w - int.var. wa - ind.list wm - manipul.
  (cond ((null wa) (getsa!> w (reverse wi))) (t
  (proc (wc wg wl we wr wo wx)
    (setq wl wa) (setq wo wi)
    (while!> wm
      (cond((null(car wm)) (setq wi(cons(car wl)wi)))
           ((singlmanp!>(car wm)) (prog2 % the `diagonal' manipulation
              (setq wi (cons (rasin!> (car wl) (car wm)) wi))
              (setq wc (cons (rasco!> (car wl) (car wm)) wc)) ))
           (t(progn (setq we t)
                    (setq wx t)
                    (setq wr (getm1!> w wi wl wm)) )))
      (exitif wx)
      (setq wl (cdr wl))
      (setq wm (cdr wm)) )
    (cond((null wc)(return(cond(we wr)
                               (t(getsa!> w(cond
                                              (wo(append(reverse wo)wa))
                                              (t wa)))))))
         ((null(setq wc(mktimes!> wc))) (return nil))
         ((zerop(gettype!> w))
           (return (mktimes!>(list wc (cond(we wr)
                                           (t(getsa!> w(reverse wi))))))))
         (t(return (fndfpr!> wc (cond(we wr)
                                     (t(getsa!> w(reverse wi))))))))
    ))))

(de getm1!> (w wi wa wm)
  (proc (wc wr wt)
    (fordim!> m do (prog2
      (setq wc (rasco2!> (car wa) m (car wm)))
      (cond(wc(prog2
        (setq wt(getm!> w (cons m wi) (cdr wa) (cdr wm)))
        (cond(wt
          (setq wr (cons
            (cond((zerop(gettype!> w))
                   (mktimes!>(list2 wc wt)))
                 (t(fndfpr!> wc wt))) wr)))))))))
     (cond(wr
       (cond((zerop(gettype!> w))(return(cons 'plus wr)))
            (t(return(dfsum!> wr))))))))

% `Diagonal' manipulation predicate. So in this case we
% do not need make a sum for rasing or lowering of the index ...
(de singlmanp!> (wt) % wt - manipulation type
  (cond ((pairp wt) t)  % spinorial
        ((eqn wt 1)  (imotop!>))                   % m^ab  .t -> 't   GI
        ((eqn wt 2)  (motop!>))                    % m_ab  't -> .t   G
        ((eqn wt 3)  (and (imotop!>) (ifdiagp!>))) % g^ab  .g -> 'g   GI D
        ((eqn wt 4)  (and (motop!>) (fdiagp!>)))   % g_ab  'g -> .g   G  T
        ((eqn wt 5)  (fdiagp!>))                   % h^a_m 'g -> 't   T
        ((eqn wt 6)  (ifdiagp!>))                  % h_a^m .g -> .t   D
        ((eqn wt 7)  (ifdiagp!>))                  % h^m_a 't -> 'g   D
        ((eqn wt 8)  (fdiagp!>))                   % h_m^a .t -> .g   T
        ((eqn wt 9)  (and (imotop!>) (ifdiagp!>))) % h^am  .g -> 't   GI D
        ((eqn wt 10) (and (motop!>) (fdiagp!>)))   % h_am  'g -> .t   G  T
        ((eqn wt 11) (and (imotop!>) (ifdiagp!>))) % h^ma  .t -> 'g   GI D
        ((eqn wt 12) (and (motop!>) (fdiagp!>)))   % h_ma  't -> .g   G  T
	(t nil)))


% Index one-to-one map for `diagonl' manipulation ...
(de rasin!> (w wt) % w - index, wt - manipulation type
  (cond ((pairp wt) (difference (abs!>(car wt)) w)) % spinorial
        ((and (imnullp!>) (member wt '(1 3 9 11)))  % null inv metric
           (rasinst!> w))
        ((and (mnullp!>) (member wt '(2 4 10 12)))  % null metric
           (rasinst!> w))
        (t w)))                                     % any other

% null indices ...
(de rasinst!> (w)
  (cond ((eqn w 0) 1)
        ((eqn w 1) 0)
        ((eqn w 2) 3)
        ((eqn w 3) 2)))

% Multiplier for `diagonal' manipulation ...
(de rasco!> (w wt) % w - index, wt - manipulation type
  (cond ((pairp wt)                                    % Spinorial
	   (cond
	     ((lessp (car wt) 0) (expt -1 w))            % 's -> .s
	     (t (expt -1 (difference (car wt) w)))))     % .s -> 's
	((and (mnullp!>) (member wt '(2 4 10 12)))     % Null Metric
	   (cond
             ((eqn wt 2)  (rascost!> w))                 % m_ab   't -> .t
	     ((eqn wt 4)  (gmetr!> w (rasinst!> w)))     % g_mn   'g -> .g
	     ((eqn wt 10) (hlam!> w (rasinst!> w)))      % h_am   'g -> .t
	     ((eqn wt 12) (hlam!> (rasinst!> w) w))))    % h_ma   't -> .g
        ((and (imnullp!>) (member wt '(1 3 9 11)))     % Null Inv Metric
	   (cond
             ((eqn wt 1)  (rascost!> w))                 % m^ab   .t -> 't
	     ((eqn wt 3)  (gimetr!> w (rasinst!> w)))    % g^mn   .g -> 'g
	     ((eqn wt 9)  (huam!> w (rasinst!> w)))      % h^am   .g -> 't
	     ((eqn wt 11) (huam!> (rasinst!> w) w))))    % h^ma   .t -> 'g
        (t (rasco2!> w w wt))))                        % Any Other

% Null metric ...
(de rascost!> (w)
  (cond ((pmmm!>) (cond ((lessp w 2)  1) (t -1)) )    % +---
        (t        (cond ((lessp w 2) -1) (t  1)) )))  % -+++

% Gives the coefficient for non-daigonal index manipulation ...
(de rasco2!> (wa wm wt) % wm - summation index
  (cond ((eqn wt 1)  (getimetr!> wa wm)) % m^ab     .t -> 't    GI
        ((eqn wt 2)  (getmetr!>  wa wm)) % m_ab     't -> .t    G
        ((eqn wt 3)  (gimetr!> wa wm))   % g^ab     .g -> 'g    GI  D
        ((eqn wt 4)  (gmetr!>  wa wm))   % g_ab     'g -> .g    G   T
        ((eqn wt 5)  (ham!>  wa wm))     % h^a_m    'g -> 't    T
        ((eqn wt 6)  (hiam!> wa wm))     % h_a^m    .g -> .t    D
        ((eqn wt 7)  (hiam!> wm wa))     % h^m_a    't -> 'g    D
        ((eqn wt 8)  (ham!>  wm wa))     % h_m^a    .t -> .g    T
        ((eqn wt 9)  (huam!> wa wm))     % h^am     .g -> 't    GI  D
        ((eqn wt 10) (hlam!> wa wm))     % h_am     'g -> .t    G   T
        ((eqn wt 11) (huam!> wm wa))     % h^ma     .t -> 'g    GI  D
        ((eqn wt 12) (hlam!> wm wa))     % h_ma     't -> .g    G   T
        ))



%---------- Cvalified simplification -------------------------------------

(de cona!> (w lst)
  (cond ((or(null lst)(null(setq lst(zn!>(eval!> lst)))))nil)
        (t(cons 0 lst))))

(de conf!> (w lst)
  (cond ((or(null lst)(null(setq lst(evalform!> lst))))nil)
        (t(cons w lst))))

(de cona1!> (w lst)
  (cond ((null lst) nil)
        (t (cons 0 lst))))

(de conf1!> (w lst)
  (cond ((null lst) nil)
        (t (cons w lst))))


%------- Evaluation Functions --------------------------------------------

% Function evaluator ...
(de funapply!> (wf lst wm) % wf - function id or internal data var
  (prog (w wi wt)          % lst - paramaters, wm - index manipulation
    (setq lst (mapcar lst (function unieval!>)))
    (cond((eq wf 'vbrc!>) (return(apply 'vbrc!> lst))) % [ , ]
         ((flagp wf '!+macros)(return(apply wf (ncons lst)))) % macro tensor
         ((setq wt (gettype!> wf)) (progn                   % data component
			  % we need this data ...
                          (cond ((flagp wf '!+macros2)
                                   (require!> (get wf '!=ndl)))
                                (t (require1!> wf)))
                          (setq wi (get wf '!=idxl))
			  % translating indices ...
                          (setq lst (mapcar lst (function indextr!>)))
                          (cond
                            ((eq (goodidxl!> lst wi) !!er!!)
			       (cond
				 % index out of range ...
				 ((eqn ![er!] 21022) (err!> ![er!]))
				 ((eqn ![er!] 21023) (err!> ![er!]))
				 % wrong number of indices ...
                                 (t (return(tryexp!> wf lst wm))))))
			  % special case: b e in basis mode ...
                          (cond ((and ![umod!] (memq wf '(!#!b !#!e)))
                            (return (cons
                                      (cond ((eq wf '!#!b) 1) (t -1))
                                      (mkdx!> (car lst))))))
                          (cond
                            ((setq wm (manitr!> wf wm)) % ind. manipul.
                               (return (getmc!> wf nil lst wm)))
                            (t (return (getsac!> wf lst))))
                          )))
    % and this is really function ...
    (setq wt (mapcar lst 'auxfun2!>))
    (cond ((memq !!er!! wt) (return(trydistr!> wf lst))))
    (return (cons 0 (cons wf wt)))))

(de auxfun2!> (w)
  (cond ((null w) 0)
        ((not(zerop(car w))) !!er!!)
        (t (cdr w))))

% Function can be applied distributively to form
% or vector on one and only one argument ...
(de trydistr!> (wf lst)
  (proc (wa wb w we wt wr)
    (while!> lst
      (setq w (car lst))
      (cond ((null w) (setq w 0))
	    ((not(zerop(car w))) (go lab))
	    (t (setq w (cdr w))))
      (setq wa (cons w wa))
      (setq lst (cdr lst)))
    lab
    (setq wt (caar lst)) % type
    (setq we (cdar lst)) % form or vector expression
    (setq lst (cdr lst))
    (setq wb (mapcar lst 'auxfun2!>))
    (cond ((memq !!er!! wb) (err!> 2023)))
    (while!> we
      (setq wr (cons (cons (cons wf (app!> wa (cons (caar we) wb)))
		           (cdar we))
		     wr))
      (setq we (cdr we)))
    (return(cons wt (reversip wr)))))

% Trying expand summed indices ...
%  wf - int.var.,  wl - list of indices,  wm - list of manipulations
(de tryexp!> (wf wl wm)
  (cond ((sp!>) (err!> ![er!])) (t
    (proc (wi wll wmm wm1 wl1 wd wss)
      (setq wi (get wf '!=idxl)) % idxl
      (cond ((null wm) (setq wm (mknlist!> nil (length wi)))))
      (while!> wi
	(cond
	  ((null wl) (err!> ![er!])) % wrong number of indices
	  % Summed spinor index ...
	  ((and (spinp!>(car wi)) (greaterp (dimid!>(car wi)) 1))
	    (setq wd (dimid!>(car wi)))
	    (while!> (geq wd 1)
	      (cond ((null wl) (err!> ![er!]))
		    (t (setq wl1 (cons (car wl) wl1))
		       (setq wm1 (cons (car wm) wm1))
		       (setq wl (cdr wl))
		       (setq wm (cdr wm))))
              (setq wd (sub1 wd)))
	    (setq wll (cons (reverse wl1) wll))
	    (setq wmm (cons (reverse wm1) wmm))
            (setq wl1 nil) (setq wm1 nil))
	  % Tetrad index ...
	  ((tetrp!>(car wi))
	    (setq wd 2)
	    (while!> (geq wd 1)
	      (cond ((null wl) (err!> ![er!]))
		    (t (setq wl1 (cons (car wl) wl1))
		       (setq wm1 (cons (car wm) wm1))
		       (setq wl (cdr wl))
		       (setq wm (cdr wm))))
              (setq wd (sub1 wd)))
	    (setq wll (cons (reverse wl1) wll))
	    (setq wmm (cons (reverse wm1) wmm))
            (setq wl1 nil) (setq wm1 nil))
	  (t
	    (setq wll (cons (car wl) wll))
	    (setq wmm (cons (manitr2!> (car wm) (car wi)) wmm))
	    (setq wl (cdr wl))
	    (setq wm (cdr wm))))
	(setq wi (cdr wi)))
      (cond ((or wm wl) (err!> ![er!]))) % wrong number of indices
      (setq wi (reverse(get wf '!=idxl)))
      (setq wss (signchange!> wll wmm wi))
      (setq wm  (indexchange!> wll wmm wi))
      (setq wl (car wm))
      (setq wm (cdr wm))
      (return
        (cond (wss (minus!>(getmc!> wf nil wl wm)))
	      (t           (getmc!> wf nil wl wm))))
      ))))

(de signchange!> (wll wmm wi)
  (proc (wss)
    (while!> wll
      (cond
	((and (pairp (car wll)) (signchange1!> (car wll) (car wmm) (car wi)))
	  (setq wss (not wss)) ))
      (setq wll (cdr wll))
      (setq wmm (cdr wmm))
      (setq wi (cdr wi)))
   (return wss)))

(de signchange1!> (wl wm wi)
  (proc (wss wl1 wm1)
    (while!> wl
      (setq wm1 (car wm))
      (setq wl1 (car wl))
      (cond ((or (lessp wl1 0) (greaterp wl1 1)) (err!> 21022))
	    ((or (eqn wm1 3) (eqn wm1 4))        (err!> 9913)))
      (cond
	((and (eqn wm1 1) (not(upperp!> wi)) (eqn wl1 0)) % index up
	   (setq wss (not wss)))
	((and (eqn wm1 2) (upperp!> wi) (eqn wl1 1)) % index down
	   (setq wss (not wss))))
      (setq wl (cdr wl))
      (setq wm (cdr wm)))
    (cond ((and (tetrpd!> wi) (not(pmmm!>))) (setq wss (not wss))))
    (return wss)))

(de indexchange!> (wl wm wi)
  (proc (wll wmm)
    (while!> wl
      (cond
	((pairp(car wl))
	   (setq wmm (cons nil wmm))
	   (setq wll (cons (idxchg1!> (car wl) (car wm) (car wi)) wll)))
	(t (setq wll (cons (car wl) wll))
	   (setq wmm (cons (car wm) wmm))))
      (setq wl (cdr wl))
      (setq wm (cdr wm))
      (setq wi (cdr wi)))
    (return (cons wll wmm))))

(de idxchg1!> (wl wm wi)
  (cond ((spinp!> wi) (idxchg2!> wl wm))
	((not(member wl '((0 0)(0 1)(1 0)(1 1)))) !!er!!)
	(t (setq wl (list2 (idch1!> (car wl) (car wm))
			   (idch1!> (cadr wl) (cadr wm))))
	   (cond ((equal wl '(0 0)) 1)
	         ((equal wl '(1 1)) 0)
	         ((equal wl '(0 1)) 3)
	         ((equal wl '(1 0)) 2) ))))

(de idch1!> (wl wm)
  (cond ((and wm (eqn wl 0)) 1)
	((and wm (eqn wl 1)) 0)
	(t wl)))
(de idxchg2!> (wl wm)
  (cond ((null wl) 0)
	((car wm) (plus2 (cond ((zerop(car wl)) 1) (t 0))
			 (idxchg2!> (cdr wl) (cdr wm))))
	(t (plus2 (car wl) (idxchg2!> (cdr wl) (cdr wm)))) ))

% Index for data component translation ...
(de indextr!> (w)
  (cond((null w) 0)
       ((not(zerop(car w))) (err!> 20231))
       ((or(not(numberp(setq w(nz!>(eval!>(cdr w))))))
           (lessp w 0)) (err!> 2102))
       (t w)))

% Dummy variable evaluation ...
(de dummyvar!> (w)
  (cond ((get w '!=subind) (prog2 (setq w (get w '!=subind))
                                  (cond((zerop w) nil)
                                       (t(cons 0 w)))))
        (t(prog2(doub!> w) (err!> 2018)))))

% _| execution
(de inpr!> (lst1 lst2)
  (cond ((or(null lst1)(null lst2)) nil)
        ((not(eqn(car lst1) -1)) (err!> 2002))
        ((eqn(car lst2) -1) (err!> 2003))
        ((eqn(car lst2) 0) (err!> 2003))
        ((eqn(car lst2) 1)
          (cona1!> 0 (vform1!>(cdr lst1)(cdr lst2))))
        (t(conf1!> (sub1(car lst2))
                 (vform!>(cdr lst1)(cdr lst2))))))

% | execution
(de vef!> (lst1 lst2)
  (cond ((or(null lst1)(null lst2)) nil)
        ((not(eqn(car lst1) -1)) (err!> 20021))
        ((not(zerop(car lst2))) (err!> 20031))
        (t (cona1!> 0 (vfun!>(cdr lst1)(cdr lst2))))))

% . execution
(de vpr!> (lst1 lst2)
  (cond ((or (null lst1) (null lst2)) nil)
	((and (eqn (car lst1) -1) (eqn (car lst2) -1))
	   (require!> '( !#!T !#!G ))
           (cona1!> 0 (vprod!> (cdr lst1) (cdr lst2))) )
	((and (eqn (car lst1) 1) (eqn (car lst2) 1))
	   (require!> '( !#!D !#!G!I ))
           (cona1!> 0 (fprod!> (cdr lst1) (cdr lst2))) )
	((and (eqn (car lst1) -1) (eqn (car lst2) 1))
           (cona1!> 0 (vform1!> (cdr lst1) (cdr lst2))) )
	((and (eqn (car lst1) 1) (eqn (car lst2) -1))
           (cona1!> 0 (vform1!> (cdr lst2) (cdr lst1))) )
        (t (err!> 2030))))

% d execution
(de dx!> (lst)
  (cond ((null lst) nil)
        ((minusp(car lst)) (err!> 2004))
        ((and(eqn(car lst)0)(idp(cdr lst))(get(cdr lst) '!=cord))
          (cons 1 (cond(![umod!](getel1!> ![xf!] (get (cdr lst) '!=cord)))
                       (t      (mkdx!>(get (cdr lst) '!=cord))))))
        ((eqn(car lst) 0) (conf1!> 1(dfun!>(cdr lst))))
        (t(conf1!> (add1(car lst))
                  (dex!>(cdr lst))))))

% @ X execution
(de bvec!> (lst)
  (cond ((null lst) nil)
        ((not(zerop(car lst))) (err!> 2013))
        ((and(idp(cdr lst))(get (cdr lst) '!=cord))
          (cons -1 (cond (![umod!]
                            (getel1!> ![xv!] (get(cdr lst) '!=cord)))
                         (t  (mkdx!>(get (cdr lst) '!=cord))))))
        (t(err!> 2013))))

% # execution
(de dualis!> (lst)
  (cond ((null lst) nil)
        ((eqn (car lst) -1) (err!> 2007))
        ((eqn (car lst) 0) (prog2
          (require!> '(!#!V!O!L))
          (conf1!> ![dim!] (dual0!>(cdr lst)))))
        ((eqn (car lst) ![dim!]) (prog2
          (require!> '(!#!V!O!L))
          (cona1!> 0 (duald!>(cdr lst)))))
        (t (prog2
          (require!> '(!#!T !#!G !#!V!O!L))
          (conf1!> (difference ![dim!] (car lst)) (dual!>(cdr lst)))))))

% / execution
(de quoti!> (lst1 lst2)
  (cond ((null lst2) (err!> 2009))
        ((null lst1) nil)
        ((not(zerop(car lst2))) (err!> 2011))
        ((zerop(car lst1))
          (cona1!> 0 (list 'quotient (cdr lst1) (cdr lst2))))
        (t(conf1!> (car lst1)
                  (fndfpr!> (list 'quotient 1 (cdr lst2))
                            (cdr lst1))))))

% + execution
(de plus2!> (lst1 lst2)
  (cond((null(setq lst1(unieval!> lst1))) (unieval!> lst2))
       ((null(setq lst2(unieval!> lst2))) lst1)
       ((not(eqn(car lst1)(car lst2))) (err!> 2012))
       ((zerop(car lst1)) (cona1!> 0 (list 'plus(cdr lst1)(cdr lst2))))
       (t(conf1!>(car lst1)(dfsum!>(list2(cdr lst1)(cdr lst2)))))))

% + execution
(de plus!> (lst)
  (prog (w wt wa)
    (foreach!> x in lst do
      (cond((setq wa(unieval!> x))(progn
               (cond((null wt)(setq wt(car wa))))
               (cond((not(eqn wt(car wa))) (err!> 2012)))
               (setq w(cons(cdr wa)w))))))
    (return(cond((null w) nil)
                ((zerop wt)(cona1!> 0 (cons 'plus w)))
                (t(conf1!> wt (dfsum!> w)))))))

% * execution
(de times2!> (lst1 lst2)
  (cond ((or(null lst1)(null lst2)) nil)
        ((and(zerop(car lst1))(zerop(car lst2)))
          (cona1!> 0 (list 'times (cdr lst1)(cdr lst2))))
        ((and(zerop(car lst1))(not(zerop(car lst2))))
          (conf1!> (car lst2)(fndfpr!> (cdr lst1)(cdr lst2))))
        ((and(zerop(car lst2))(not(zerop(car lst1))))
          (conf1!> (car lst1)(fndfpr!> (cdr lst2)(cdr lst1))))
        (t (err!> 2010))))

(de times22!> (lst1 lst2)
  (cond ((or(null lst1)(null lst2)) nil)
        ((or(null(setq lst1 (unieval!> lst1)))
            (null(setq lst2 (unieval!> lst2)))) nil)
        ((and(zerop(car lst1))(zerop(car lst2)))
          (cona1!> 0 (list 'times (cdr lst1)(cdr lst2))))
        ((and(zerop(car lst1))(not(zerop(car lst2))))
          (conf1!> (car lst2)(fndfpr!> (cdr lst1)(cdr lst2))))
        ((and(zerop(car lst2))(not(zerop(car lst1))))
          (conf1!> (car lst1)(fndfpr!> (cdr lst2)(cdr lst1))))
        (t (err!> 2010))))

% - execution
(de minus!> (lst)
  (cond ((null lst) nil)
        ((zerop(car lst)) (cons 0 (chsign!> nil (cdr lst))))
        (t(cons (car lst)(chsign!> t (cdr lst))))))

% ~ execution
(de co!> (lst)
  (cond ((null lst) nil)
        ((zerop(car lst)) (cons 0 (coalg!> (cdr lst))))
        ((eqn(car lst) -1) (cons -1 (covec!> (cdr lst))))
        (t(cons (car lst)(coform!> (cdr lst))))))

% re=(expr+~expr)/2 execution
(de re!> (lst)
  (cond((cdr lst) (err!> 2105))(t
    (times2!> '(0 quotient 1 2)
              (plus2!>(car lst)(co!>(car lst)))))))

% expr+~~=expr+~expr   execution
(de re2!> (lst)  (plus2!> lst (co!> lst)))

% im=-i * (expr-~expr)/2 execution
(de ima!> (lst)
  (cond((cdr lst) (err!> 2105))(t
    (times2!> '(0 quotient(minus i)2)
              (plus2!>(car lst)(minus!> (co!>(car lst))))))))

% expr-~~ = expr-~expr  execution
(de im2i!> (lst) (plus2!> lst(minus!> (co!> lst))))

% /\ execution
(de dfpr2!> (lst1 lst2)
  (cond ((or(null lst1)(null lst2)) nil)
        ((or(lessp(car lst1)1)(lessp(car lst2)1)) (err!> 2005))
        (t(conf1!> (plus(car lst1)(car lst2))
                  (dfprod2!> (cdr lst1)(cdr lst2))))))

% [ , ] execution
(de vbrc!> (lst1 lst2)
  (cond ((or(null lst1)(null lst2)) nil)
        ((or(not(minusp(car lst1)))(not(minusp(car lst2))))
          (err!> 2006))
        (t(conf1!> -1 (vbrack!> (cdr lst1)(cdr lst2))))))

% ** execution
(de exp!> (lst1 lst2)
  (cond((null lst1) nil)
       ((not(zerop(car lst1))) (err!> 2008))
       ((null lst2) '(0 . 1))
       ((not(zerop(car lst2))) (err!> 2008))
       (t(cona1!> 0 (list 'expt (cdr lst1)(cdr lst2))))))


%---------- SUM translator. 08.01.91 -------------------------------------

% SUM translation ...
(de sumtr!> (lst)
  (prog (w)
    (setq lst(memlist!> '!, lst))
    (cond((eq lst !!er!!) (err!> 2020))
         ((null(cdr lst)) (err!> 2103)))
    (setq lst (reverse lst))
    (setq w (car lst))
    (setq lst (itercon!>(reverse(cdr lst))))
    (cond((eq lst !!er!!) (err!> 2103)))
    (setq lst (append lst (ncons w)))
    (return(sumtr1!> lst nil))))

(de sumtr1!> (lst bool)
  (cond((null(cdr lst))(cond((eq bool 'func)(funtr!>(car lst)nil))
                            ((eq bool 'term)(termtr1!>(car lst)))
                            (t(unitra0!> lst))))
       (t(list 'sumexec!> (car lst)
                          (sumtr1!> (cdr lst) bool)))))

% SUM Execution ...
(de sumexec!> (wi we)
  (proc (w wr)
    (setq wi(itertr!> wi (cond(![ivs!] (car ![ivs!]))(t t))))
    (while!> wi
      (put (caar wi) '!=subind (cdar wi))
      (setq ![ivs!] (cons (cdar wi) ![ivs!]))
      (setq wr (errorset!> (list 'plus2!> (list 'quote we)(list 'quote w))
                           ![erst1!] ![erst2!]))
      (remprop (caar wi) '!=subind)
      (cond(![ivs!] (setq ![ivs!] (cdr ![ivs!]))))
      (cond((atom wr) (err!> wr)))
      (setq w (car wr))
      (setq wi(cdr wi)))
    (return w)))


%---------- PROD translator 02.03.94 -------------------------------------

% Prod Translation ...
(de prodtr!> (lst)
  (prog (w)
    (setq lst(memlist!> '!, lst))
    (cond((eq lst !!er!!) (err!> 2020))
         ((null(cdr lst)) (err!> 2103)))
    (setq lst (reverse lst))
    (setq w (car lst))
    (setq lst (itercon!>(reverse(cdr lst))))
    (cond((eq lst !!er!!) (err!> 2103)))
    (setq lst (append lst (ncons w)))
    (return(prodtr1!> lst nil))))

(de prodtr1!> (lst bool)
  (cond((null(cdr lst))(cond((eq bool 'func)(funtr!>(car lst)nil))
                            ((eq bool 'term)(termtr1!>(car lst)))
                            (t(unitra0!> lst))))
       (t(list 'prodexec!> (car lst)
                          (prodtr1!> (cdr lst) bool)))))

% PROD Execution ...
(de prodexec!> (wi we)
  (proc (w wr)
    (setq wi(itertr!> wi (cond(![ivs!] (car ![ivs!]))(t t))))
    (setq w '(0 . 1))
    (while!> wi
      (put (caar wi) '!=subind (cdar wi))
      (setq ![ivs!] (cons (cdar wi) ![ivs!]))
      (setq wr (errorset!> (list 'times22!> (list 'quote we)(list 'quote w))
                           ![erst1!] ![erst2!]))
      (remprop (caar wi) '!=subind)
      (cond(![ivs!] (setq ![ivs!] (cdr ![ivs!]))))
      (cond((atom wr) (err!> wr)))
      (setq w (car wr))
      (setq wi(cdr wi)))
    (return w)))


%----- Iterator translation for SUM/PROD and Print -----------------------

% Main Iterator translation ...
(de itertr!> (lst wp)
  (prog (wa wc w)
    (setq wa (car lst))
    (setq wc (cdr lst))
    (cond((not(idp(car wa))) (err!> 21031))
         ((flagp (car wa) '!+grgvar)(msg!> 2109)))
    (cond
      ((null(cdr wa))(return(iditertr!> (car wa) wc wp))) % j or j1 or j02
      ((not(eq(cadr wa) '!=)) (err!> 21031))
      ((not(memq '!.!. (cddr wa)))   % j = a
        (return(mkiter!> (car wa) 0 (boundtr!>(cddr wa)) wc wp)))
      (t(progn   % j = a _ b
          (setq w (car wa))
          (setq wa (seek1!> (cddr wa) '!.!. ))
          (cond((or(null(car wa))(null(cdr wa))) (err!> 21031)))
          (return
            (mkiter!> w (boundtr!>(reverse(car wa)))
                        (boundtr!>(cdr wa)) wc wp)))))))

% Iterator in the form of single identifier j or j1 or j02 ...
(de iditertr!> (wi wc wp)
  (prog (wa wd)
    (setq wa (explode2 wi))
    (cond ((not (liter (car wa))) (doub!> wi) (err!> 2104)))
    (setq wd (selid!> wa nil)) % wd - numbers wa - atom
    % we cut trailing ~ , we do not care about it ...
    (setq wd (wipe!~!> wd))
    (cond
      % j12d = 0 .. (dim-1)
      ((notalldig!> wd)
        (return (mkiter!> wi 0 ![dim1!] wc wp)))
      % j = 0 .. (dim-1)
      ((and (null wd) (get (car wa) '!=uc))
        (return (mkiter!> wi 0 ![dim1!] wc wp)))
      % abc = 0 .. length(abc)
      ((and (null wd) (get (car wa) '!=lc))
        (return (mkiter!> wi 0 (length wa) wc wp)))
      % j3 = 0 .. 3
      ((null (cdr wd))
        (return (mkiter!> wi 0 (compress wd) wc wp)))
      % j13 = 1 .. 3
      ((null(cddr wd))
        (progn (setq wa (compress (ncons (car wd))))
               (setq wd (compress (cdr wd)))
               (return (mkiter!> wi wa wd wc wp))))
      (t(err!> 2108)))))

(de wipe!~!> (w)
  (cond ((null w) nil)
	((eq (car w) '!~) nil)
	(t (cons (car w) (wipe!~!> (cdr w))))))

(de notalldig!> (w)
  (cond ((null w) nil)
        ((not (digit (car w))) t)
        (t (notalldig!>(cdr w)))))

% Bound translation ...
(de boundtr!> (lst)
  (progn (cond((null lst) (err!> 21031)))
         (setq lst(translate!>(ncons lst)))
         (cond((eq lst !!er!!) (err!> ![er!]))
              ((null lst) 0)
              ((or(not(zerop(car lst)))
                  (not(numberp(cdr lst))))
                (err!> 2108))
              (t(cdr lst)))))

% Prepares Iterator ...
(de mkiter!> (id wi wf wc wp) % wc-comparison with wp ...
  (proc (w)                   % wi wf - up/lo bounds ...
    (cond((lessp wf wi)(prog2(msg!> 2104)(return nil))))
    (loop!>
      (cond((or(null wc)(validit!> wi wc wp))
        (setq w(cons(cons id wi) w))))
      (exitif (eqn wi wf))
      (setq wi(add1 wi)))
    (return w)))

% Compare by  <  >  <=  >=  ...
(de validit!> (wi wc wp)
  (cond ((eqn wc 1)(lessp wp wi))
	((eqn wc 2)(greaterp wp wi))
	((eqn wc 3)(leq wp wi))
	((eqn wc 4)(geq wp wi))
	(t t)))


%-------- LHS and RHS ----------------------------------------------------

(de lhs0!> (lst)
    (prog2(setq ![lsrs!] nil)(list2 'lhs!> (unitra0!> lst))))
(de rhs0!> (lst)
    (prog2(setq ![lsrs!] t)(list2 'rhs!> (unitra0!> lst))))

(de lhs!> (w) (prog2 (setq ![lsrs!] nil) (unieval!> w)))
(de rhs!> (w) (prog2 (setq ![lsrs!] t)   (unieval!> w)))


%--------- Asy Sy Cy expansion 6.03.94 -----------------------------------

(de allcy!> (lst)
  (proc (wi w)
    (while!> lst
      (setq w (cons (ncons(append lst (reverse wi))) w))
      (setq wi (cons (car lst) wi))
      (setq lst (cdr lst)))
    (return w)))

(de allasy!> (lst)
  (cond ((or(null lst)(null(cdr lst))) nil)
	((null(cddr lst)) (all2y!> lst t))
	(t(add1y!> t (car lst) (allasy!>(cdr lst))))))

(de allsy!> (lst)
  (cond ((or(null lst)(null(cdr lst))) nil)
	((null(cddr lst)) (all2y!> lst nil))
	(t(add1y!> nil (car lst) (allsy!>(cdr lst))))))

(de all2y!> (lst wt)
  (list2 (cons(list2(cadr lst)(car lst))wt)
	 (ncons(list2(car lst)(cadr lst)))))

(de add1y!> (wt w lst)
  (proc (wr)
    (while!> lst
      (setq wr (add11y!> wt w (car lst) wr))
      (setq lst (cdr lst)))
    (return wr)))

(de add11y!> (wtt w wl wr)
  (proc (wt wi)
    (setq wt (cdr wl))
    (setq wl (car wl))
    (while!> wl
      (setq wr (cons (cons(app!> wi (cons w wl))wt) wr))
      (setq wt (cond(wtt(not wt))(t nil)))
      (setq wi (cons (car wl) wi))
      (setq wl (cdr wl)))
    (setq wr (cons (cons(app!> wi (cons w wl))wt) wr))
    (return wr)))

(de expandsym!> (lst)
  (cond(!*expandsym (expandsym0!> lst))(t lst)))

(de expandsym0!> (lst)
  (cond
    ((atom lst) lst)
    (t(prog (w)
	(while!> lst
	  (cond
            ((memq (car lst) '(!A!s!y !S!y !C!y))(progn
	      (cond((or(null(cdr lst))(atom(cadr lst)))(err!> 6200)))
	      (setq w (cons (expandsym1!>(car lst)(cadr lst)) w))
	      (setq lst (cdr lst))))
	    (t(setq w (cons (expandsym0!>(car lst)) w))))
	 (setq lst (cdr lst)))
	 (return(reversip w))))))

(de expandsym1!> (w lst)
  (proc (we we wi wr)
    (setq lst (memlist!> '!, lst))
    (cond((or(eq lst !!er!!)(null(cdr lst)))(err!> 6200)))
    (setq lst (reverse lst))
    (setq we (expandsym0!>(car lst)))
    (setq lst (mapcar (cdr lst) 'idorerr!>))
    (setq wi lst)
    (setq lst (cond((eq w '!A!s!y )(allasy!> wi))
		   ((eq w '!S!y   )(allsy!>  wi))
		   ((eq w '!C!y   )(allcy!>  wi))))
    (while!> lst
      (setq wr (cons (cond((cdar lst) '!-)(t '!+)) wr))
      (setq wr (cons (mkreplace!> (pair wi (caar lst)) we) wr))
      (setq lst (cdr lst)))
    (return(reversip wr))))

(de idorerr!> (w)
  (cond((or(cdr w)(not(idp(car w))))(err!> 6200))
       (t(car w))))

(de mkreplace!> (w lst)
  (cond((atom lst)
         (cond((setq w (assoc lst w))(cdr w))
	      (t lst)))
       (t(proc (wr)
	   (while!> lst
	     (setq wr (cons (mkreplace!> w (car lst)) wr))
	     (setq lst (cdr lst)))
	   (return(reversip wr))))))

%-----------  DF in prefix form 05.96 ------------------------------------

%(de pdftra!> (w)  (invord!> w 'df))

%(de dfptra!> (w)  (invord!> w 'dfp))

%(de invord!> (w wf)
%  (proc (wa wr)
%    (while!> w
%      (cond
%        ((eq (car w) '!,)
%          (setq wr (append (cons '!, (reverse wa)) wr))
%	   (setq wa nil)
%	   (setq w (cdr w)))
%	(t (setq wa (cons (car w) wa))
%	   (setq w (cdr w)))))
%    (setq wr (append (reverse wa) wr))
%    (return (funtr!> (list2 wf wr) t))))

%-----------  Limits  6.03.94 --------------------------------------------

%(de limtr!> (lst) (limtra!> nil lst))
%(de limtrm!> (lst) (limtra!> 'm lst))
%(de limtrp!> (lst) (limtra!> 'p lst))

%(de limtra!> (wt lst)
%  (prog (wx wl)
%    (cond((not(or(flagp 'limit 'opfn)(get 'limit 'simpfn)))
%      (err!> 6201)))
%    (setq lst (memlist!> '!, lst))
%    (cond((or (eq lst !!er!!) (null(cdr lst)) (cddr lst)
%	      (not(idp(caar lst))) (not(eq(cadar lst) '!-!>))
%	      (null(caddar lst)))
%	   (err!> 6202)))
%    (setq wx (caar lst))
%    (cond((not(flagp wx '!+grgvar))
%      (prog2(doub!> wx) (err!> 2018))))
%    (setq wl (unitra0!>(cddar lst)))
%    (setq lst (unitra0!>(cdr lst)))
%    (return(list 'limexec!> (list wx wl wt) lst))))
%
% wx - limiting var  wl - limiting point  wt - limit's type
%(de limexec!> (ww lst)
%  (prog (wx wl wt)
%    (setq wx (car ww))
%    (setq wl (cadr ww))
%    (setq wt (caddr ww))
%    (setq wl (unieval!> wl))
%    (cond((or(null wl)(zerop wl))(setq wl 0))
%	 ((not(zerop(car wl))) (err!> 6203))
%	 (t(setq wl (cdr wl))))
%    (setq lst (unieval!> lst))
%    (return
%      (cond((null lst) nil)
%	   ((zerop(car lst))(cona1!> 0 (lima!> wx wl wt (cdr lst))))
%	   (t(conf1!>(car lst)(limf!> wx wl wt (cdr lst))))))))


%------- SUBstitutions 7.03.94 -------------------------------------------

(de subtr!> (lst)
  (prog (wl)
    (setq lst (memlist!> '!, lst))
    (cond((eq lst !!er!!) (err!> 6204)))
    (setq lst (reverse lst))
    (setq wl (cdr lst))
    (setq lst (unitra0!>(car lst)))
    (setq wl (mapcar wl 'subtr1!>))
    (setq wl (reversip wl))
    (return(list 'subexec!> wl lst))))

(de subtr1!> (w)
  (prog (ww)
    (setq ww w)
    (setq w (seek1!> w '!=))
    (cond
      ((null w)
	(cond((or(atom ww)(not(eq (car ww) '!S!o!l))) (err!> 6204))
	     (t(progn
		 (setq w (soltra!> ww))
		 (cond((eq w !!er!!)(err!> ![er!])))
		 (return w)))))
      ((or (null(car w)) (null(cdr w))) (err!> 6204))
      (t(return(cons (unitraa!>(reverse(car w))) (unitra!>(cdr w))))))))

(de subexec!> (wl lst)
  (cond((null(setq lst (unieval!> lst))) nil)
       ((zerop(car lst))
         (cona1!> 0 (subalg!>(mapcar wl 'subexec1!>)(cdr lst))))
       (t(conf1!>(car lst)(subdf!>(mapcar wl 'subexec1!>)(cdr lst))))))

(de subexec1!> (w)
  (prog (ww)
    (cond((eq (car w) 'equal)(return w)))
    (setq ww (unieval!>(cdr w)))
    (cond((null ww)(setq ww nil))
	 ((not(zerop(car ww))) (err!> 6205))
	 (t(setq ww (cdr ww))))
    (return (list 'equal (nz!>(car w)) (nz!> ww)))))


%------- If and boolean expressions. 19.03.94 ----------------------------

(de iftran!> (lst)
  (cond
    ((eq (setq lst (memlist!> '!, lst)) !!er!!) (err!> 8200))
    ((eqn (length lst) 2)  % if ... then ...
      (list 'ifexec!> (booltrai!>(car lst))
		      (unitra0!>(cadr lst)) nil))
    ((eqn (length lst) 3)  % if ... then ... else ...
      (list 'ifexec!> (booltrai!>(car lst))
		      (unitra0!>(cadr lst))
		      (unitra0!>(caddr lst)) ))
    (t(err!> 8200))))

(de booltrai!> (lst)
  (cond ((atom lst) (atomtrabi!> lst))   % atom
        ((and(pairp lst)(null(cdr lst))) % next level
          (booltrai!>(car lst)))
        (t(prog (w)                      % or - translation ...
            (setq w (memlist!> '!o!r lst))
            (cond((eq w !!er!!) (err!> 2400))
		 ((null(cdr w))(return(andtrai!> lst))))
            (return(list2 'orex!> (mapcar w 'andtrai!>)))))))

(de andtrai!> (lst)
  (cond ((null lst) (err!> 2400))
	((null(cdr lst))
          (booltrai!>(car lst)))         % next level
        (t(prog (w)                      % and - translation ...
            (setq w (memlist!> '!a!n!d lst))
            (cond((eq w !!er!!) (err!> 2400))
		 ((null(cdr w))(return(nottrai!> lst)))) % bool function
            (return(list2 'andex!> (mapcar w 'nottrai!>)))))))

(de nottrai!> (lst)
  (cond ((null lst) (err!> 2400))
	((null(cdr lst))
          (booltrai!>(car lst)))         % next level
	((and (idp(car lst)) (get (car lst) '!=boolmac))
	  (list2 (get (car lst) '!=boolmac)
                 (list2 'quote (cadr lst))))
	((eq (car lst) '!n!o!t)
	  (list2 'notex!> (reltrai!>(cdr lst))))
	(t(reltrai!> lst))))

(de reltrai!> (lst)
  (cond ((null lst) (err!> 2400))
	((null(cdr lst))
          (booltrai!>(car lst)))         % next level
	(t(prog (w wa wb)
	    (setq w (seek!> lst '( != !< !> !<!= !>!= !|!= )))
	    (cond((null w) (return(algtra1i!> lst)))
		 ((or (null(car w)) (null(cddr w))) (err!> 2400)))
	    (setq wa (unitra0!>(reverse(car w))))
	    (setq wb (unitra0!>(cddr w)))
	    (setq w (cadr w))
	    (cond
	      ((eq w '!=)   (setq w 'equal))
	      ((eq w '!<)   (setq w 'lessp))
	      ((eq w '!>)   (setq w 'greaterp))
	      ((eq w '!|!=) (setq w 'neq))
	      ((eq w '!<!=) (setq w 'leq))
	      ((eq w '!>!=) (setq w 'geq)))
	    (return(list 'relex!> w wa wb))))))

(de algtra1i!> (lst)
  (list 'balgex!> (unitra0!> lst)))

(de atomtrabi!> (lst)
  (list 'balgex!> (atomtr!> lst)))

(de ifexec!> (wc wa wb)
  (cond((booleval!> wc) (unieval!> wa))
       (t               (unieval!> wb)) ))

(de booleval!> (lst)
  (cond((or (atom lst) (numberp(car lst)) (pairp(car lst))) lst)
       ((and (idp(car lst)) (flagp (car lst) '!+specbexe))
	  (eval lst))
       (t (apply (car lst) (mapcar (cdr lst) (function booleval!>))))))

(de balgex!> (w)
  (cond((unievaluate!> w) t)
       (t nil)))

(de orex!> (w)
  (proc nil
    (while!> w
      (cond((booleval!>(car w)) (return t)))
      (setq w (cdr w)))
    (return nil)))

(de andex!> (w)
  (proc nil
    (while!> w
      (cond((null(booleval!>(car w))) (return nil)))
      (setq w (cdr w)))
    (return t)))

(de notex!> (w) (not(booleval!> w)))

(de n00!> (w) (cond(w w)(t '(0 . 0))))

(de relex!> (w wa wb)
  (progn
    (setq wa (n00!>(unievaluate!> wa)))
    (setq wb (n00!>(unievaluate!> wb)))
    (cond((or (not(zerop(car wa))) (not(zerop(car wb)))
	      (not(numberp(cdr wa))) (not(numberp(cdr wb))) )
	    (err!> 8201)))
    (setq wa (cdr wa))
    (setq wb (cdr wb))
    (cond
      ((eq w 'equal)    (eqn wa wb))
      ((eq w 'lessp)    (lessp wa wb))
      ((eq w 'greaterp) (greaterp wa wb))
      ((eq w 'neq)      (not(eqn wa wb)))
      ((eq w 'leq)      (leq wa wb))
      ((eq w 'geq)      (geq wa wb))
    )))

(de prepiv!> (w)
  (cond ((or (not(pairp w)) (not(idp(car w)))) (err!> 2400))
	(t (incomiv!> (explode2(car w))))))

(de prepsw!> (w)
  (cond ((or (not(pairp w)) (not(idp(car w)))) (err!> 2400))
	(t (makeswvar!>(car w)))))

(de objexe!> (w)
  (prog2
    (setq w (prepiv!> w))
    (cond ((flagp w '!+ivar) t)
	  (t nil))))

(de onexe!> (w)
  (prog nil
    (setq w (prepsw!> w))
    (cond ((not(or (globalp w) (fluidp w))) (err!> 2420)))
    (return(eval w))))

(de offexe!> (w)
  (prog nil
    (setq w (prepsw!> w))
    (cond ((not(or (globalp w) (fluidp w))) (err!> 2420)))
    (return(not(eval w)))))

(de valexe!> (w)
  (prog nil
    (setq w (prepiv!> w))
    (cond ((not(flagp w '!+ivar)) (err!> 2410)))
    (return(eval w))))

(de zeroexe!> (w)
  (prog nil
    (setq w (prepiv!> w))
    (cond ((not(flagp w '!+ivar)) (err!> 2410)))
    (return(equal (eval w) (mkbox!> w)))))

(de nullexe!> (w)
  (prog nil
    (setq w (prepiv!> w))
    (cond ((not(flagp w '!+ivar)) (err!> 2410)))
    (return(equal (eval w)
		  (cond ((pmmm!>) ![nullm1!])
			(t        ![nullm!]))))))

%----- User interrupt ----------------------------------------------------

(de errortr!> (w) (list 'error!> w))

(de error!> (w)
  (progn
    (cond ((pairp w) (setq w (car w))))
    (prin2 w)(terpri)
    (err!> 1000)))

%----- Translation for Algebraic Expressions Only ------------------------

%      Without Evaluation for Let, Clear, Factor, RamFac, Ordaer ...

% Translation with !!ER!! return for Algebraic Expressions only ...
% dim sgnt sign - are replaced by exact numbers
(de translata!> (lst)
  (prog nil
    (cond((null lst)(return nil)))
    (setq lst (errorset!> (list2 'unitraa!> (list2 'quote lst))
                          ![erst1!] ![erst2!]))
    (cond((atom lst)(prog2(setq ![er!] lst)(return !!er!!))))
    (return(car lst)) ))

% The same but result must ne a pure number after EVAL!> ...
(de ntranslata!> (lst)
  (prog nil
    (cond ((null lst) (return 0)))
    (setq lst (errorset!> (list2 'unitraa!> (list2 'quote lst))
                          ![erst1!] ![erst2!]))
    (cond ((atom lst) (setq ![er!] lst) (return !!er!!)))
    (setq lst (errorset!> (list2 'eval!> (list2 'quote (car lst)))
                          ![erst1!] ![erst2!]))
    (cond ((atom lst) (setq ![er!] lst) (return !!er!!)))
    (setq lst (car lst))
    (cond ((null lst) (return 0))
	  ((numberp lst) (return lst))
	  (t (setq ![er!] 99) (return !!er!!))) ))

% Alg translation ...
(de unitraa!> (lst)
  (cond ((atom lst) (atomtra!> lst))  % atom
        ((and(pairp lst)(null(cdr lst)))(unitraa!>(car lst))) % next level
        (t(proc (w)
            (cond((not(memq(car lst) '(!+ !-)))            % + - translation
                    (setq lst(cons '!+ lst))))
            (setq w(mems!> '(!+ !-) (reverse lst) nil))
            (cond((eq w !!er!!) (err!> 2017)))
            (setq w(mapcar w 'auxfun3!>))
            (return(cond((null(cdr w))(car w))
                        (t(cons 'plus w))))))))

(de auxfun3!> (w)
  (cond((eq(cdr w) '!+)(termtra!>(car w)))
       (t(list2 'minus (termtra!>(car w))))))

% Atom translation ...
(de atomtra!> (w)
  (cond ((zerop w) nil)                              % zero
        ((stringp w)(prog2(doubs!> w) (err!> 2019)))
	((eq w '!d!i!m) ![dim!])                        % dimension
	((or (eq w '!s!g!n!t) (eq w '!s!i!g!n)) ![sigprod!]) % signature
        ((or(numberp w)(flagp w '!+grgvar)) w) % number or variable
        (t(prog2(doub!> w) (err!> 2018)))))

%  *  translation
(de termtra!> (lst)
  (prog (w)
    (cond((null lst) (err!> 2016)))
    (setq w(seek1!> lst '!* ))
    (cond((null w) (return(quotra!> lst))))
    (return (list 'times
                  (quotra!>(reverse(car w)))
                  (termtra!>(cdr w))))))

% / translation
(de quotra!> (lst)
  (cond((null lst) (err!> 2016))
       ((not(memq '!/ lst))(exptra!> lst))
       (t(prog (w)
           (setq w(memlist!> '!/ lst))
           (cond((eq w !!er!!) (err!> 2016)))
           (return(quotmka!> nil w))))))

(de quotmka!> (lst1 lst2)
  (cond((null lst2) lst1)
       (t(quotmka!>
           (list 'quotient
              (cond(lst1 lst1)(t(exptra!>(car lst2))))
              (exptra!>(cond(lst1(car lst2))(t(cadr lst2)))))
           (cond(lst1(cdr lst2))(t (cddr lst2)))))))

% ** or ^ translation
(de exptra!> (lst)
  (prog (w)
    (cond((null lst) (err!> 2016)))
    (setq w(seek!> lst '(!*!* !^) ))
    (cond((null w)(return(kertra!> lst))))
    (return (list 'expt
                  (kertra!>(reverse(car w)))
                  (exptra!>(cddr w))))))

% Kernel translation
(de kertra!> (lst)
  (cond((null lst) (err!> 2015))
       ((pairp(car lst))(cond((cdr lst) (err!> 2014))
                             (t(unitraa!>(car lst)))))
       ((not(cdr lst)) (atomtra!>(car lst)))
       (t(funtra!> lst))))

% Function translation
(de funtra!> (lst)
  (cond((or(null lst)(atom lst)(not(eqn(length lst)2))
           (not(idp(car lst)))(atom(cadr lst)))
           (err!> 2021))
       ((and(not(flagp(car lst) '!+fun))
            (not(redgood!>(car lst))))
         (prog2(doub!>(car lst)) (err!> 2022)))
       (t(prog (w)
           (setq w (car lst))
           (setq lst(cadr lst))
           (setq lst(memlist!> '!, lst))
           (cond((eq lst !!er!!) (err!> 2020)))
           (setq lst (mapcar lst (function unitraa1!>)))
           (return(cons w lst))))))

(de unitraa1!> (lst)
  (cond((setq lst (unitraa!> lst)) lst)
       (t 0)))


%--------- Boolean Expressions Translation -------------------------------

%  For  For All Such That ; command ...

% Translation with !!ER!! return for Bollean Expressions ...
(de booltra!> (lst)
  (prog nil
    (cond((null lst)(return nil)))
    (setq lst (errorset!> (list2 'booltra0!> (list2 'quote lst))
                          ![erst1!] ![erst2!]))
    (cond((atom lst)(prog2(setq ![er!] lst)(return !!er!!))))
    (return(car lst)) ))

(de booltra0!> (lst)
  (cond ((atom lst) (atomtrab!> lst))    % atom
        ((and(pairp lst)(null(cdr lst))) % next level
          (booltra0!>(car lst)))
        (t(prog (w)                      % or - translation ...
            (setq w (memlist!> '!o!r lst))
            (cond((eq w !!er!!) (err!> 2400))
		 ((null(cdr w))(return(andtra!> lst))))
            (return(cons 'or (mapcar w 'andtra!>)))))))

(de andtra!> (lst)
  (cond ((null lst) (err!> 2400))
	((null(cdr lst))
          (booltra0!>(car lst)))         % next level
        (t(prog (w)                      % and - translation ...
            (setq w (memlist!> '!a!n!d lst))
            (cond((eq w !!er!!) (err!> 2400))
		 ((null(cdr w))(return(nottra!> lst))))
            (return(cons 'and (mapcar w 'nottra!>)))))))

(de nottra!> (lst)
  (cond ((null lst) (err!> 2400))
	((null(cdr lst))
          (booltra0!>(car lst)))         % next level
	((eq (car lst) '!n!o!t)
	  (list2 'not (reltra!>(cdr lst))))
	(t(reltra!> lst))))

(de reltra!> (lst)
  (cond ((null lst) (err!> 2400))
	((null(cdr lst))
          (booltra0!>(car lst)))         % next level
	(t(prog (w wa wb)
	    (setq w (seek!> lst '( != !< !> !<!= !>!= !|!= )))
	    (cond((null w) (return(algtra1!> lst)))
		 ((or (null(car w)) (null(cddr w))) (err!> 2400)))
	    (setq wa (algtra!>(reverse(car w))))
	    (setq wb (algtra!>(cddr w)))
	    (setq w (cadr w))
	    (cond
	      ((eq w '!=)   (setq w 'evalequal))
	      ((eq w '!<)   (setq w 'evallessp))
	      ((eq w '!>)   (setq w 'evalgreaterp))
	      ((eq w '!|!=) (setq w 'evalneq))
	      ((eq w '!<!=) (setq w 'evalleq))
	      ((eq w '!>!=) (setq w 'evalgeq)))
	    (return(list w wa wb))))))

(de algtra!> (lst)
  (list 'aeval (list 'quote (unitraa!> lst))))

(de algtra1!> (lst)
  (list 'boolvalue!* (list 'eval!> (list 'quote (unitraa!> lst)))))

(de atomtrab!> (lst)
  (list 'boolvalue!* (list 'eval!> (list 'quote (atomtra!> lst)))))

%=========  End of GRGtrans.sl  ===========================================%

