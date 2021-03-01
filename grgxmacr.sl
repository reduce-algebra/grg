%==========================================================================%
%   GRXmacro.sl                                           Macro Expansion  %
%==========================================================================%
%   GRG 3.2 Standard Lisp Source Code       (C) 1988-97 Vadim V. Zhytnikov %
%==========================================================================%
% This file is distributed without any warranty. You may modify it but you %
% are not allowed to remove author's name and/or distribute modified file. %
%==========================================================================%

(de expand!-file!> (ifile ofile)
  (prog (ic oc w is os)
     (setq ic (open ifile 'input))
     (setq oc (open ofile 'output))
     (terpri)
     (prin2 "### Expanding `")(prin2 ifile)(prin2 "' to `")(prin2 ofile)(prin2 "' ...")(terpri)
     (setq is (rds ic))
     (setq os (wrs oc))
     (prin2 "%==========================================================================%")(terpri)
     (prin2 "%   GRG 3.2 Standard Lisp Source Code       (C) 1988-97 Vadim V. Zhytnikov %")(terpri)
     (prin2 "%==========================================================================%")(terpri)
     (prin2 "% This file is distributed without any warranty. You may modify it but you %")(terpri)
     (prin2 "% are not allowed to remove author's name and/or distribute modified file. %")(terpri)
     (prin2 "%==========================================================================%")(terpri)
     (prin2 "% The file `")(prin2 ofile)(prin2 "' was generated from `")(prin2 ifile)(prin2 "' at ")
     (terpri)
     (prin2 "%  ")(prin2 (date))
     (terpri)
     (prin2 "% Must be used on a ")
     (cond ((getd 'rdf) (prin2 "CSL-based"))
           (t           (prin2 "PSL-based")))
     (terpri)
     (cond ((getd '!c!a!r) (prin2 "% Lower-Case system only!"))
           (t              (prin2 "% Upper-Case system only!")))
     (terpri)
     (terpri)
     (terpri)
    loop
     (setq w (errorset '(read) nil nil))
     (cond ((or (atom w) (eq (car w) !$eof!$)) (go end)))
     (print (xpand!> (car w)))
     (terpri)
     (go loop)
    end
     (prin2 "%======== End of `")(prin2 ofile)(prin2 "' =============================================%")
     (terpri)
     (wrs os)
     (rds is)
     (close ic)
     (close oc)
))

(de xpand!> (w)
  (cond ((atom w) w)
        ((and (eq (car w) 'explode2) % CSL explode2 is buggy!
              (getd 'rdf))
           (list 'explode2!> (xpand!> (cadr w))))
        ((eq (car w) 'proc)      (xproc w))
        ((eq (car w) 'loop!>)    (xloop!> w))
        ((eq (car w) 'while!>)   (xwhile!> w))
        ((eq (car w) 'repeat!>)  (xrepeat!> w))
        ((eq (car w) 'for!>)     (xfor!> w))
        ((eq (car w) 'fordim!>)  (xfordim!> w))
        ((eq (car w) 'foreach!>) (xforeach!> w))
        (t (cons (xpand!>(car w)) (xpand!>(cdr w))))))

(de mkcng!> (bool lab)
  (list2
    (quote cond)
    (list2
      (list2 (quote not) bool)
      (list2 (quote go) lab))))

(de mkcg!> (bool lab)
  (list2
    (quote cond)
    (list2
      bool
      (list2 (quote go) lab))))

(de xproc (u)
  (prog (body w wa wb wc)
    (setq body (list2 (cadr u) (quote prog)))
    (setq u (cddr u))
    label1
    (cond ((and (null u) (null wa)) (go label2)))
    (cond ((null u) (go label3)))
    (cond
      ((atom(car u)) (prog2 (setq body (cons (car u) body))
                     (setq u (cdr u))))
      ((or (setq wb (eq (caar u) (quote while!>)))
           (eq (caar u) (quote loop!>))
           (eq (caar u) (quote repeat!>)))
        (progn
          (setq wa (cons (cdr u) wa))
          (setq u (cdar u))
          (setq w (cons (gensym) w))
          (setq w (cons (gensym) w))
          (cond
            (wb (setq body (cons (mkcng!> (car u) (car w))
                                 (cons (cadr w) body))))
            (t (setq body (cons (cadr w) body))))
          (cond (wb (setq u (cdr u))))
          (setq wc (cons nil wc))))
      ((eq (caar u) (quote exitif))
        (prog2 (setq body (cons (mkcg!> (cadar u)(car w)) body))
              (setq u (cdr u)) ))
      ((eq (caar u) (quote tohead))
        (prog2 (setq body (cons (mkcg!> (cadar u)(cadr w)) body))
               (setq u (cdr u)) ))
      ((eq (caar u) (quote until))
        (progn
          (setq body (cons (car w) (cons (mkcng!> (cadar u)(cadr w)) body)))
          (setq u (cdr u))
          (setq wc (cons t wc))))
      (t (prog2 (setq body (cons (car u) body)) (setq u (cdr u)) )))
    label3
    (cond((and wa (null u))
           (progn
              (cond ((null (car wc))
                (setq body (cons (car w)
                                 (cons (list2 (quote go) (cadr w)) body)))))
              (setq w (cddr w))
              (setq u (car wa))
              (setq wa (cdr wa))
              (setq wc (cdr wc)))))
    (go label1)
    label2
    (return (xpand!>(reverse body)))))


(de xloop!>   (u)  (xproc (list (quote proc) nil (cons (quote loop!>) (cdr u)))))

(de xwhile!>  (u)  (xproc (list (quote proc) nil (cons (quote while!>) (cdr u)))))

(de xrepeat!> (u)  (xproc (list (quote proc) nil (cons (quote repeat!>) (cdr u)))))


(de xfor!> (u)
       (prog (action body exp incr lab1 lab2 result tail var x)
          (setq var (cadr u))
          (setq incr (caddr u))
          (setq action (cadddr u))
          (setq body (xpand!>(car (cddddr u))))
          (setq result (list (list 'setq var (car incr))))
          (setq incr (cdr incr))
          (setq x (list 'difference (cadr incr) var))
          (cond
             ((not (equal (car incr) 1))
                (setq x (list 'times (car incr) x))))
          (setq lab1 (gensym))
          (setq lab2 (gensym))
          (setq x (list 'minusp x))
          (setq result
             (nconc
                result
                (cons
                   lab1
                   (cons
                      (list 'cond (list x (list 'go lab2)))
                      (cons
                         body
                         (cons
                            (list
                               'setq
                               var
                               (list 'plus2 var (car incr)) )
                            (cons (list 'go lab1) (cons lab2 tail)))) ))) )
          (return (mkprog (cons var exp) result))))


(de xfordim!> (u)
       (prog (action body exp incr lab1 lab2 result tail var x)
          (setq var (cadr u))
          (setq incr (list 0 1 '![dim1!]))
          (setq action (caddr u))
          (setq body (xpand!>(car (cdddr u))))
          (setq result (list (list 'setq var (car incr))))
          (setq incr (cdr incr))
          (setq x (list 'difference (cadr incr) var))
          (cond
             ((not (equal (car incr) 1))
                (setq x (list 'times (car incr) x))))
          (setq lab1 (gensym))
          (setq lab2 (gensym))
          (setq x (list 'minusp x))
          (setq result
             (nconc
                result
                (cons
                   lab1
                   (cons
                      (list 'cond (list x (list 'go lab2)))
                      (cons
                         body
                         (cons
                            (list
                               'setq
                               var
                               (list 'plus2 var (car incr)) )
                            (cons (list 'go lab1) (cons lab2 tail)))) ))) )
          (return (mkprog (cons var exp) result))))


(de xforeach!> (u)
       (prog (action body fn lst mod var)
          (setq var (cadr u))
          (setq u (cddr u))
          (setq mod (car u))
          (setq u (cdr u))
          (setq lst (car u))
          (setq u (cdr u))
          (setq action (car u))
          (setq u (cdr u))
          (setq body (xpand!>(car u)))
          (setq fn
             (cond
                ((eq action 'do) (cond ((eq mod 'in) 'mapc) (t 'map)))
                ((eq action 'conc)
                   (cond ((eq mod 'in) 'mapcan) (t 'mapcon)))
                ((eq action 'collect)
                   (cond ((eq mod 'in) 'mapcar) (t 'maplist)))
                (t (rederr (list action "invalid in foreach statement")))) )
          (return
             (list
                fn
                lst
                (list 'function (list 'lambda (list var) body)))) ))

%========== End of GRXmacro.sl ============================================%

