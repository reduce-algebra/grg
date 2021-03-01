%==========================================================================%
%   GRGprin.sl                                            Output Routines  %
%==========================================================================%
%   GRG 3.2 Standard Lisp Source Code       (C) 1988-96 Vadim V. Zhytnikov %
%==========================================================================%
% This file is distributed without any warranty. You may modify it but you %
% are not allowed to remove author's name and/or distribute modified file. %
%==========================================================================%


%-----  REDUCE algebraic expression printing -----------------------------

% Algebraic Expressions Printing ...
(de algpri!> (w)
  (cond ((getd 'assgnpri) (assgnpri w nil nil))
        (t                (varpri   w nil nil))))

% TERPRI for algebraic expressions ...
(de algterpri!> nil
  (cond ((getd 'assgnpri) (assgnpri "" nil t))
        (t                (varpri   "" nil t))))

% Plain print list without spaces and () ...
(de algrpril!> (lst) (mapc lst 'algrpri!>))

% Print list without () with Special treatment
% of strings and spaces ...
(de algprinwb!> (lst)
  (foreach!> x on lst do
   (prog2
     (cond
         ((stringp(car x)) (progn (algpri!> '!" )
                                  (algpri!> (car x) )
                                  (algpri!> '!" )))
         ((atom(car x)) (algpri!> (car x) ))
         (t(progn
            (algpri!> '!( )
            (algprinwb!>(car x))
            (algpri!> '!) ) )))
     (cond((and x (cdr x) (atom(cadr x))
                (not(or (flagp (cadr x) '!+nonsp)
                        (flagp (car x) '!+nonsp))))
            (algpri!> " " ))))))

%-----  Print Functions with Linelength check  ---------------------------

(de gterpri!> nil
  (progn
    (cond(![line!] (gterpri0!> ![line!])))
    (terpri)
    (setq ![gpfirst!] nil)
    (setq ![line!] nil)
    (setq ![lline!] 0) ))

(de gterpri0!> (lst)
  (cond
    ((null(cdr lst)) (prin2(car lst)))
    (t (prog2 (gterpri0!>(cdr lst)) (prin2(car lst))))))

(de gprinreset!> nil
  (progn (setq ![lline!] 0)
         (setq ![line!] nil)
         (setq ![gpfirst!] t)
         (setq ![gptab!] 0) ))

(de gprin!> (w)
  (cond
    ((pairp w) (progn (gprin!> "(") (mapcar w 'gprin!>) (gprin!> ")")))
    (t(prog (wc wl)
      (setq wl (difference (linelength nil) spare!*))
      (setq wc (length(explode2 w)))
      (cond
        ((lessp (plus2 ![lline!] wc) wl) (progn
	  (cond
            ((and(null ![line!])(not ![gpfirst!])) (progn
	      (spaces ![gptab!])
	      (setq ![lline!] ![gptab!]))))
	  (cond % We skip '!  in the beginning of line (but not " ") ...
            ((not(and (null ![line!]) (seprp w))) (prog2
	      (setq ![line!] (cons w ![line!]))
	      (setq ![lline!] (plus2 ![lline!] wc)) )))))
	(t(progn
	  (gterpri!>)
	  (cond((not(seprp w))(progn
	    (spaces ![gptab!])
	    (setq ![lline!] (plus2 ![gptab!] wc))
	    (setq ![line!] (ncons w))))))))))))

% Print list without () by GPRIN> with Special treatment
% of strings and spaces ...
(de gprinwb!> (lst) (gprinwb0!> lst 0))
(de gprinwb0!> (lst wl)
  (foreach!> x on lst do
   (prog2
     (cond
         ((stringp(car x)) (progn
            (gprin!> '!")
            (gprin!>(car x))
            (gprin!> '!")    ))
         ((atom(car x)) (gprin!>(car x)))
         (t(progn
            (gprin!> '!( )
            (gprinwb0!> (car x) (add1 wl))
            (gprin!> '!) ) )))
     (cond ((and x (cdr x) (atom(cadr x))
                 (not(or (flagp (cadr x) '!+nonsp)
                         (flagp (car x) '!+nonsp))))
            (gprin!> '! )))
     (cond ((and (eq (car x) '!,) (zerop wl)) (gprin!> '! )))
     )))

% Prints simply spaced list of atoms without ()
(de gprils!> (lst)
  (while!> lst
    (gprin!>(car lst)) (gprin!> '! )
    (setq lst (cdr lst))))

% Prints simply spaced list of atoms without ()
% and without last trailing space
(de gprils0!> (lst)
  (while!> lst
    (gprin!> (car lst))
    (cond ((cdr lst) (gprin!> '! )))
    (setq lst (cdr lst))))

(de gprils0dot!> (lst)
  (while!> lst
    (gprin!> (cond ((cdr lst) (car lst))
		   (t (incom!> (append (explode2(car lst)) '(!! !.))))))
    (cond ((cdr lst) (gprin!> '! )))
    (setq lst (cdr lst))))

(de gpris!> nil (gprin!> '! ))

% Prints concatenated list of atoms
(de gpril!> (lst)
  (while!> lst
    (gprin!>(car lst))
    (setq lst (cdr lst))))

% Function Print
(de gfnpri!> (lst)
  (progn (gprin!> (car lst))
	 (cond ((get (car lst) 'generic!_function) (gprin!> "*")))
         (gprin!> "(")
	 (gfnpri0!> (cdr lst))
	 (gprin!> ")")  ))

(de gfnpri0!> (lst)
  (cond((null(cdr lst)) (gprin!>(car lst)))
       (t(progn (gprin!>(car lst))(gprin!> ",")(gfnpri0!>(cdr lst))))))



%----------  Output Switches Management  ---------------------------------

% Fancy/LaTeX (FT) switcses: FANCY LATEX
% Output (O) switches: GRG REDUCE MAPLE MATH MACSYMA

% FT mode is defined by *FANCY=T (FANCYON>)
% latex mode is defined by *latex=T
% This detects O output mode ...
(de ifmodo!> nil (or !*grg !*reduce !*maple !*math !*macsyma))

% This detects existence of fancy mode in REDUCE
(de fancyexist!> nil (flagp 'fancy 'switch))
(de fancyloaded!> nil (getd 'fmp!-switch))
(de fancyon!> nil
  (and (or(fluidp '!*fancy)(globalp '!*fancy)) (eval '!*fancy)))

(de tunefancy!> (bool)
  (cond(bool(progn
      (cond((or (fluidp '!*fancy!-lower) (globalp '!*fancy!-lower))
              (set '!*fancy!-lower nil))
           (t(msg!> 9100)))
      (cond ((not ![fldtuned!]) (fldtune!>)))
      (onoff2!> 'latex nil)
      (set 'fancy!-switch!-on!*   (int2id 16))
      (set 'fancy!-switch!-off!*  (int2id 17))
      (onfancydefs!>)
      (offothero!> nil)))
    (t(offallo!>))))

(de tunetex!> (bool)
  (prog nil
    (cond ((not(fancyexist!>)) (loadpack!> '(fmprint) nil)))
    (cond ((not(fancyexist!>))
      (progn (msg!> 9101)
             (msg!> 91011)
             (msg!> 91012)
             (msg!> 91013)
             (msg!> 91014)
             (setq !*latex nil)
             (return nil))))
    (cond(bool(progn
        (on fancy)
        (cond((or (fluidp '!*fancy!-lower) (globalp '!*fancy!-lower))
                (set '!*fancy!-lower nil))
             (t (progn (msg!> 9100)
                       (msg!> 91011)
                       (msg!> 91012)
                       (msg!> 91013)
                       (msg!> 91014) )))
        (cond ((not ![fldtuned!]) (fldtune!>)))
	(set 'fancy!-switch!-on!*   '!$)
	(set 'fancy!-switch!-off!*  '!$)
        (ontexdefs!>)
        (offothero!> nil)))
      (t(progn
        (offothero!> nil)
        (set 'fancy!-switch!-on!*   (int2id 16))
        (set 'fancy!-switch!-off!*  (int2id 17))
        (onfancydefs!>)  )))))

(de fldtune!> nil
  (progn
    (setq ![fldtuned!] t)
    (copyd 'oldfld!> 'fancy!-lower!-digits)
    (remd 'fancy!-lower!-digits)
    (copyd 'fancy!-lower!-digits 'fancylowerdigits!>)
    ))

(de fancylowerdigits!> (u)
  (prog (w wa wn wz wr)
    (setq w (reverse u))
    % Last symbol is ~ ?
    (cond ((eq (car w) '!~) (setq wz t) (setq w (cdr w))))
    % Selecting digits ...
    lab1
    (cond ((or (null w) (not(digit(car w)))) (go lab2)))
      (setq wn (cons (car w) wn))
      (setq w (cdr w))
      (go lab1)
    lab2
    % Atom itself
    (setq w (reverse w))
    (setq wa (intern(compress w)))
    % Symbol is special
    (cond
      ((setq wa (get wa 'fancy!-special!-symbol))
	(cond
	  ((stringp wa) (setq w (explode2 wa)))
	  (t (setq w (append '(!\ !s !y !m !b !{)
		              (append (explode2 wa) '(!}))))))))
    (cond
      (!*latex % latex mode: usinge \dot{}
	(cond
	  (wz (setq w (append '( !\ !d !o !t !{ ) (append w '( !} ))))))
        (cond
         (wn (setq wr (append w (append '( !_ !{ ) (append wn '( !} ))))))
         (t  (setq wr w))))
      (t(cond % FANCY mode: using ' for conjugation
         ((and wz wn)
	  (setq wr (append w (append '( !' !_ !{ ) (append wn '( !} ))))))
         (wz (setq wr (append w '( !' ))))
         (wn (setq wr (append w (append '( !_ !{ ) (append wn '( !} ))))))
         (t  (setq wr w)))))
   (return wr)))

(de tunedfindexed!> (bool)
  (cond ((or (globalp 'fancy!_print!_df) (fluidp  'fancy!_print!_df))
    (cond (bool (set 'fancy!_print!_df 'indexed))
          (t    (set 'fancy!_print!_df 'partial))))))

(de tunegrg!> (bool)
  (cond(bool(progn
      (offft!>)
      (offothero!> 'grg)))
    (t(offallo!>))))

(de tunereduce!> (bool)
  (cond(bool(progn
      (offft!>)
      (offothero!> 'reduce)))
    (t(offallo!>))))

(de tunemaple!> (bool)
  (cond(bool(progn
      (offft!>)
      (offothero!> 'maple)))
    (t(offallo!>))))

(de tunemath!> (bool)
  (cond(bool(progn
      (offft!>)
      (offothero!> 'math)))
    (t(offallo!>))))

(de tunemacsyma!> (bool)
  (cond(bool(progn
      (offft!>)
      (offothero!> 'macsyma)))
    (t(offallo!>))))

% Offs All O-switches exept WSS ...
(de offothero!> (wss)
  (proc (w)
    (setq w ![flaglo!])
    (while!> w
      (cond((not(eq (car w) wss))
        (onoff2!> (car w) nil)))
      (setq w (cdr w)))))

% Offs FT-switces ...
(de offft!> nil
  (progn
    (cond(!*latex (onoff2!> 'latex nil)))
    (cond((fancyon!>)(off fancy)))))

% Offs all FT and O-switches ...
(de offallo!> nil
  (prog2 (offft!>) (offothero!> nil)))

(de ontexdefs!> nil
 (progn
    (put '!#!#lr 'fancy!-special!-symbol "{}")
    (put '!#!#e 'fancy!-special!-symbol "e")
    (put '!#!#b 'fancy!-special!-symbol "b")
    (put '!#!#p 'fancy!-special!-symbol "\partial")
    (flag '(!#!#e !#!#p) 'print!-indexed)
    (put 'e 'fancy!-special!-symbol "e")
    (put 'i 'fancy!-special!-symbol "i")
    (put '!a!l!p!h!a    'fancy!-special!-symbol "\alpha")
    (remprop '!A!L!P!H!A   'fancy!-special!-symbol)
    (put '!b!e!t!a     'fancy!-special!-symbol "\beta")
    (remprop '!B!E!T!A   'fancy!-special!-symbol)
    (put '!g!a!m!m!a     'fancy!-special!-symbol "\gamma")
    (put '!G!A!M!M!A     'fancy!-special!-symbol "\Gamma")
    (put '!G!a!m!m!a     'fancy!-special!-symbol "\Gamma")
    (put '!d!e!l!t!a     'fancy!-special!-symbol "\delta")
    (put '!D!E!L!T!A     'fancy!-special!-symbol "\Delta")
    (put '!D!e!l!t!a     'fancy!-special!-symbol "\Delta")
    (put '!e!p!s!i!l!o!n  'fancy!-special!-symbol "\epsilon")
    (remprop '!E!P!S!I!L!O!N   'fancy!-special!-symbol)
    (put '!z!e!t!a     'fancy!-special!-symbol "\zeta")
    (remprop '!Z!E!T!A   'fancy!-special!-symbol)
    (put '!e!t!a      'fancy!-special!-symbol "\eta")
    (remprop '!E!T!A   'fancy!-special!-symbol)
    (put '!t!h!e!t!a    'fancy!-special!-symbol "\theta")
    (put '!T!H!E!T!A     'fancy!-special!-symbol "\Theta")
    (put '!T!h!e!t!a     'fancy!-special!-symbol "\Theta")
    (put '!i!o!t!a     'fancy!-special!-symbol "\iota")
    (remprop '!I!O!T!A   'fancy!-special!-symbol)
    (put '!k!a!p!p!a    'fancy!-special!-symbol "\kappa")
    (remprop '!K!A!P!P!A   'fancy!-special!-symbol)
    (put '!l!a!m!b!d!a   'fancy!-special!-symbol "\lambda")
    (put '!L!A!M!B!D!A    'fancy!-special!-symbol "\Lambda")
    (put '!L!a!m!b!d!a    'fancy!-special!-symbol "\Lambda")
    (put '!m!u       'fancy!-special!-symbol "\mu")
    (remprop '!M!U   'fancy!-special!-symbol)
    (put '!n!u       'fancy!-special!-symbol "\nu")
    (remprop '!N!U   'fancy!-special!-symbol)
    (put '!x!i       'fancy!-special!-symbol "\xi")
    (put '!X!I        'fancy!-special!-symbol "\Xi")
    (put '!X!i        'fancy!-special!-symbol "\Xi")
    (put '!p!i       'fancy!-special!-symbol "\pi")
    (put '!P!I        'fancy!-special!-symbol "\pi")
    (put '!P!i        'fancy!-special!-symbol "\Pi")
    (put '!r!h!o      'fancy!-special!-symbol "\rho")
    (remprop '!R!H!O   'fancy!-special!-symbol)
    (put '!s!i!g!m!a    'fancy!-special!-symbol "\sigma")
    (put '!S!I!G!M!A     'fancy!-special!-symbol "\Sigma")
    (put '!S!i!g!m!a     'fancy!-special!-symbol "\Sigma")
    (put '!t!a!u      'fancy!-special!-symbol "\tau")
    (remprop '!T!A!U   'fancy!-special!-symbol)
    (put '!u!p!s!i!l!o!n  'fancy!-special!-symbol "\upsilon")
    (put '!U!P!S!I!L!O!N   'fancy!-special!-symbol "\Upsilon")
    (put '!U!p!s!i!l!o!n   'fancy!-special!-symbol "\Upsilon")
    (put '!p!h!i      'fancy!-special!-symbol "\phi")
    (put '!P!H!I       'fancy!-special!-symbol "\Phi")
    (put '!P!h!i       'fancy!-special!-symbol "\Phi")
    (put '!c!h!i      'fancy!-special!-symbol "\chi")
    (remprop '!C!H!I   'fancy!-special!-symbol)
    (put '!p!s!i      'fancy!-special!-symbol "\psi")
    (put '!P!S!I       'fancy!-special!-symbol "\Psi")
    (put '!P!s!i       'fancy!-special!-symbol "\Psi")
    (put '!o!m!e!g!a    'fancy!-special!-symbol "\omega")
    (put '!O!M!E!G!A     'fancy!-special!-symbol "\Omega")
    (put '!O!m!e!g!a     'fancy!-special!-symbol "\Omega")
    (put 'infinity 'fancy!-special!-symbol  "\infty")
    (put 'partial!-df 'fancy!-special!-symbol "\partial")
    (remflag '(!D!E!L!T!A !d!e!l!t!a) 'PRINT!-INDEXED)
    (put 'sin  'fancy!-functionsymbol   "\sin")
    (put 'sinh  'fancy!-functionsymbol  "\sinh")
    (put 'asin  'fancy!-functionsymbol  "\arcsin")
    (put 'asinh  'fancy!-functionsymbol "arcsinh")
    (put 'cos  'fancy!-functionsymbol   "\cos")
    (put 'cosh  'fancy!-functionsymbol  "\cosh")
    (put 'acos  'fancy!-functionsymbol  "\arccos")
    (put 'acosh  'fancy!-functionsymbol "arccosh")
    (put 'tan  'fancy!-functionsymbol   "\tan")
    (put 'tanh  'fancy!-functionsymbol  "\tanh")
    (put 'atan  'fancy!-functionsymbol  "\arctan")
    (put 'atanh  'fancy!-functionsymbol "arctanh")
    (put 'cot  'fancy!-functionsymbol  "\cot")
    (put 'coth  'fancy!-functionsymbol "\coth")
    (put 'acot  'fancy!-functionsymbol  "arccot")
    (put 'acoth  'fancy!-functionsymbol "arccoth")
    (put 'sec  'fancy!-functionsymbol  "\sec")
    (put 'sech  'fancy!-functionsymbol  "sech")
    (put 'asec  'fancy!-functionsymbol  "arcsec")
    (put 'asech  'fancy!-functionsymbol "arcsech")
    (put 'csc  'fancy!-functionsymbol  "\csc")
    (put 'csch  'fancy!-functionsymbol  "csch")
    (put 'acsc  'fancy!-functionsymbol  "arccsc")
    (put 'acsch  'fancy!-functionsymbol "arccsch")
    (put 'ln   'fancy!-functionsymbol "\ln")
    (put 'log  'fancy!-functionsymbol "\log")
))

(DE ONFANCYDEFS!> NIL
 (PROGN
    (put '!#!#lr 'fancy!-special!-symbol "{}")
    (put '!#!#e 'fancy!-special!-symbol "e")
    (put '!#!#b 'fancy!-special!-symbol "b")
    (put '!#!#p 'fancy!-special!-symbol 182)
    (flag '(!#!#e !#!#p) 'print!-indexed)
    (put 'e 'fancy!-special!-symbol "e")
    (put 'i 'fancy!-special!-symbol "i")
    (put '!a!l!p!h!a    'fancy!-special!-symbol "\alpha")
    (remprop '!A!L!P!H!A   'fancy!-special!-symbol)
    (put '!b!e!t!a     'fancy!-special!-symbol "\beta")
    (remprop '!B!E!T!A   'fancy!-special!-symbol)
    (put '!g!a!m!m!a    'fancy!-special!-symbol "\gamma")
    (put '!G!A!M!M!A     'fancy!-special!-symbol 71)
    (put '!G!a!m!m!a     'fancy!-special!-symbol 71)
    (put '!d!e!l!t!a    'fancy!-special!-symbol "\delta")
    (put '!D!E!L!T!A     'fancy!-special!-symbol 68)
    (put '!D!e!l!t!a     'fancy!-special!-symbol 68)
    (put '!e!p!s!i!l!o!n  'fancy!-special!-symbol "\epsilon")
    (remprop '!E!P!S!I!L!O!N   'fancy!-special!-symbol)
    (put '!z!e!t!a     'fancy!-special!-symbol "\zeta")
    (remprop '!Z!E!T!A   'fancy!-special!-symbol)
    (put '!e!t!a      'fancy!-special!-symbol "\eta")
    (remprop '!E!T!A   'fancy!-special!-symbol)
    (put '!t!h!e!t!a    'fancy!-special!-symbol "\theta")
    (put '!T!H!E!T!A     'fancy!-special!-symbol 81)
    (put '!T!h!e!t!a     'fancy!-special!-symbol 81)
    (put '!i!o!t!a     'fancy!-special!-symbol "\iota")
    (remprop '!I!O!T!A   'fancy!-special!-symbol)
    (put '!k!a!p!p!a    'fancy!-special!-symbol "\kappa")
    (remprop '!K!A!P!P!A   'fancy!-special!-symbol)
    (put '!l!a!m!b!d!a   'fancy!-special!-symbol "\lambda")
    (put '!L!A!M!B!D!A    'fancy!-special!-symbol 76)
    (put '!L!a!m!b!d!a    'fancy!-special!-symbol 76)
    (put '!m!u       'fancy!-special!-symbol "\mu")
    (remprop '!M!U   'fancy!-special!-symbol)
    (put '!n!u       'fancy!-special!-symbol "\nu")
    (remprop '!N!U   'fancy!-special!-symbol)
    (put '!x!i       'fancy!-special!-symbol "\xi")
    (put '!X!I        'fancy!-special!-symbol 88)
    (put '!X!i        'fancy!-special!-symbol 88)
    (put '!p!i       'fancy!-special!-symbol "\pi")
    (put '!P!I        'fancy!-special!-symbol "\pi")
    (put '!P!i        'fancy!-special!-symbol 80)
    (put '!r!h!o      'fancy!-special!-symbol "\rho")
    (remprop '!R!H!O   'fancy!-special!-symbol)
    (put '!s!i!g!m!a    'fancy!-special!-symbol "\sigma")
    (put '!S!I!G!M!A     'fancy!-special!-symbol 83)
    (put '!S!i!g!m!a     'fancy!-special!-symbol 83)
    (put '!t!a!u      'fancy!-special!-symbol "\tau")
    (remprop '!T!A!U   'fancy!-special!-symbol)
    (put '!u!p!s!i!l!o!n  'fancy!-special!-symbol "\upsilon")
    (put '!U!P!S!I!L!O!N   'fancy!-special!-symbol 161)
    (put '!U!p!s!i!l!o!n   'fancy!-special!-symbol 161)
    (put '!p!h!i      'fancy!-special!-symbol "\phi")
    (put '!P!H!I       'fancy!-special!-symbol 70)
    (put '!P!h!i       'fancy!-special!-symbol 70)
    (put '!c!h!i      'fancy!-special!-symbol "\chi")
    (remprop '!C!H!I   'fancy!-special!-symbol)
    (put '!p!s!i      'fancy!-special!-symbol "\psi")
    (put '!P!S!I       'fancy!-special!-symbol 89)
    (put '!P!s!i       'fancy!-special!-symbol 89)
    (put '!o!m!e!g!a    'fancy!-special!-symbol "\omega")
    (put '!O!M!E!G!A     'fancy!-special!-symbol 87)
    (put '!O!m!e!g!a     'fancy!-special!-symbol 87)
    (put 'infinity 'fancy!-special!-symbol "\infty")
    (put 'partial!-df 'fancy!-special!-symbol 182)
    (remflag '(!D!E!L!T!A !d!e!l!t!a) 'PRINT!-INDEXED)
    (put 'sin  'fancy!-functionsymbol "sin")
    (put 'sinh  'fancy!-functionsymbol "sinh")
    (put 'asin  'fancy!-functionsymbol "asin")
    (put 'asinh  'fancy!-functionsymbol "asinh")
    (put 'cos  'fancy!-functionsymbol "cos")
    (put 'cosh  'fancy!-functionsymbol "cosh")
    (put 'acos  'fancy!-functionsymbol "acos")
    (put 'acosh  'fancy!-functionsymbol "acosh")
    (put 'tan  'fancy!-functionsymbol "tan")
    (put 'tanh  'fancy!-functionsymbol "tanh")
    (put 'atan  'fancy!-functionsymbol "atan")
    (put 'atanh  'fancy!-functionsymbol "atanh")
    (put 'cot  'fancy!-functionsymbol "cot")
    (put 'coth  'fancy!-functionsymbol "coth")
    (put 'acot  'fancy!-functionsymbol "acot")
    (put 'acoth  'fancy!-functionsymbol "acoth")
    (put 'sec  'fancy!-functionsymbol "sec")
    (put 'sech  'fancy!-functionsymbol "sech")
    (put 'asec  'fancy!-functionsymbol "asec")
    (put 'asech  'fancy!-functionsymbol "asech")
    (put 'csc  'fancy!-functionsymbol "csc")
    (put 'csch  'fancy!-functionsymbol "csch")
    (put 'acsc  'fancy!-functionsymbol "acsc")
    (put 'acsch  'fancy!-functionsymbol "acsch")
    (put 'ln   'fancy!-functionsymbol "ln")
    (put 'log  'fancy!-functionsymbol "log")
))


%------- Print functions for GRG REDUCE MAPLE ... ------------------------

(de ooprin!> (lst)
  (cond ((atom lst)                 (ooatom!> lst))
	((eq (car lst) 'plus)       (oonop!> lst "+"))
	((eq (car lst) 'minus)      (oominus!> lst))
	((eq (car lst) 'difference) (oo2op!> lst "-"))
	((eq (car lst) 'times)      (oonop!> lst "*"))
	((eq (car lst) 'quotient)   (oo2op!> lst "/"))
	((eq (car lst) 'expt)       (oo2op!> lst '!^ ))
	(t                          (oofun!> lst))
))

(de oominus!> (lst)
  (progn (gprin!> "(")
         (gprin!> "-")
         (ooprin!> (cadr lst))
         (gprin!> ")") ))

(de oo2op!> (lst w)
 (progn (gprin!> "(")
        (ooprin!> (cadr lst))
        (gprin!> w)
        (ooprin!> (caddr lst))
        (gprin!> ")") ))

(de oonop!> (lst w)
  (proc nil
    (gprin!> "(")
    (setq lst (cdr lst))
    (ooprin!> (car lst))
    (setq lst (cdr lst))
    (while!> lst
      (gprin!> w)
      (ooprin!> (car lst))
      (setq lst (cdr lst)))
    (gprin!> ")")))

(de ooatom!> (w)
  (cond ((null w)          (gprin!> 0))
        ((eq w 'e)         (ooae!>))
        ((eq w 'i)         (ooai!>))
        ((eq w 'pi)        (ooapi!>))
        ((eq w 'infinity)  (ooainf!>))
	((and (not !*grg) (get w '!=depend))
                           (oofun0!>(get w '!=depend)))
        (t                 (gprin!> w))))

(de ooae!> nil
  (gprin!> (cond
    (!*macsyma            '!%!e )
    ((or !*math !*maple)  '!E   )
    (t                    'e    ))))

(de ooai!> nil
  (gprin!> (cond
    (!*macsyma            '!%!i )
    ((or !*math !*maple)  '!I   )
    (t                    'i    ))))

(de ooapi!> nil
  (gprin!> (cond
    (!*macsyma            '!%!p!i )
    ((or !*maple !*math)  '!P!i   )
    (t                    'pi     ))))

(de ooainf!> nil
  (gprin!> (cond
    (!*maple '!i!n!f!i!n!i!t!y )
    (!*math  '!I!n!f!i!n!i!t!y )
    (t       'infinity         ))))

(de oolb!> nil (gprin!> (cond (!*math "[") (t "("))))
(de oorb!> nil (gprin!> (cond (!*math "]") (t ")"))))

(de oofun!> (w)
  (cond
    ((or !*grg !*reduce)      (oofun0!> w))
    ((eq (car w) 'df)         (oodf!>   w))
    ((eq (car w) 'int)        (ooint!>  w))
    ((eq (car w) 'prod)       (oops!>   w t))
    ((eq (car w) 'sum)        (oops!>   w nil))
    ((eq (car w) 'ln)         (ooln!>   w))
    ((eq (car w) 'log)        (oolog!>  w))
    ((eq (car w) 'sqrt)       (oosqrt!> w))
    ((flagp (car w) '!+trig)  (ootrig!> w))
    (t                        (oofun0!> w))))

(de oofun0!> (lst)
  (prog2
    (gprin!> (car lst))
    (ooargs!> (cdr lst))))

(de ooargs!> (lst)
  (proc nil
    (oolb!>)
    (ooprin!> (car lst))
    (setq lst (cdr lst))
    (while!> lst
      (gprin!> ",")
      (ooprin!> (car lst))
      (setq lst (cdr lst)))
    (oorb!>)))

(de oodf!> (lst)
  (cond((or !*reduce !*grg) (oofun0!> lst))
    (t(prog2
      (gprin!> (cond ((or !*maple !*macsyma) '!d!i!f!f )
		     (!*math                 '!D       )
		     (t                      'df       )))
      (ooargsdf!>(cdr lst))))))

(de ooargsdf!> (lst)
  (proc (w wc)
    (oolb!>)
    (ooprin!> (car lst))
    (setq lst (cdr lst))
    (while!> lst
      (gprin!> ",")
      (setq wc (car lst))
      (cond
        ((numberp wc)
	  (for!> ww (2 1 wc) do
            (prog2 (ooprin!> w)
                   (cond((not(eqn ww wc))(gprin!> ","))))))
	(t(ooprin!> wc)))
      (setq w wc)
      (setq lst (cdr lst)))
    (oorb!>)))

(de ooint!> (lst)
  (prog2
    (gprin!> (cond ((or !*maple !*macsyma) '!i!n!t!e!g!r!a!t!e )
		   (!*math                 '!I!n!t!e!g!r!a!t!e )
		   (t                      'int                )))
    (ooargs!>(cdr lst))))

(de oosqrt!> (lst)
  (prog2
    (gprin!> (cond ((or !*maple !*macsyma) '!s!q!r!t )
		   (!*math                 '!S!q!r!t )
		   (T                      'sqrt     )))
    (ooargs!>(cdr lst))))

(de ooln!> (lst)
  (prog2
    (gprin!> (cond (!*maple   '!l!n   )
		   (!*macsyma '!l!o!g )
		   (!*math    '!L!o!g )
		   (t         'ln     )))
    (ooargs!>(cdr lst))))

(de oolog!> (lst)
  (prog2
    (gprin!> (cond (!*maple   '!l!o!g )
		   (!*macsyma '!l!o!g )
		   (!*math    '!L!o!g )
		   (t         'log    )))
    (ooargs!>(cdr lst))))

(de oops!> (lst bool)
  (prog nil
    (gprin!>
      (cond (bool (cond ((or !*maple !*macsyma) '!p!r!o!d )
			(!*math                 '!P!r!o!d )
			(t                      'prod     )))
            (t    (cond ((or !*maple !*macsyma) '!s!u!m  )
			(!*math                 '!S!u!m  )
			(t                      'sum     )))  ))
    (cond((not(or !*math !*maple))
      (prog2 (ooargs!>(cdr lst)) (return nil))))
    (oolb!>)
    (ooprin!> (cadr lst))
    (setq lst (cddr lst))
    (gprin!> ",")
    (cond(!*math (gprin!> "{")))
    (ooprin!> (car lst))
    (gprin!> (cond (!*math   ",")
		   (!*maple  "=")))
    (ooprin!> (cadr lst))
    (gprin!> (cond (!*math   ",")
		   (!*maple  "..")))
    (ooprin!> (caddr lst))
    (cond(!*math (gprin!> "}")))
    (oorb!>)))

(de ootrig!> (lst)
  (prog (w wa)
    (setq w (explode2(car lst)))
    (cond((eq (car w) 'a) (prog2
      (setq wa t)
      (setq w (cdr w)))))
    (cond(wa
      (setq wa (cond (!*maple  '( !a !r !c ))
		     (!*math   '( !A !r !c ))
		     (t        '( A ))))))
    (cond
      (!*maple (setq w (mapcar w 'tolc!>)))
      (!*math  (setq w (cons (touc!> (car w)) (mapcar (cdr w) 'tolc!>)))))
    (setq w (compress(append wa w)))
    (oofun0!>(cons w (cdr lst)))))

(de ooend!> nil
  (cond ((not !*math) (gprin!> ";"))))

(de ooends!> nil
  (cond((not !*math)
    (gprin!>
      (cond ((or !*reduce !*macsyma) "$")
	    (!*maple ":")
	    (t ";"))))))

(de ooelem!> (wi wl)
  (proc nil
    (gprin!> wi)
    (cond((null wl) (return nil)))
    (gprin!> (cond((or !*math !*macsyma) "[")(t "(")))
    (while!> wl
      (gprin!> (car wl))
      (cond((cdr wl)(gprin!> ",")))
      (setq wl (cdr wl)))
    (gprin!> (cond((or !*math !*macsyma) "]")(t ")")))
    ))


%---------- For Write ----------------------------------------------------

(de wriassign!> (we)
  (cond ((fancyon!>) (algpri!> (cond (we ":\,") (t "\,=\,")) ))
	((ifmodo!>)
           (gprin!>
	     (cond (!*macsyma             " : " )
		   ((or !*maple !*reduce) " := ")
		   (t                     " = " ))))
	(t (algpri!> (cond (we " : ") (t " = ")) ))))

(de wriequal!> nil
  (cond ((fancyon!>) (algpri!> "\,=\," ))
	((ifmodo!>)
           (gprin!>
	     (cond (!*math " == ")
		   (t      " = " ))))
	(t (algpri!> " = " ))))


%----------  Equations Printing ------------------------------------------

(de eqpri!> (wl wr wt)
  (progn
    (cond ((zerop wt) (alpri!> wl)) (t (dfpri!> wl wt)))
    (wriequal!>)
    (cond ((zerop wt) (alpri!> wr)) (t (dfpri!> wr wt)))
    ))


%----------  Algebraic Expressions Printing  -----------------------------

(de alpri!> (lst)
  (cond ((ifmodo!>) (ooprin!> lst))
	(t (algpri!> (cond (!*wrs (aeval lst)) (t lst)) ))))


%----------  Form Printing  ----------------------------------------------

(de dfpri!> (lst type)
  (cond ((ifmodo!>) (dfpri1!> lst type))
	(t (dfpri0!> lst type))))

(de dfpri0!> (lst type)
  (cond((null lst) (algpri!> 0 )) % 0
       (t(prog (wx)
           (setq type (lessp type 0))
           (cond(!*wrs(setq lst(aevalform!> lst))))
           (cond((null lst)(algpri!> 0 ))(t
             (foreach!> x in lst do % for all terms ...
               (progn
                 (cond((eqn(car x)-1)       (primi!>))  % - d x
                      ((not(eq x(car lst))) (pripl!>))) % ... + ...
                 (cond((not(or(eqn(car x)-1)(eqn(car x)1))) %  d x
                   (cond((or(idp(car x))
                            (and(numberp(car x))(not(lessp(car x)0)))
                            (and !*wrs
                              !*exp (not(getd 'taysimpexpt))
                              (not(numberp(car x))) % not -n
                              (eqn(cdr(cadar x)) 1) % den = 1
                              (null(cdar(cadar x)) ) % not a + b
                              (eqn(cdaar(cadar x)) 1) % not n * a
                              (eqn(cdaaar(cadar x)) 1) % not a ** b
                              )) % a d x
                           (algpri!> (car x) ))
                        (t
%                         (algpri!> (list2 '!  (car x)) )
                          (progn
                            (algpri!> "(" )
                            (algpri!> (car x) )
                            (algpri!> ")" ) )
                            )) )) % (...) d x
                 (setq wx (cddr x)) % wx - d x list
                 (prixvost!> wx type) ))))))))

(de primi!> nil
  (algpri!>
    (cond (!*latex "-")
	  (t     " -")) ))

(de pripl!> nil
  (algpri!>
    (cond (!*latex "+")
	  (t     " + ")) ))

(de prixvost!> (wx type)
  (proc (w wc)
    (setq wc 0)
    (while!> wx
      (cond((caar wx) (prog2
        (printdx0!> wc type)
        (cond((cdr wx) (priex!>))) )))
      (setq wc (add1 wc))
      (setq wx (cdr wx)))))

(de priex!> nil
  (algpri!>
    (cond (!*latex       "\,\wedge")
	  ((fancyon!>) "\,\symb{217}")
	  (t           " /\"))
    ))

(de printdx0!> (wc type)
  (cond
    (![modp!]                   %%% Anholonomic mode: b or e
      (cond
        ((fancyon!>) (prog2       % latex or fancy ...
	   (algpri!> "\," )
	   (cond (type (algpri!> (list '!#!#e wc) ))          % e_i
		 (t    (algpri!> (list 'expt '!#!#b wc) ))))) % b^i
        (t (prog2                 % plain grg ...
	     (algpri!> " " )
             (algpri!>
               (compress (cons (bore!> type) (explode2 wc)))     % bi or ei
               )))))
    (t(cond                     %%% Holonomic mode: @ x or d x ...
        ((fancyon!>)              % latex or fancy ...
	   (cond (type                                       % \partial_x
                    (algpri!> (list '!#!#p (getel1!> ![cord!] wc)) ))
		 (t (prog2                                   %  d x
		      (algpri!> "\,d\," )
		      (algpri!> (getel1!> ![cord!] wc) )))))
        (t (prog2                 % plain grg ...
	     (algpri!> (cond(type " @ ")(t " d ")) )
             (algpri!> (getel1!> ![cord!] wc) )))))))

(de bore!> (type) (cond (type '!e) (t '!b)))

(de dfpri1!> (lst type)
  (cond((null lst) (gprin!> 0)) % 0
       (t(proc (w wf wx wc)
           (setq type (lessp type 0))
           (while!> lst
	    (setq w (car lst))
	    (cond (wf (gprin!> "+"))
                  (t  (setq wf t)))
	    (cond((not(equal (car w) 1)) (prog2
	      (cond
                ((and (numberp(car w)) (lessp (car w) 0))
		  (ooprin!> (list2 'minus (minus(car w)))))
	        (t (ooprin!> (car w))))
	      (gprin!> "*"))))
	    (setq w (cddr w)) % d x list
	    (setq wc 0)
	    (setq wx nil)
	    (while!> w
	      (cond((caar w)
		(setq wx (cons (prepdx1!> wc type) wx))))
	      (setq wc (add1 wc))
	      (setq w (cdr w)))
	    (cond(!*grg (oogrgdx!> (reverse wx) type))
	         (t (oofun0!> (cons (cond (type '!pd) (t '!dx))
			            (reverse wx)))))
	    (setq lst (cdr lst)))))))

(de oogrgdx!> (wx type)
  (loop!>
    (cond((not ![modp!])(prog2
      (cond (type (gprin!> '!@))
	    (t    (gprin!> '!d)))
      (gprin!> '! ))))
    (gprin!> (car wx))
    (setq wx (cdr wx))
    (exitif (null wx))
    (gprin!> '!/!\)))

(de prepdx1!> (wc type)
  (cond
    (![modp!] (compress (cons (bore!> type)
			      (explode2 wc))))
    (t (getel1!> ![cord!] wc))))


%-------- Some General Print Functions -----------------------------------

(de grgterpri!> nil
  (cond((ifmodo!>) (gterpri!>))
       (t          (algterpri!>))))

(de grgend!> nil
  (cond((ifmodo!>) (ooend!>))))

(de grgends!> nil
  (cond((ifmodo!>) (ooends!>))))


%============ End of GRGprin.sl ===========================================%

