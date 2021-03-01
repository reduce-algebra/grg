%==========================================================================%
%   GRGcoper.sl                             Operators and Transformations  %
%==========================================================================%
%   GRG 3.2 Standard Lisp Source Code       (C) 1988-96 Vadim V. Zhytnikov %
%==========================================================================%
% This file is distributed without any warranty. You may modify it but you %
% are not allowed to remove author's name and/or distribute modified file. %
%==========================================================================%


%--- Spinorial rotation  13.03.91, 05.96 ---------------------------------

% Main function ...
(de rotas!> (lst) % 05.96
  (prog2
    (setq lst (errorset!> (list 'rotas0!> (list 'quote lst))
                          ![erst1!] ![erst2!]))
    (cond ((atom lst) (erm!> lst) (erm!> 8803) (msg!> 88033) !!er!!)
          (t          (car lst))) ))

(de rotas0!> (lst)  % 05.96
  (proc (w wa wm wr wc)
    (cond ((sp!>) (setq ![er!] 78041) (return !!er!!))) % null metric!
    (setq wm '(mat (0 1) (-1 0)))
    (cond ((null lst) (prog2 (setq wr t) (go lab)))) % matrix from ls
    % translating the rotation matrix ...
    (cond ((or (atom lst) (cdr lst) (atom(car lst)))
             (setq ![er!] 8500) (return !!er!!)))
    (setq lst (memlist!> '!, (car lst)))
    (cond ((or (eq lst !!er!!) (not(eqn (length lst) 2)))
             (setq ![er!] 8500) (return !!er!!)))
    (while!> lst
      (setq wa (car lst))
      (setq lst (cdr lst))
      (cond ((or (cdr wa) (atom(car wa)))
              (setq ![er!] 8500) (return !!er!!)))
      (setq wa (memlist!> '!, (car wa)))
      (cond ((or (eq wa !!er!!) (not(eqn (length wa) 2)))
              (setq ![er!] 8500) (return !!er!!)))
      (setq wa (mapcar wa (function translate!>)))
      (cond ((memq !!er!! wa) (return !!er!!)))
      (setq wa (mapcar wa 'nullzero!>))
      (cond ((memq !!er!! wa) (setq ![er!] 8500) (return !!er!!)))
      (setq w (cons wa w)))
    lab % here we should have the matrix ...
    (cond (wr (cond (!#!L!S (setq w !#!L!S))
                    (t (setq ![er!] 4001) (return !!er!!))))
          (t (setq w (reverse w))))
    (setq wa (aeval (list 'times (cons 'mat w)
                                 wm
                                 (list2 'tp (cons 'mat w)) )))
    (cond ((not(equal wa wm)) % chek for sl(2,c)
            (setq ![er!] 8501) (return !!er!!)))
    (setq ![ls!] w)
    (ls!-li!>)  % ls -> li
    (li!-l!>)   % li -> l
    (setq w (altdata!>(alldata!>)))
    (setq ![dens!] nil) % no density for spinorial rotations
    (while!> w  % rotate all known objects ...
      (setq wc (car w))
      (cond ((or (memq wc % skipping silently ...
                      '( ![cord!] ![const!] ![fun!] ![sol!] ![apar!]
                         !#!L !#!L!S !#!b !#!e ))
                 (null(get wc '!=idxl)))  nil)
            ((flagp wc '!+hold) (nonrot!> wc)) % skipping noisily...
            (t % rotating particular object ...
               (set wc (allcoll!> (eval wc) wc nil
                                  (cond ((get wc '!=idxl) (get wc '!=idxl))
                                        (t '(0)))
                                  (function rotatel!>)))
	       (cond
		 ((flagp wc '!+uconn) (gammascorrect!> (eval wc) nil))
		 ((flagp wc '!+dconn) (gammascorrect!> (eval wc) t))
		 ((flagp wc '!+fconn) (gammacorrect!> (eval wc) )))
               ))
      (setq w (cdr w)))
    (clearandfinish!>)))

(de clearandfinish!> nil  % 05.96
  (progn
    % clearing all matrices ...
    (setq ![l!] nil)
    (setq ![li!] nil)
    (setq ![dl!] nil)
    (setq ![sdl!] nil)
    (setq ![ls!] nil)
    (setq ![dens!] nil)
    (setq ![dex!] nil)
    (setq ![dfx!] nil)
    (setq ![x!] nil)
    % new types of frame and metric ...
    (ftype!>)
    (mtype!>)
    (fitype!>)
    (mitype!>)
    % done message ...
    (done!>) ))

% Build tensorial rotation from spinorial ...
(de ls!-li!> nil  % 05.96
  (prog (wa wb)
    (setq ![li!] (mkt!> 2))
    (fordim!> a  do (fordim!> b do (progn
      (setq wa (tenspini!> a))
      (setq wb (tenspini!> b))
      (putel!> (evalalg!>(list 'times  (getel2!> ![ls!] (car wb) (car wa))
                           (coalg!>(getel2!> ![ls!] (cdr wb) (cdr wa)))))
               ![li!] (list2 b a)))))))

(de tenspini!> (w)  % 05.96
  (cond ((eqn w 0) '(1 . 1))
        ((eqn w 1) '(0 . 0))
        ((eqn w 2) '(1 . 0))
        ((eqn w 3) '(0 . 1))))

% Build inverse transposed matrix ...
(de li!-l!> nil  % 05.96
  (progn (setq ![l!] (mkt!> 2))
         (rmat!> ![l!] (aeval(list 'quotient 1
                                   (list 'tp (mat!> ![li!])))))))
(de l!-li!> nil
  (progn (setq ![li!] (mkt!> 2))
         (rmat!> ![li!]
                 (aeval(list 'quotient 1 (list 'tp (mat!> ![l!])))))))

% Correction for spinorial connection ...
% WB=NIL - Undotted, WB=T - Dotted
(de gammascorrect!> (w wb) % 05.96
  (progn
    (putel1!> (evalform!> (dfsum!> (list
      (getel1!> w 0)
      (fndfpr!> (ls!> 0 1 wb) (dfunsgn!>(ls!> 0 0 wb)))
      (chsign!> t (fndfpr!> (ls!> 0 0 wb) (dfunsgn!>(ls!> 0 1 wb)))))))
      w 0)
    (putel1!> (evalform!> (dfsum!> (list
      (getel1!> w 1)
      (fndfpr!> (ls!> 1 1 wb) (dfunsgn!>(ls!> 0 0 wb)))
      (chsign!> t (fndfpr!> (ls!> 1 0 wb) (dfunsgn!>(ls!> 0 1 wb)))))))
      w 1)
    (putel1!> (evalform!> (dfsum!> (list
      (getel1!> w 2)
      (fndfpr!> (ls!> 1 1 wb) (dfunsgn!>(ls!> 1 0 wb)))
      (chsign!> t (fndfpr!> (ls!> 1 0 wb) (dfunsgn!>(ls!> 1 1 wb)))))))
      w 2)))

(de dfunsgn!> (lst)  % 05.96
  (cond ((pmmm!>) (chsign!> t (dfun!> lst)))
        (t                    (dfun!> lst))))

% aux function ...
(de nullzero!> (w) % 05.96
  (cond ((null w) nil)
        ((zerop(car w)) (cdr w))
        (t !!er!!)))

%--- Rotation of single element  03.91, 05.96 ---------------------------

% WI - Current Indices, WN - Internal Variable
(de rotatel!> (lst wi wn)
 (cond
   ((syaidxp!> wi (get wn '!=sidxl)) % if wi is in canonic order ...
     (cond
      (![dens!] (dcorr!> wn (rotatel1!> wi nil (get wn '!=idxl) wn t nil)))
      (t                    (rotatel1!> wi nil (get wn '!=idxl) wn t nil))))
   (t nil)))

% WA,WI - Current Indices, WD - IDXL, WN - Int. Variable
(de rotatel1!> (wi wa wd wn wf wc) % 05.96
  (cond
    % Last element (IDXL is empty), so getting the value of the element
    ((null wd) (getsa0!> wn (reverse wa)))
    % Enumerating or Holonomic index, skipping ...
    ((or (enump!> (car wd)) (holp!> (car wd)))
       (rotatel1!> (cdr wi)
                   (cons (car wi) wa)
                   (cdr wd)
                   wn t nil))
    % Spinorial index ...
    ((spinp!>(car wd)) (prog (w wl we wx)
      (cond (wf (setq wa (cons 0 wa))
                (setq wc (dotp!>(car wd)))
                (setq wf nil)))
      (foreach!> x in '(0 1) do (progn
	(setq wx (cond ((lessp (car wi) (cdar wd)) 0) (t 1)))
        (cond
	  ((upperp!>(car wd))
             (setq wl (lsi!> wx x wc)))
          (t (setq wl (ls!>  wx x wc))))
        (cond (wl (progn
          (setq we (rotatel1!>
                     (cond ((eqn (cdar wd) 1) (cdr wi)) (t wi))
                     (cons (plus (car wa) x) (cdr wa))
                     (cond ((eqn (cdar wd) 1) (cdr wd))
                           (t (cons (cons (caar wd) (sub1(cdar wd)))
                                    (cdr wd))))
                     wn
                     (cond ((eqn (cdar wd) 1) t) (t nil))
                     wc
		     ))
          (cond (we (setq w
              (cons (cond ((algp!> wn) (multax!> wl we))
                          (t           (multfx!> wl we)))
                    w)))))))))
      (return (cond ((null w)             nil)
                    ((algp!> wn) (summax!> w))
                    (t           (summfx!> w))))))
    % Frame index ...
    (t(prog (w wl we)
      (fordim!> x do (progn
        (setq wl (lli!> (car wi) x (car wd)))
        (cond (wl (progn
          (setq we (rotatel1!>
                      (cdr wi)
                      (cons x wa)
                      (cdr wd)
                      wn t nil))
          (cond (we (setq w
              (cons (cond ((algp!> wn) (multax!> wl we))
                          (t           (multfx!> wl we)))
                    w)))))))))
      (return (cond ((null w)             nil)
                    ((algp!> wn) (summax!> w))
                    (t           (summfx!> w))))))))


% Element of LS matrix or ~LS matrix ...
(de ls!> (wa wb wc)  % 05.96
  (cond (wc (coalg!> (getel2!> ![ls!] wa wb)))
        (t           (getel2!> ![ls!] wa wb))))

% Element of inverse transposed spinorial matrix ...
(de lsi!> (wa wb wc) % 05.96
  (cond ((and (eqn wa 0) (eqn wb 0))            (ls!> 1 1 wc))
        ((and (eqn wa 0) (eqn wb 1)) (chsigna!> (ls!> 1 0 wc)))
        ((and (eqn wa 1) (eqn wb 0)) (chsigna!> (ls!> 0 1 wc)))
        ((and (eqn wa 1) (eqn wb 1))            (ls!> 0 0 wc))))

% Element of L or LI matrix ...
(de lli!> (wa wb wc)  % 05.96
  (cond (wc (getel2!> ![l!]  wa wb))
        (t  (getel2!> ![li!] wa wb))))


%---------- Tensorial rotation 15.03.91, 05.96 ---------------------------

% Main function ...
(de rotat!> (lst bool) % 05.96
  (prog2
    (setq lst (errorset!> (list 'rotat0!> (list 'quote lst) bool)
                         ![erst1!] ![erst2!]))
    (cond ((atom lst) (erm!> lst) (erm!> 8803) (msg!> 88033) !!er!!)
          (t          (car lst))) ))

% BOOL=T - Transformation, BOOL=NIL - Rotation
(de rotat0!> (lst bool)
  (proc (w wa wm we wb wr wd wc)
    (cond ((null bool) % for rotation we need metric ...
      (setq ![chain!] nil)
      (setq we (request!> '!#!G))
      (cond ((eq we !!er!!) (return we))
            ((null we) (trsf!> '!#!G)
                       (prin2 "Cannot perform rotation without Metric.")
                       (terpri) (setq ![er!] 6046) (return !!er!!))) ))
    (cond ((null lst) (prog2 (setq wr t) (go lab))))% matrix from L
    (cond ((or (atom lst) (cdr lst) (atom(car lst)))% matrix in the command
            (prog2 (setq ![er!] 8500) (return !!er!!))))
    (setq lst (memlist!> '!, (car lst)))
    (cond((or (eq lst !!er!!) (not(eqn (length lst) ![dim!])))
           (prog2 (setq ![er!] 8500) (return !!er!!))))
    (while!> lst
      (setq wa (car lst)) (setq lst(cdr lst))
      (cond((or(cdr wa)(atom(car wa)))
             (prog2 (setq ![er!] 8500) (return !!er!!))))
      (setq wa (memlist!> '!, (car wa)))
      (cond ((or (eq wa !!er!!) (not(eqn (length wa) ![dim!])))
             (prog2 (setq ![er!] 8500) (return !!er!!))))
      (setq wa (mapcar wa (function translate!>)))
      (cond ((memq !!er!! wa) (return !!er!!)))
      (setq wa (mapcar wa 'nullzero!>))
      (cond ((memq !!er!! wa) (prog2 (setq ![er!] 8500) (return !!er!!))))
      (setq w (cons wa w)) )
    lab % here in w we should have the matrix already ...
    (cond (wr (cond (!#!L (setq w !#!L))
                    (t  (prog2 (setq ![er!] 4001) (return !!er!!)))))
          (t (setq w (reverse w))))
    (cond (bool(go lab1))) % transformation -> skipping correct rotation
    % checking for correct rotation ...
    (setq wm !#!G)
    (setq wm (cons 'mat (mapcar wm 'aeval2!>)))
    (setq wa (aeval (list 'times (cons 'mat w)
                                 wm
                                 (list2 'tp (cons 'mat w)) )))
    (cond ((not (equal wa wm)) % check for correct rotation
            (prog2 (setq ![er!] 8502) (return !!er!!))))
    lab1
    % Here W is the matrix ...
    (setq wd (raeval!>(list 'det (cons 'mat w)))) % wd=detl
    (cond ((or (null wd) (zerop wd))
            (prog2 (setq ![er!] 8504) (return !!er!!))))
    (setq ![l!] w)
    (setq ![dl!] wd)
    % The most sabtle point in all machinery with densityes
    % and pseudotensors. We choose sign factor as
    %   sdl = detL * sqrt(1/(detL)^2)                <- we use this!
    % this gives transformation for pseudo tensors consistent
    % with their calculation after transformation. The sabtle
    % point is for imagenary detL this definition of sdl is
    % quite strange and is different from another
    %   sdl1 = detL/sqrt((detL)^2)
    % in fact for positive real "a" we have:
    %   detL:     sdl:    sdl1:
    %     a        1        1
    %    -a       -1       -1
    %   i*a       -1        1
    %  -i*a        1       -1
    % Actually the whole problem is in the way how to choose
    % the branch of sqrt.
    (setq ![sdl!] (raeval!>
      (list 'times ![dl!]
	           (list 'sqrt (list 'quotient 1
                                               (list 'expt ![dl!] 2))))))
    (l!-li!>)
    (setq w (altdata!>(alldata!>)))
    (while!> w
      (setq wc (car w))
      (cond ((memq wc  '(![cord!] ![const!] ![fun!] ![sol!] ![apar!]
                         !#!b !#!e ))
               nil)
            ((flagp wc '!+hold) (nonrot!> wc))
	    ((isspinor!> wc)    (nonrot!> wc))
            (t (prepldens!> wc)
               (set wc
                 (allcoll!> (eval wc )  wc  nil
                            (cond ((get wc '!=idxl) (get wc '!=idxl))
                                  (t '(0)))
                            (function rotatel!>)))
	       (cond
		 ((flagp wc '!+fconn) (gammacorrect!> (eval wc) )))
               ))
      (setq w (cdr w)))
    (clearandfinish!>)))

(de aeval2!> (w) (mapcar w 'aeval1!>))
(de aeval1!> (w) (aeval(nz!> w)))

% Correction for connection ...
(de gammacorrect!> (w) % 05.96
  (fordim!> a  do
    (fordim!> b  do
       (putel!>
         (evalform!> (dfsum!> (cons (getel2!> w a b)
                                    (mkldli!> a b))))
         w (list2 a b)))))

(de mkldli!> (wa wb) % 05.96
  (foreach!> wx in (dimlist!> 0) collect
    (fndfpr!> (getel2!> ![l!] wa wx)
	      (dfun!> (getel2!> ![li!] wb wx)))))

(de nonrot!> (wd) % 05.96
  (progn (gprinreset!>)
         (gprin!> "WARNING: ")
         (pn!> wd)
         (gprils0!> (cond
             ((flagp wd '!+pl) '("remain" "unchanged."))
             (t '("remains" "unchanged."))))
         (gterpri!>)))

(de dcorr!> (wn w)
  (cond ((algp!> wn) (multax!> ![dens!] w))
	(t           (multfx!> ![dens!] w))))

(de prepldens!> (wn)
  (prog (w)
    (setq w (get wn '!=dens))
    (cond
      ((null w)
         (setq ![dens!] nil))
      ((and (null(caddr w)) (null(cadddr w)))
         (setq ![dens!] nil))
      ((null(cadddr w))
         (setq ![dens!] ![sdl!]))
      ((null(caddr w))
         (setq ![dens!] (list 'expt ![dl!] (cadddr w))))
      (t (setq ![dens!]
           (list 'times ![sdl!] (list 'expt ![dl!] (cadddr w))))))
    (return ![dens!])))


%--- Coordinates Transformations 25.02.91, 05.96 -------------------------

% Main Function ...
(de chcoord!> (lst)
  (prog2
    (setq lst (errorset!> (list 'chcoord0!> (list 'quote lst))
                          ![erst1!] ![erst2!]))
    (cond ((atom lst) (erm!> lst) (erm!> 8803) (msg!> 88033) !!er!!)
          (t          (car lst))) ))

(de chcoord0!> (lst) % 05.96 ...
 (proc (w wn wa wb wd)
   (cond ((null lst) (return nil)))
   (setq wn 0)
   (setq ![xb!] nil)
   (while!> (and lst (not(eqs!> (car lst) 'with))) % word!!!
     (setq w (cons (car lst) w))
     (setq lst (cdr lst)))
   (cond ((or (null w) (null lst) (null(cdr lst)))
            (setq ![er!] 8375) (return !!er!!)))
   (setq w (memlist!> '!, (reverse w)))
   (setq lst (memlist!> '!, (cdr lst)))
   (cond ((or (eq w !!er!!)
              (eq lst !!er!!)
              (not(eqn (length lst) ![dim!]))
              (not(eqn (length w) ![dim!])))
           (setq ![er!] 8375) (return !!er!!)))
   (setq ![ocord!] ![cord!])
   (setq ![cord!] nil)
   (while!> w % new coordinates list ...
     (cond ((or (cdar w) (not(idp(caar w))))
        (setq ![er!] 8375) (remnew!>) (return !!er!!)))
     (cond ((flagp(caar w) '!+grg)
        (setq ![er!] 5013) (doub!>(caar w)) (remnew!>) (return !!er!!)))
     (flag (car w) 'used!*)
     (flag (car w) '!+grgvar)
     (flag (car w) '!+grg)
     (put (caar w) '!=cord wn)
     (cond (![apar!] (depend (cons (caar w) ![apar!]))))
     (setq ![cord!] (cons (caar w) ![cord!]))
     (setq wn (add1 wn))
     (setq w (cdr w)))
   (setq ![cord!] (reverse ![cord!]))
   (setq ![dfx!] (mkt!> 1))
   (setq ![x!] (mkt!> 1))
   (while!> lst % x = f(x') ...
     (setq wa (car lst))
     (setq lst (cdr lst))
     (cond ((or (null(cdr wa)) (null(cddr wa))
                (not(eq (cadr wa) '=)) (not(idp(car wa)))
                (not (memq (car wa) ![ocord!])) )
              (setq ![er!] 8375) (remnew!>) (return !!er!!))
           ((memold!> (cddr wa))
              (setq ![er!] 8388) (remnew!>) (return !!er!!)))
     (setq wb (translate!>(cddr wa)))
     (cond ((eq wb !!er!!) (remnew!>) (return !!er!!))
           ((not(zerop(car wb)))
              (setq ![er!] 8389) (remnew!>) (return !!er!!)))
     (setq wd (evalform!> (dfun1!> (cdr wb) nil)))
     (putel1!> (cdr wb) ![x!]   (get (car wa) '!=cord))
     (putel1!> wd       ![dfx!] (get (car wa) '!=cord)) )
   (setq w (evalform!>(dfprod!> ![dfx!])))
   (cond ((null w) (setq ![er!] 8377)(remnew!>)(return !!er!!)))
   (setq ![dbas!] nil)
   (idfx!>) % d x -> /d x
   (ncfdep!>) % rebuilding implicit dependence
   (evalcomm!> '(all) (function ncel!>)) % transform all objects ...
   (remold!>) % remove old coordinates
   (copar1!> (ncons ![cord!])) % conjugated pairs
   (cond (![umod!] (mktables!>))) % refreshing tables in amode
   % now transforming holonomic indices ...
   (crotat0!>)
   % finish ...
   (clearandfinish!>)))

(de ncel!> (lst wi wn)
  (cond ((null lst) nil)
	% in holonomic regime frame/inv frame stay holonomic
	((and (eq wn '!#!T) (holonomicp!>)) lst)
	((and (eq wn '!#!D) (holonomicp!>)) lst)
        ((eq wn '!#!b) (ncform0!> lst))                       % b
        ((eq wn '!#!e) (ncvec0!> lst))                        % e
        ((and (zerop(gettype!> wn)) (not (flagp wn '!+equ)))  % alg
          (ncalg!> lst))
        ((and (eqn(gettype!> wn)-1)(not (flagp wn '!+equ)))   % vec
          (ncvec!> lst))
        ((not (flagp wn '!+equ))                              % form
          (ncform!> lst))
        ((zerop(gettype!> wn))                                % eq alg
          (equation!> (ncalg!>(cadr lst)) (ncalg!>(caddr lst))))
        ((eqn(gettype!> wn)-1)                                % eq vec
          (equation!> (ncvec!>(cadr lst)) (ncvec!>(caddr lst))))
        (t                                                     % eq alg
          (equation!> (ncform!>(cadr lst)) (ncform!>(caddr lst))))
        ))

% New coord for algebraic expression ...
(de ncalg!> (w)
  (cond ((null w) w)
        (t (evalalg!> (ncalg0!> w)))))

(de ncalg0!> (w)
  (cond ((and (idp w) (get w '!=cord))
           (getel1!> ![x!] (get w '!=cord)))
        ((atom w) w)
	((eq (car w) 'dfp) (list 'dfp (ncalg!>(cadr w)) (caddr w)))
        ((eq (car w) 'df)  (ncdf!> (ncalg!>(cadr w)) (cddr w)))
	((or (eq (car w) '!*sq) (eq (car w) 'taylor!*)) (err!> 9999))
        (t (mapcar w (function ncalg0!>)))))

% New coord for DF(...) ...
(de ncdf!> (w wl) % w - expr, wl - diff list
  (cond ((null wl) w)
        (t(prog (wb wn wd)
	    % wd - diff or number of coordinate
            (cond ((and (atom(car wl)) (memq (car wl) ![ocord!]))
                     (setq wd (get (car wl) '!=cord)))
                  (t (prog2 (setq wb t) (setq wd (car wl)))))
	    % wn - how many times
            (cond ((and (cdr wl) (numberp(cadr wl)))
                     (prog2 (setq wn (cadr wl)) (setq wl (cddr wl))))
                  (t (prog2 (setq wn 1) (setq wl (cdr wl)))))
	    % not coordinate, so exiting
            (cond (wb
              (return (ncdf!> (list 'df w (ncalg!> wd) wn) wl))))
            (setq wd (getel1!> ![dex!] wd))
	    % we diffentiate wn times
            (for!> x (1 1 wn) do (setq w (vfun!> wd w)))
            (return (ncdf!> w wl))))))

% New coord for form ...
(de ncform!> (w)
  (cond ((null w) w)
        (t (evalform!> (dfsum!> (mapcar w (function ncform1!>)))))))

(de ncform1!> (w)
  (fndfpr!> (ncalg!> (car w))
            (ncxb!> (cdr w) ![umod!])))

% New coord for d X/\d Y/\...
(de ncxb!> (w wm)
  (cond
    (wm (ncons (cons 1 w)))
    ((assoc (car w) ![xb!]) (cadr(assoc (car w) ![xb!])))
    (t(progn
        (setq ![xb!] (cons
           (list2 (car w) (evalform!> (mkxb!>(cdr w))))
           ![xb!]))
        (cadar ![xb!])))))

(de mkxb!> (w)
  (proc (wa wn)
    (setq wn 0)
    (while!> w
      (cond ((caar w) (setq wa (cons (getel1!> ![dfx!] wn) wa))))
      (setq wn (add1 wn))
      (setq w (cdr w)))
    (return (evalform!> (dfprod!> (reverse wa))))))

(de ncform0!> (w)
  (cond ((null w) w)
        (t (evalform!> (dfsum!> (mapcar w (function ncform00!>)))))))

(de ncform00!> (w)
  (fndfpr!> (ncalg!> (car w))
            (ncxb!> (cdr w) nil)))

% New coord for vector ...
(de ncvec!> (w)
  (cond ((null w) w)
        (t (evalform!> (dfsum!> (mapcar w (function ncvec1!>)))))))

(de ncvec1!> (w)
  (fndfpr!> (ncalg!> (car w))
            (ncxv!> (cdr w) ![umod!])))

(de ncxv!> (w wm)
  (proc (wc)
    (cond (wm (return (ncons (cons 1 w)))))
    (setq wc -1)
    (setq w (car w))
    (while!> (not(eqn w 1))
      (setq w (quotient w 2))
      (setq wc (add1 wc)) )
    (return (getel1!> ![dex!] wc)) ))

(de ncvec0!> (w)
  (cond ((null w) w)
        (t (evalform!> (dfsum!> (mapcar w (function ncvec00!>)))))))

(de ncvec00!> (w)
  (fndfpr!> (ncalg!> (car w))
            (ncxv!> (cdr w) nil)))

% d x -> /d x
(de idfx!> nil
  (prog (w)
    (setq ![dex!] (mkt!> 1))
    (setq w (aeval (list 'tp (list 'quotient 1 (mkmtetr!> ![dfx!])))))
    (mktetrm!> (cdr w) ![dex!])
    (return t)))

% New coord for implicit function dependence ...
(de ncfdep!> nil
  (prog (wd wn)
    (foreach!> x in ![fun!] do (prog2
      (setq wd (get x '!=depend))
      (cond (wd (progn
        (setq wn (vard!> (ncalg0!> wd)))
        (nodepend wd)
        (depend wn)
        (put x '!=depend wn))))))))

(de vard!> (lst)
  (cond ((and (atom lst) (flagp lst '!+grgvar)) (ncons lst))
        ((atom lst) nil)
        (t (appmem!> (vard!>(car lst)) (vard!>(cdr lst))))))

(de memold!> (w)
  (cond ((and (atom w) (memq w ![ocord!])) t)
        ((atom w) nil)
        (t (or (memold!>(car w)) (memold!>(cdr w))))))

(de remold!> nil
  (progn (remflag ![ocord!] '!+grg)
         (remflag ![ocord!] '!+grgvar)
         (remflag ![ocord!] 'used!*)
         (foreach!> x in ![ocord!] do (progn
	    (cond (![apar!] (nodepend (cons x ![apar!]))))
            (remprop x '!=cord)
            (remprop x '!=conj)))
         (setq ![xb!] nil)
         (setq ![ocord!] nil)
         ))

(de remnew!> nil
  (progn (remflag ![cord!] '!+grg)
         (remflag ![cord!] '!+grgvar)
         (remflag ![cord!] 'used!*)
         (foreach!> x in ![cord!] do (progn
	    (cond (![apar!] (nodepend (cons x ![apar!]))))
            (remprop x '!=cord)))
         (setq ![cord!] ![ocord!])
         (setq ![dex!] nil)
         (setq ![dfx!] nil)
         (setq ![x!] nil)
         (setq ![xb!] nil)
         (setq ![ocord!] nil)
         ))

(de crotat0!> nil
  (proc (w wa wm we wb wr wd wc)
    % here w is the matrix ...
    (setq w (foreach!> a in (dimlist!> 0) collect
              (foreach!> b in (dimlist!> 0) collect
                (getfdx!> (getel1!> ![dex!] b) a))))
    (setq wd (raeval!> (list 'det (cons 'mat w))))
    (cond ((or (null wd) (zerop wd))
            (prog2 (setq ![er!] 8377) (return !!er!!))))
    (setq ![l!] w)    % d = d xnew/d xold
    (setq ![dl!] wd)  % detd
    (setq ![sdl!] (raeval!> % sign(detd)
      (list 'times ![dl!]
	           (list 'sqrt (list 'quotient 1
                                               (list 'expt ![dl!] 2))))))
    (l!-li!>)         % d^(-1)
    (setq w (altdata!>(alldata!>)))
    % transforming all ...
    (while!> w
      (setq wc (car w))
      (cond ((memq wc  '(![cord!] ![const!] ![fun!] ![sol!] ![apar!] % skipping
                         !#!b !#!e ))
               nil)
	    ((and (holonomicp!>) (eq wc '!#!T)) (msg!> 8391))  % keep T
	    ((and (holonomicp!>) (eq wc '!#!D)) (msg!> 8392))  % keep D
	    ((not(mustbecrotated!> wc)) nil)                  % skipping
            ((flagp wc '!+hold) (nonrot!> wc))        % skipping noisily
            (t (cprepdens!> wc)   % prepare density
               (set wc
                 (allcoll!> (eval wc )  wc  nil
                            (cond ((get wc '!=idxl) (get wc '!=idxl))
                                  (t '(0)))
                            (function crotatel!>)))
	       % correct connection
	       (cond
		 % holonomic ...
		 ((flagp wc '!+hconn) (gammacorrect!> (eval wc)))
		 % in holonomic regime holonomir = frame ...
		 ((and (flagp wc '!+fconn) (holonomicp!>))
				      (gammacorrect!> (eval wc))))
               ))
      (setq w (cdr w)))
    ))

% Defines whether this object requires any cord rotation or not ...
(de mustbecrotated!> (w)
  (or (hashol!> w)                        % it has hol. index
      (and (holonomicp!>) (hasfram!> w))  % in hol. regime hol.=frame
      (get w '!=dens)))                   % density correction

% Rotate an element ...
(de crotatel!> (lst wi wn)
 (cond
   ((syaidxp!> wi (get wn '!=sidxl)) % if wi is in canonic order ...
     (cond
       (![dens!] (dcorr!> wn (crotatel1!> wi nil (get wn '!=idxl) wn t nil)))
       (t                    (crotatel1!> wi nil (get wn '!=idxl) wn t nil))))
   (t nil)))

% Prepares density correction ...
(de cprepdens!> (wn)
  (prog (w)
    (setq w (get wn '!=dens))
    % In hol. regime if exists DENS for frame roration
    % then we use it ...
    (cond ((and w (holonomicp!>) (or (caddr w) (cadddr w)))
	     (return (prepldens!> wn))))
    (cond
      ((null w)
         (setq ![dens!] nil))
      ((and (null(car w)) (null(cadr w)))
         (setq ![dens!] nil))
      ((null(cadr w))
         (setq ![dens!] ![sdl!]))
      ((null(car w))
         (setq ![dens!] (list 'expt ![dl!] (cadr w))))
      (t (setq ![dens!]
           (list 'times ![sdl!] (list 'expt ![dl!] (cadr w))))))
    (return ![dens!])))

% WA,WI - Current Indices, WD - IDXL, WN - Int. Variable
(de crotatel1!> (wi wa wd wn wf wc) % 05.96
  (cond
    % Last element (IDXL is empty), so getting the value of the element
    ((null wd) (getsa0!> wn (reverse wa)))
    % Enumerating or Spinor index, or Frame in Nonholonomic skipping ...
    ((or (enump!> (car wd))
         (spinp!> (car wd))
	 (and (tetrp!> wd) (not(holonomicp!>))))
       (crotatel1!> (cdr wi)
                    (cons (car wi) wa)
                    (cdr wd)
                    wn t nil))
    % Holonomic of Frame in holonomic mode index ...
    (t(prog (w wl we)
      (fordim!> x do (progn
        (setq wl (lli!> (car wi) x (upperp!>(car wd))))
        (cond (wl (progn
          (setq we (crotatel1!>
                      (cdr wi)
                      (cons x wa)
                      (cdr wd)
                      wn t nil))
          (cond (we (setq w
              (cons (cond ((algp!> wn) (multax!> wl we))
                          (t           (multfx!> wl we)))
                    w)))))))))
      (return (cond ((null w)             nil)
                    ((algp!> wn) (summax!> w))
                    (t           (summfx!> w))))))))

%----- Lie Derivatives ---------------------------------------------------

(de lietr!> (lst)
  (prog (wv wn wi wi1 wl wm wsi wr)
    % wv - vector, wn - int.var. of differentiated object
    % wi - idxl of wn, wl - indices, wm - manipulations
    % wi1 - new idxl after manipulation
    (setq lst (memlist!> '!, lst))
    (cond ((eq lst !!er!!) (err!> 2020))
          ((not(eqn (length lst) 2)) (err!> 2500)))
    (setq wv (unitra0!> (car lst)))  % vector
    (setq lst (cadr lst))            % lst = (id (...))
     % Internal variable ...
    (cond ((not(idp(car lst))) (err!> 2500))
          (t (setq wn (incomiv!>(explode(car lst))))))
    (cond ((not (or (flagp wn '!+ivar) (flagp wn '!+macros2))) (err!> 2500))
	  ((flagp wn '!+noncov) (err!> 2502)))
    % Indices ...
    (setq wi (get wn '!=idxl))
    (cond
      ((null wi)
	(cond ((not(eqn (length lst) 1)) (err!> 2207)))
	(setq wi nil)
	(go lab))
      ((null(cdr lst)) (err!> 2207))
      ((not(pairp(cadr lst))) (err!> 2102)))
    (setq lst (memlist!> '!, (cadr lst)))
    (cond ((eq lst !!er!!) (err!> 2020))
          ((not(eqn (length lst) (length wi))) (err!> 2207)))
    (setq wm (mapcar lst 'selmani!>)) % manipulations
    (setq lst (mapcar lst 'delmani!>))
    (setq wl (mapcar lst (function unitra0!>)))
    (setq wi1 (chidxl!> wi wm))
    % Maybe we need T and D ...
    (cond ((frameorspin!> wi1)  (require!> '( !#!T !#!D ))))
    lab
    (cond ((get wn '!=dens)  (require!> '( !#!T !#!D ))))
    % Einstein summation ...
    (setq wsi (intersecl!> (freevar!> wv ![extvar!])
                           (freevar!> wl ![extvar!])))
    % result ...
    (setq wr (list 'lieexec!> wn wi1 wl wm wv))
    (cond (wsi (setq wr (mkeinsum0!> wsi wr))))
    (return wr)
    ))

(de frameorspin!> (wi)
  (cond ((null wi) nil)
	((or (spinp!>(car wi)) (tetrp!>(car wi))) t)
	(t (frameorspin!>(cdr wi)))))

(de chidxl!> (wi wm)
  (cond ((null wi) nil)
	(t (cons (chidxl1!> (car wi) (car wm))
		 (chidxl!> (cdr wi) (cdr wm))))))

(de chidxl1!> (wi wm)
  (cond
    ((null wm)     wi)
    ((enump!> wi)  wi)
    ((eqn wm 1) % ' cvalificator - up
                  (cond
		    ((and (spinp!> wi) (not(upperp!> wi)))
                       (spinup!> wi))    % .s -> 's
                    ((holpd!> wi)   t)   % .g -> 't
                    ((tetrpd!> wi)  t)   % .t -> 't
                    ((holpu!> wi)   t)   % 'g -> 't
                    (t wi)))
    ((eqn wm 2) % . cvalificator - down
                  (cond
                    ((and (spinp!> wi) (upperp!> wi))
		       (spindown!> wi))  % 's -> .s
                    ((holpu!> wi)  nil)  % 'g -> .t
                    ((tetrpu!> wi) nil)  % 't -> .t
                    ((holpd!> wi)  nil)  % .g -> .t
                    (t wi)))
    ((eqn wm 3) % ^ cvalificator - g up
                  (cond
                    ((spinp!> wi) (err!> 9913))
                    ((holpd!> wi)  1)    % .g -> 'g
                    ((tetrpd!> wi) 1)    % .t -> 'g
                    ((tetrpu!> wi) 1)    % 't -> 'g
                    (t wi)))
    ((eqn wm 4) % _ cvalificator - g down
                  (cond
                    ((spinp!> wi) (err!> 9913))
                    ((holpu!> wi)  0)    % 'g -> .g
                    ((tetrpu!> wi) 0)    % 't -> .g
                    ((tetrpd!> wi) 0)    % .t -> .g
                    (t wi)))
    ))

(de spinup!> (wi)
  (cond ((eq (car wi) 'u) (cons 'uu (cdr wi)))
	((eq (car wi) 'd) (cons 'ud (cdr wi)))
	(t wi)))

(de spindown!> (wi)
  (cond ((eq (car wi) 'uu) (cons 'u (cdr wi)))
	((eq (car wi) 'ud) (cons 'd (cdr wi)))
	(t wi)))

(de cdrnil!> (w)
  (cond ((null w) nil)
	(t (cdr w))))

% wv - vector, wn - int. variable, wi - modified idxl
% wl - index list, wm - ind. manipulations
(de lieexec!> (wn wi wl wm wv)
  (prog (wt wr w0 ww wi1 wl0 wl1 wc wd)
    % evaluating vector ...
    (setq wv (unieval!> wv))
    (cond ((null wv) (return nil))
	  ((not(eqn (car wv) -1)) (err!> 2501)))
    (setq wv (cdr wv))
    % evaluating indices ...
    (setq wl (mapcar wl 'unieval!>))
    % type of expression ...
    (setq wt (get wn '!=type))
    % main element of lie derivative
    (setq ww (cdrnil!>(funapply!> wn wl wm)))
    (setq w0 ww)
    (cond ((eqn wt 0)  (setq wr (ncons(vfun!> wv ww))))        % ksi | w
	  ((eqn wt -1) (setq wr (ncons(vbrack!> wv ww))))      % [ksi,w]
	  ((eqn wt  1) (setq wr (list2
				  (vform!> wv (dex!> ww))      % ksi _| d w
				  (dfun!> (vform1!> wv ww))))) % + d ksi _| w
	  (t           (setq wr (list2
				  (vform!> wv (dex!> ww))      % ksi _| d w
				  (dex!> (vform!> wv ww))))))  % + d ksi _| w
    (setq wl1 wl)
    (setq wi1 wi)
    % for all indices ...
    (while!> wl1
      (cond
	% frame or holonomic ...
	((or (tetrp!>(car wi1)) (holp!>(car wi1)))
	  (fordim!> x do (progn
	    (setq wc (liecoef!> (tonumb!>(car wl1)) x wv (car wi1)))
	    (cond (wc
              (setq ww (cdrnil!>(funapply!> wn
                                            (app!> wl0 (cons (tocalg!> x)
                                                             (cdr wl1)))
                                            wm)))))
	    (cond (wc
	      (setq wr (cons (cond ((zerop wt) (mktimes2!> wc ww))
				   (t          (fndfpr!> wc ww)))
			     wr)))))))
	% spinorial index ...
	((spinp!>(car wi1))
	  (for!> x (0 1 2) do (progn
	    (setq wc (liespin!> (tonumb!>(car wl1)) x wv (car wi1)))
	    (cond (wc
              (setq ww (cdrnil!>(funapply!> wn
                                            (app!> wl0
                                              (cons
                                                (tocalg!>
                                                  (sind!> (tonumb!>(car wl1))
                                                           x (car wi1)))
                                                (cdr wl1)))
                                            wm)))))
	    (cond (wc
	      (setq wr (cons (cond ((zerop wt) (mktimes2!> wc ww))
				   (t          (fndfpr!> wc ww)))
			     wr)))))))
	(t nil))
      (setq wl0 (cons (car wl1) wl0))
      (setq wl1 (cdr wl1))
      (setq wi1 (cdr wi1)))
    % density ...
    (setq wd (get wn '!=dens))
    (cond (wd
      (setq wd (mkplus2!>
		 (mktimes2!> (cadr wd) (ksisum!> wv))
		 (mktimes2!> (cadddr wd) (zetasum!> wv))))))
    (cond (wd
      (setq wd (chsign!> nil wd))
      (setq wr (cons (cond ((zerop wt) (mktimes2!> wd w0))
			   (t          (fndfpr!> wd w0)))
		     wr))))
    % result ...
    (cond ((zerop wt) (setq wr (evalalg!>(algsum!> wr))))
	  (t          (setq wr (evalform!>(dfsum!>  wr)))))
    (cond ((null wr) (return nil)))
    (return (cons wt wr))) )

(de mkplus2!> (wa wb)
  (cond ((and (null wa) (null wb)) nil)
	((null wa) wb)
	((null wb) wa)
	(t (list 'plus wa wb))))

% Frame and Holonomic indices ...

(de liecoef!> (wa wb wv wi)
  (cond
    ((holpu!> wi)  (evalalg!> (chsign!> nil (ksicoef!> wa wb wv))))
    ((holpd!> wi)  (evalalg!>               (ksicoef!> wb wa wv)))
    ((tetrpu!> wi) (evalalg!> (chsign!> nil (zetacoef!> wa wb wv))))
    ((tetrpd!> wi) (evalalg!>               (zetacoef!> wb wa wv)))  ))

%  KSI^a_b
(de ksicoef!> (wa wb wv)
  (prog2
    (setq wv
      (cond (![umod!] (vform1!> wv (getel1!> ![xf!] wa)))
            (t        (getfdx!> wv wa))))
    (cond ((null wv) wv)
	  (t (list 'df wv (getel1!> ![cord!] wb))))))

%  ZETA'a.b
(de zetacoef!> (wa wb wv)
  (prog2
    (setq wv (dfsum!> (list (dfun!> (vform1!> wv (getframe!> wa)))
			    (vform!> wv (dex!> (getframe!> wa))))))
    (vform1!> (getiframe!> wb) wv)))

% KSI^x_x
(de ksisum!> (wv)
  (prog (w)
    (fordim!> x do
      (setq w (cons (ksicoef!> x x wv) w)))
    (return (evalalg!> (algsum!> w)))))

% ZETA'm.m
(de zetasum!> (wv)
  (prog (w)
    (fordim!> x do
      (setq w (cons (zetacoef!> x x wv) w)))
    (return (evalalg!> (algsum!> w)))))


% Spinorial indices ...

(de liespin!> (wk wx wv wi)
  (prog (w)
    (setq w (spinumb!> wk wx wi))
    (cond ((zerop w) (return nil)))
    (return
      (mktimes2!> w
		  (cond ((dotp!> wi) (zetaspinc!> wx wv))
			(t           (zetaspin!>  wx wv)))))))


(de spinumb!> (wk wx wi)
  (cond
    % upper spinorial ...
    ((upperp!> wi)
       (cond
	 ((eqn wx 0)
	   (cond ((greaterp wk 0) (pm!> wk))
                 (t               0 )))
	 ((eqn wx 1)
	   (pm!>(difference (times 2 wk) (cdr wi))))
	 ((eqn wx 2)
	   (cond ((lessp wk (cdr wi)) (pm!>(difference wk (cdr wi))))
		 (t                   0 )))))
    % lower spinorial ...
    (t (cond
	 ((eqn wx 0)
	   (cond ((lessp wk (cdr wi)) (pm!>(difference wk (cdr wi))))
		 (t                   0 )))
	 ((eqn wx 1)
	   (mp!>(difference (times 2 wk) (cdr wi))))
	 ((eqn wx 2)
	   (cond ((greaterp wk 0) (pm!> wk))
                 (t               0 )))))))

(de sind!> (wk wx wi)
  (cond ((upperp!> wi) (plus wk       (sub1 wx)))
	(t             (plus wk (minus(sub1 wx))))))

% ZETA_AA
(de zetaspin!> (wa wv)
  (cond
    ((eqn wa 0) (mpa!>(zetacoef!> 2 1 wv)))
    ((eqn wa 1) (pma!>(evalalg!>
                    (list 'quotient
		      (list 'plus (zetacoef!> 3 3 wv)
				  (zetacoef!> 1 1 wv)) 2))))
    ((eqn wa 2) (pma!>(zetacoef!> 3 0 wv)))))

% ZETA~_AA
(de zetaspinc!> (wa wv)
  (cond
    ((eqn wa 0) (mpa!>(zetacoef!> 3 1 wv)))
    ((eqn wa 1) (pma!>(evalalg!>
                    (list 'quotient
		      (list 'plus (zetacoef!> 2 2 wv)
				  (zetacoef!> 1 1 wv)) 2))))
    ((eqn wa 2) (pma!>(zetacoef!> 2 0 wv)))))


(de tocalg!> (w)
  (cond ((null w) '(0 . 0))
	(t (cons 0 w))))

(de tonumb!> (w)
  (cond ((null w) 0)
	(t (cdr w))))

(de pm!> (w)
  (cond ((not(pmmm!>)) w)
	(t (minus w ))))

(de mp!> (w)
  (cond ((pmmm!>) w)
	(t (minus w ))))

(de pma!> (w)
  (cond ((not(pmmm!>)) w)
	(t (chsign!> nil w ))))

(de mpa!> (w)
  (cond ((pmmm!>) w)
	(t (chsign!> nil w ))))

(de pmf!> (w)
  (cond ((not(pmmm!>)) w)
	(t (chsign!> t w ))))

(de mpf!> (w)
  (cond ((pmmm!>) w)
	(t (chsign!> t w ))))

%------- Covariant Differential -------------------------------------------

(de dctran!> (lst)
  (prog (wn wi wi1 wl wm wc w wf wh wu wd)
    % wn - int.var. of differentiated object
    % wi - idxl of wn, wl - indices, wm - manipulations
    % wi1 - new idxl after manipulation
    % wc - possible list of alternative connections
    (setq lst (memlist!> '!, lst))
    (cond ((eq lst !!er!!) (err!> 2020)))
    (setq wc (cdr lst))
    (setq lst (car lst))  % lst = (id (...))
     % Internal variable ...
    (cond ((not(idp(car lst))) (err!> 2600))
          (t (setq wn (incomiv!>(explode(car lst))))))
    (cond ((not (or (flagp wn '!+ivar) (flagp wn '!+macros2))) (err!> 2600))
	  ((flagp wn '!+noncov) (err!> 2602))
	  ((eqn (get wn '!=type) -1) (err!> 2004)))
    % Indices ...
    (setq wi (get wn '!=idxl))
    % We need connections ...
    (setq wf '!#!o!m!e!g!a)
    (setq wh '!#!G!A!M!M!A)
    (setq wu '!#!o!m!e!g!a!u)
    (setq wd '!#!o!m!e!g!a!d)
    (cond ((holonomicp!>) (setq wh '!#!o!m!e!g!a)))
    % possible alternative connections ...
    (cond (wc
      (setq wc (mapcar wc 'car))
      (foreach!> wx in wc do (progn
	(cond ((not(idp wx)) (err!> 2603)))
        (setq w (incomiv!>(explode wx)))
	(cond ((flagp w '!+fconn) (setq wf w)
                                  (cond ((holonomicp!>) (setq wh w))))
	      ((flagp w '!+hconn) (setq wh w)
                                  (cond ((holonomicp!>) (setq wf w))))
	      ((flagp w '!+uconn) (setq wu w))
	      ((flagp w '!+dconn) (setq wd w))
	      (t (err!> 2603)))))))
    (setq wc (list wf wh wu wd))
    % indices ...
    (cond
      ((null wi)
	(cond ((not(eqn (length lst) 1)) (err!> 2207)))
	(setq wi nil)
	(go lab))
      ((null(cdr lst)) (err!> 2207))
      ((not(pairp(cadr lst))) (err!> 2102)))
    (setq lst (memlist!> '!, (cadr lst)))
    (cond ((eq lst !!er!!) (err!> 2020))
          ((not(eqn (length lst) (length wi))) (err!> 2207)))
    (setq wm (mapcar lst 'selmani!>)) % manipulations
    (setq lst (mapcar lst 'delmani!>))
    (setq wl (mapcar lst (function unitra0!>)))
    (setq wi1 (chidxl!> wi wm))
    % which of connections we really need ...
    (foreach!> wx in wi1 do
      (cond ((tetrp!> wx)  (require!> (list wf)))
	    ((holp!> wx)   (require!> (list wh)))
	    ((undotp!> wx) (require!> (list wu)))
	    ((dotp!> wx)   (require!> (list wd))) ))
    lab
    (cond ((get wn '!=dens)
      (cond ((cadr(get wn '!=dens)) (require!> (list wh))))
      (cond ((cadddr(get wn '!=dens)) (require!> (list wf))))  ))
    % result ...
    (return (list 'dcexec!> wn wi1 wl wm wc))
    ))

% wn - int. variable, wi - modified idxl
% wl - index list, wm - ind. manipulations
% wo - connections
(de dcexec!> (wn wi wl wm wo)
  (prog (wt wr w0 ww wi1 wl0 wl1 wc wd)
    % evaluating connections ...
    (setq wo (mapcar wo 'eval))
    % evaluating indices ...
    (setq wl (mapcar wl 'unieval!>))
    % type of expression ...
    (setq wt (get wn '!=type))
    % main differential
    (setq ww (cdrnil!>(funapply!> wn wl wm)))
    (setq w0 ww)
    (cond ((eqn wt 0)  (setq wr (ncons(dfun!> ww))))   % d alg
	  (t           (setq wr (ncons(dex!>  ww)))))  % d form
    (setq wl1 wl)
    (setq wi1 wi)
    % for all indices ...
    (while!> wl1
      (cond
	% frame or holonomic ...
	((or (tetrp!>(car wi1)) (holp!>(car wi1)))
	  (fordim!> x do (progn
	    (setq wc (concoef!> (tonumb!>(car wl1)) x (car wi1) wo))
	    (cond (wc
              (setq ww (cdrnil!>(funapply!> wn
                                            (app!> wl0 (cons (tocalg!> x)
                                                             (cdr wl1)))
                                            wm)))))
	    (cond (wc
	      (setq wr (cons (cond ((zerop wt) (fndfpr!>  ww wc))
				   (t          (dfprod2!> wc ww)))
			     wr)))))))
	% spinorial index ...
	((spinp!>(car wi1))
	  (for!> x (0 1 2) do (progn
	    (setq wc (conspin!> (tonumb!>(car wl1)) x (car wi1) wo))
	    (cond (wc
              (setq ww (cdrnil!>(funapply!> wn
                                            (app!> wl0
                                              (cons
                                                (tocalg!>
                                                  (sind!> (tonumb!>(car wl1))
                                                           x (car wi1)))
                                                (cdr wl1)))
                                            wm)))))
	    (cond (wc
	      (setq wr (cons (cond ((zerop wt) (fndfpr!>  ww wc))
				   (t          (dfprod2!> wc ww)))
			     wr)))))))
	(t nil))
      (setq wl0 (cons (car wl1) wl0))
      (setq wl1 (cdr wl1))
      (setq wi1 (cdr wi1)))
    % density ...
    (setq wd (get wn '!=dens))
    (cond (wd
      (setq wd (evalform!> (dfsum2!>
		 (cond ((cadr wd)
                          (fndfpr!>(cadr wd)(hosum!> wo)))(t nil))
		 (cond ((cadddr wd)
                          (fndfpr!>(cadddr wd)(fosum!> wo)))(t nil))
		 )))))
    (cond (wd
      (setq wr (cons (cond ((zerop wt) (fndfpr!> w0 wd))
			   (t          (fndfpr!> wd w0)))
		     wr))))
    % result ...
    (setq wr (evalform!>(dfsum!> wr)))
    (cond ((null wr) (return nil)))
    (return (cons (add1 wt) wr))) )

% Frame of Holonomic ...
(de concoef!> (wa wb wi wo)
  (cond
    ((tetrpu!> wi) (getel2!> (car wo) wa wb))
    ((tetrpd!> wi) (chsignf!>(getel2!>(car wo) wb wa)))
    ((holpu!> wi)  (getel2!> (cadr wo) wa wb))
    ((holpd!> wi)  (chsignf!>(getel2!>(cadr wo) wb wa)))))

% Spinorial ...
(de conspin!> (wk wx wi wo)
  (prog (w)
    (setq w (spinumb!> wk wx wi))
    (cond ((zerop w) (return nil)))
    (return
      (fndfpr!> (chsigna!> w)
                (cond ((dotp!> wi) (getel1!> (cadddr wo) wx))
		      (t           (getel1!>  (caddr wo) wx)))))))

% Summed connection ...
(de fosum!> (wo)
  (prog (w)
    (setq wo (car wo))
    (fordim!> wx do
      (setq w (cons (getel2!> wo wx wx) w)))
    (return(dfsum!> w))))

(de hosum!> (wo)
  (prog (w)
    (setq wo (cadr wo))
    (fordim!> wx do
      (setq w (cons (getel2!> wo wx wx) w)))
    (return(dfsum!> w))))

%------- Covariant Derivative ---------------------------------------------

(de dfctran!> (lst)
  (prog (wv wn wi wi1 wl wm wc w wf wh wu wd wsi wr)
    % wv - vector
    % wn - int.var. of differentiated object
    % wi - idxl of wn, wl - indices, wm - manipulations
    % wi1 - new idxl after manipulation
    % wc - possible list of alternative connections
    (setq lst (memlist!> '!, lst))
    (cond ((eq lst !!er!!) (err!> 2020))
	  ((lessp (length lst) 2) (err!> 2700)))
    (setq wv (unitra0!> (car lst)))  % vector
    (setq lst (cdr lst))
    (setq wc (cdr lst))   % alternative connections
    (setq lst (car lst))  % lst = (id (...))
     % Internal variable ...
    (cond ((not(idp(car lst))) (err!> 2700))
          (t (setq wn (incomiv!>(explode(car lst))))))
    (cond ((not (or (flagp wn '!+ivar) (flagp wn '!+macros2))) (err!> 2700))
	  ((flagp wn '!+noncov) (err!> 2702))
	  ((not(eqn (get wn '!=type) 0)_) (err!> 2704)))
    % Indices ...
    (setq wi (get wn '!=idxl))
    % We need connections ...
    (setq wf '!#!o!m!e!g!a)
    (setq wh '!#!G!A!M!M!A)
    (setq wu '!#!o!m!e!g!a!u)
    (setq wd '!#!o!m!e!g!a!d)
    (cond ((holonomicp!>) (setq wh '!#!o!m!e!g!a)))
    % possible alternative connections ...
    (cond (wc
      (setq wc (mapcar wc 'car))
      (foreach!> wx in wc do (progn
	(cond ((not(idp wx)) (err!> 2703)))
        (setq w (incomiv!>(explode wx)))
	(cond ((flagp w '!+fconn) (setq wf w)
                                  (cond ((holonomicp!>) (setq wh w))))
	      ((flagp w '!+hconn) (setq wh w)
                                  (cond ((holonomicp!>) (setq wf w))))
	      ((flagp w '!+uconn) (setq wu w))
	      ((flagp w '!+dconn) (setq wd w))
	      (t (err!> 2703)))))))
    (setq wc (list wf wh wu wd))
    % indices ...
    (cond
      ((null wi)
	(cond ((not(eqn (length lst) 1)) (err!> 2207)))
	(setq wi nil)
	(go lab))
      ((null(cdr lst)) (err!> 2207))
      ((not(pairp(cadr lst))) (err!> 2102)))
    (setq lst (memlist!> '!, (cadr lst)))
    (cond ((eq lst !!er!!) (err!> 2020))
          ((not(eqn (length lst) (length wi))) (err!> 2207)))
    (setq wm (mapcar lst 'selmani!>)) % manipulations
    (setq lst (mapcar lst 'delmani!>))
    (setq wl (mapcar lst (function unitra0!>)))
    (setq wi1 (chidxl!> wi wm))
    % which of connections we really need ...
    (foreach!> wx in wi1 do
      (cond ((tetrp!> wx)  (require!> (list wf)))
	    ((holp!> wx)   (require!> (list wh)))
	    ((undotp!> wx) (require!> (list wu)))
	    ((dotp!> wx)   (require!> (list wd))) ))
    lab
    (cond ((get wn '!=dens)
      (cond ((cadr(get wn '!=dens)) (require!> (list wh))))
      (cond ((cadddr(get wn '!=dens)) (require!> (list wf))))  ))
    % einstein summation ...
    (setq wsi (intersecl!> (freevar!> wv ![extvar!])
                           (freevar!> wl ![extvar!])))
    % result ...
    (setq wr (list 'dfcexec!> wn wi1 wl wm wc wv))
    (cond (wsi (setq wr (mkeinsum0!> wsi wr))))
    (return wr)
    ))

% wn - int. variable, wi - modified idxl
% wl - index list, wm - ind. manipulations
% wo - connections, wv - vector
(de dfcexec!> (wn wi wl wm wo wv)
  (prog (wr w0 ww wi1 wl0 wl1 wc wd)
    % evaluating vector ...
    (setq wv (unieval!> wv))
    (cond ((null wv) (return nil))
	  ((not(eqn (car wv) -1)) (err!> 2701)))
    (setq wv (cdr wv))
    % evaluating connections ...
    (setq wo (mapcar wo 'eval))
    % evaluating indices ...
    (setq wl (mapcar wl 'unieval!>))
    % main differential
    (setq ww (cdrnil!>(funapply!> wn wl wm)))
    (setq w0 ww)
    (setq wr (ncons(dfun!> ww)))   % d alg
    (setq wl1 wl)
    (setq wi1 wi)
    % for all indices ...
    (while!> wl1
      (cond
	% frame or holonomic ...
	((or (tetrp!>(car wi1)) (holp!>(car wi1)))
	  (fordim!> x do (progn
	    (setq wc (concoef!> (tonumb!>(car wl1)) x (car wi1) wo))
	    (cond (wc
              (setq ww (cdrnil!>(funapply!> wn
                                            (app!> wl0 (cons (tocalg!> x)
                                                             (cdr wl1)))
                                            wm)))))
	    (cond (wc
	      (setq wr (cons (fndfpr!>  ww wc)
			     wr)))))))
	% spinorial index ...
	((spinp!>(car wi1))
	  (for!> x (0 1 2) do (progn
	    (setq wc (conspin!> (tonumb!>(car wl1)) x (car wi1) wo))
	    (cond (wc
              (setq ww (cdrnil!>(funapply!> wn
                                            (app!> wl0
                                              (cons
                                                (tocalg!>
                                                  (sind!> (tonumb!>(car wl1))
                                                           x (car wi1)))
                                                (cdr wl1)))
                                            wm)))))
	    (cond (wc
	      (setq wr (cons (fndfpr!>  ww wc)
			     wr)))))))
	(t nil))
      (setq wl0 (cons (car wl1) wl0))
      (setq wl1 (cdr wl1))
      (setq wi1 (cdr wi1)))
    % density ...
    (setq wd (get wn '!=dens))
    (cond (wd
      (setq wd (evalform!> (dfsum2!>
		 (cond ((cadr wd)
                          (fndfpr!>(cadr wd)(hosum!> wo)))(t nil))
		 (cond ((cadddr wd)
                          (fndfpr!>(cadddr wd)(fosum!> wo)))(t nil))
		 )))))
    (cond (wd
      (setq wr (cons (fndfpr!> w0 wd)
		     wr))))
    % result ...
    (setq wr (evalalg!>(vform1!> wv (dfsum!> wr))))
    (cond ((null wr) (return nil)))
    (return (cons 0 wr))) )


%======= End of GRGcoper.sl ===============================================%

