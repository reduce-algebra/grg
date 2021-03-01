%==========================================================================%
%   GRGdecl.sl                      Internal Variables, Flags, Properties  %
%==========================================================================%
%   GRG 3.2 Standard Lisp Source Code     (C) 1988-2000 Vadim V. Zhytnikov %
%==========================================================================%
% This file is distributed without any warranty. You may modify it but you %
% are not allowed to remove author's name and/or distribute modified file. %
%==========================================================================%
%
%  Notation for GRG symbols :
%
%    !!sym!!    -  Self-Quoted Symbols
%    ![sys!]    -  Internal GRG Control or Working System Variables
%    !#id       -  Internal Variables of Built-in Objects
%    !+flag     -  GRG Specific Flags
%    !=prop     -  GRG Specific Properties
%    !*switch   -  GRG and REDUCE Switches
%    funtion!>  -  GRG functions
%


%========== (1) Internal GRG Control Variables ==========================

(put 'grg 'stat 'endstat)  % Making  grg;  REDUCE command ...

%---------- GRG System Variables   --------------------------------------

(global '(

     ![version!]  % Version number

% Start mode :
     ![autostart!] % Run (grg) atomatically during load grg; or not

% General Status :
     ![dim!]      % Current Dimension 4
     ![dim1!]     % dim-1
     ![sgn!]      % Current Signature (-1 1 1 1)
     ![sigprod!]  %  prod(-1 1 1 1)
     ![dim0!]     % Initial Dimension and
     ![sgn0!]     % Signature in the session
     ![umod!]     % Current basis mode

% Metric and Frame Type :
     ![mtype!]    % Metric type:   nil - unknown
     ![mitype!]   %   1 - null  2 - diagonal 3 - general
     ![dtype!]    % Metric differentiability:  nil - unknown
     ![ditype!]   %   1 - constant  2 - general
     ![ftype!]    % Frame type:  nil - unknown
     ![fitype!]   %   1 - holonomic  2 - diagonal  3 - general
     ![nullm!]    % Standard Null Metric for -,+,+,+
     ![nullm1!]   % Standard Null Metric for +,-,-,-

% Others working variables :
     ![w!]        % General purpose
     ![instr!]    % All Commands list
     ![datl!]     % All Objects
     ![abbr!]     % All User-Defined Objects (Abbreviations)
     ![rconstl!]  % List of reserved constants
     ![sublist!]  % Substitutions List
     ![rpfl!]     % Flags and properties which must be cleared
     ![rpflcr!]   %   for Coordinates
     ![rpflcn!]   %   for Constants
     ![rpflap!]   %   for Affine Parameter
     ![rpflfu!]   %   for Functions
     ![tlst!]     % List of Energy-Momentum tensors
     ![slst!]     % List of spin forms
     ![solveq!]   % Equations for solve
     ![allprops!] % All Flags and Props
     ![allflags!] %   important for Load/Unload
     ![icompos!]  % List of Commands allowed in composites
     ![newabbr!]  % New object in assignment
     ![wi!] ![wh!] ![wf!] ![ws!]
     ![gfun!]     % Generic functions list

% Session Control :
     ![er!]       % Error type
     ![firsti!]   % First instruction indicator for Dimension
     ![time!]     % Timer
     ![gctime!]   % GC Timer
     ![ttime!]    % Total Session Time
     ![tgctime!]  % Total GC Time
     ![pause!]    % Pause regim indicator
     ![origlower!]

% Switches control :
     ![flaghis!]  % Flags On/Off history list
     ![flagl!]    % GRG Flags list
     ![flaglo!]   % GRG Output-Flags list
     ![iflago!]   % Initial mode of output
     ![echo!]     % Echo in LISTOK>
     ![flagnil!]  % Swithes initailly to nil
     ![flagt!]    % Swithes initially t
     ![fldtuned!] % nil tuning of FANCY-LOWER-DIGITS is needded

% OS scpecific :
     ![dirsep!]   % The directories separator. This symbol is
     	          % added to the end of GRG environ. var. when
     	          % trying to open files.
                  % \ for DOS, / for UNIX, : for VMS (?)
                  % if nil then nothing added.
     ![syscall!]  % Temporary exit to OS and OS commands
          	  %  1 - via SYSTEM (UNIX,DOS)
        	  %  2 - via QUIT (VAX/VMS)
        	  %  nil - forbidden
     ![grgdir!]   % Standard Input Didrectory Expanded
     ![grgdir1!]  % Standard Input Didrectory

% Version specific:
     ![lower!]    % If t then background lisp internally is in lower case

% Debugging :
     ![erst1!]    % First ERRORSET debuggin parameter
     ![erst2!]    % Second ERRORSET debuggin parameter

% GRG printing:
     ![line!]     % Current Line for GPRIN
     ![lline!]    % Current Line Length
     ![gptab!]    % Tabulation for GPRIN
     ![gpfirst!]  % First Line marker for GPRIN
     ![modp!]     % Basis mode for write
     ![allzero!]  % Zero-Nonzero components indicator for write
     ![idwri!]    % Writed Data Identifier

% Files manipulation :
     ![fromf!]    % In file
     ![loa!]      % Load file
     ![unl!]      % Global Unload file
     ![lunl!]     % Local Unload file
     ![wri!]      % Global Write file
     ![lwri!]     % Local Write file

% Data evaluation control :
     ![chain!]    % Chain of required data in REQUEST
     ![way!]      % Way for Find/Calculate

% Translation control:
     ![cs!]       % Chanhe Sign
     ![ch!]       % Change Conjugation
     ![lsrs!]     % Left or Right side in equation
     ![extvar!]   % External variables list
     ![extvara!]  % Additional external variables list
     ![idl!]      % For T(J) = expr(J)
     ![texpr!]    %     translation
     ![ivs!]      % Iteration vars stack

% Coordinates transformation:
     ![ocord!]    % Old coordinates list
     ![x!]        %    X
     ![dfx!]      %   d X
     ![dex!]      %   @ X

% Basis mode:
     ![xb!]       % d X/\d Y/\...
     ![xf!]       % d X = b
     ![xv!]       % @ X = e
     ![ccb!]      % ~ b
     ![ccbi!]     % ~ e
     ![dbas!]     % d(b/\...) accumulation

% Rotations:
     ![l!]        %     L         - frame rotaion matrix
     ![dl!]       %   det(L)      -   its det
     ![sdl!]      %  sgn(det(L))  -   the sign of its det
     ![li!]       %    L^(-1)     -   its inverse
     ![ls!]       %     LS        - spinor rotation matrix
     ![dens!]     % density factor for an object

% Processor internals:
     ![tlow!]     % T_a (lower index a) for Duialisation
))

(setq ![autostart!] t) % By default we start (grg) during load grg;


%-------  Self Quoted Atoms  -------------------------------------------

(global '( !!stop!! !!next!! !!er!! ))

(setq !!stop!! '!!stop!!)  %   This is STOP
(setq !!er!!   '!!er!!)    %   This is ERROR
(setq !!next!! '!!next!!)  %   This is NEXT

%-----------------------------------------------------------------------



%========== (2) Built-In Objects =======================================

%---- Flags and Properties for Internal Data variables #ID -------------
%
%  Prop =type     - Type of Component:
%                    -1 - vector; 0 - algebraic expression, n - n-form.
%
%  Prop =idxl     - List of Indices. Absent for Scalars. In The List:
%                   nil - lower frame,     t - upper frame,
%                   0   - lower holonomic, 1 - upper holonomic,
%                   (u . n) - un. spinor,  (d . n) - do. spinor,
%                   (uu . n) - up un. spinor,  (ud . n) - up do. spinor,
%                   (n . n) - enimerating, (n) - enum. d-dimensional.
%
%  Prop =sidxl    - Symmetries List is  (sy1 sy2 ...)
%                     sy = (type el1 el2 ...)
%                   with type = a | s | h | c which stands for
%                   Antisymmetric, Symmetric, Hermitian, Cyclic
%                     el = n | (n1 n2 ...) | sy
%                   where n is the index number and sy as above.
%
%  Prop =way      - Ways of Calculation is  (el1 el2 ...)
%                     el = ( (name) (cond) (evfun) data ... )
%                     data = id | (cond id1 id2  ...) | (t id)
%                   the second form is included iff cond=true
%                   the third form defines Main data.
%
%  Prop =constr   - Restriction when data can be used is
%                     (fn1 fn2 ...)
%                   where fn is function call.
%
%  Prop =dens     - Pseudo-tensor and Density properties
%                   List of four elements (a b c d)
%                   a=t/nil - Pseudo for coodrinate transform   sgnD
%                   b=n     - Density for coordinate transform  D^n
%                   c=t/nil - Pseudo for rotations              sgnL
%                   d=n     - Density for rotations             L^n
%
%  Flag +noncov   - Marks Noncovariant data types for
%                   preventing Dc and Lie calculation.
%                   But don't prevent rotations.
%
%  Flags  +fconn  +hconn  +uconn  +dconn
%                   types of connection are
%                   Frame, Holonomic, Spinorial, Conjugate Spinorial
%
%  Flag +hold     - Prevents rotation or coordinate
%                   transformation of the object.
%
%  Flag +pl       - Marks oblects with plural name.
%
%  Flag +equ      - Marks equations.
%
%  Flag +ivar     - Marks all internal variables.
%
%  Flag +abbr     - Marks new user created objects (abbreviations).
%
%  Prop =unl      - Special function call for Unload.
%
%  Prop =datl     - Special function call for Write.
%
%  Prop =tex      - Writre in FANCY/TEX output mode.
%                   If ID than ID both in TEX and FANCY mode
%                   If (IT . IF) IT for TEX IF for FANCY

%-----  Flags and Prop. for Funs and Vars (Cord, Const, Fun)  --------
%
%  Flag +grg      - Already used by GRG (Can't be declared once again).
%
%  Prop =depend   - Dependence List for Functions.
%
%  Flag +grgvar   - Marks Variables: Cord, Const, Implicit Fun.
%                   So, can be used as var in any expression.
%
%  Flag +fun      - Marks Functions.
%
%  Prop =cord     - Coordinate number N (0 1 ... dim-1).
%
%  Prop =conj     - Complex Conjugated Object.
%
%  Prop =subind   - Value of Iteration Variable.
%
%  Flag +redbad   - Specially blocks some atoms.
%
%  Reduce Flags:  used!* constant
%
%  Reduce Flags:  subfn symmetric  antisymmetric  odd  even
%
%  Reduce Props:  simpfn  kvalue  klist  narg
%
%-----------------------------------------------------------------------

%-------   Data List   -------------------------------------------------

(setq ![datl!] '(
% Coordinates, Constants, Functions, Solutions ...
    ((Coordinates)       ![cord!]  )
    ((Functions)         ![fun!]   )
    ((Constants)         ![const!] )
    ((Affine Parameter)  ![apar!]  )
    ((Solutions)         ![sol!]   )
% Metric, Frame, Basis, Volume ...
    ((Frame)                    !#!T   )
    ((Vector Frame)             !#!D   )
    ((Metric)                   !#!G   )
    ((Inverse Metric)           !#!G!I )
    ((Det of Metric)            !#!d!e!t!G   )
    ((Det of Holonomic Metric)  !#!d!e!t!g   )
    ((Sqrt Det of Metric)       !#!s!d!e!t!G )
    ((Volume)                   !#!V!O!L )
    ((Basis)                    !#!b   )
    ((Vector Basis)             !#!e   )
    ((S - forms)                !#!S   )
% Rotation Matrices ...
    ((Frame Transformation)     !#!L   )
    ((Spinorial Transformation)    !#!L!S )
% Connection and related objects ...
    ((Frame Connection)      !#!o!m!e!g!a   )
    ((Holonomic Connection)  !#!G!A!M!M!A   )
    ((Undotted Connection)   !#!o!m!e!g!a!u )
    ((Dotted Connection)     !#!o!m!e!g!a!d )
    ((Spinorial Connection)  ( !#!o!m!e!g!a!u  !#!o!m!e!g!a!d ))
    ((Riemann Frame Connection)      !#!r!o!m!e!g!a   )
    ((Riemann Holonomic Connection)  !#!R!G!A!M!M!A   )
    ((Riemann Undotted Connection)   !#!r!o!m!e!g!a!u )
    ((Riemann Dotted Connection)     !#!r!o!m!e!g!a!d )
    ((Riemann Spinorial Connection)  ( !#!r!o!m!e!g!a!u  !#!r!o!m!e!g!a!d ))
    ((Connection Defect)     !#!K    )
    ((Undotted S - forms)    !#!S!U  )
    ((Dotted S - forms)      !#!S!D  )
    ((Spinorial S - forms)   ( !#!S!U !#!S!D ))
% Torsion ...
    ((Torsion)     !#!T!H!E!T!A )
    ((Contorsion)  !#!K!Q       )
    ((Torsion Trace 1 - form)         !#!Q!Q   )
    ((Antisymmetric Torsion 3 - form) !#!Q!Q!A )
    ((Undotted Contorsion)    !#!K!U )
    ((Dotted Contorsion)      !#!K!D )
    ((Spinorial Contorsion) ( !#!K!U !#!K!D ))
    ((Torsion Trace)                  !#!Q!T )
    ((Torsion Pseudo Trace)           !#!Q!P )
    ((Traceless Torsion Spinor)       !#!Q!C )
    ((Torsion Spinors)      ( !#!Q!C !#!Q!T !#!Q!P ))
    ((Torsion Components)   ( !#!Q!C !#!Q!T !#!Q!P ))
    ((Traceless Torsion 2 - form)      !#!T!H!Q!C )
    ((Torsion Trace 2 - form)          !#!T!H!Q!T )
    ((Antisymmetric Torsion 2 - form)  !#!T!H!Q!A )
    ((Torsion 2 - forms) ( ((geq ![dim!] 3) !#!T!H!Q!C)
                                            !#!T!H!Q!T
                           ((geq ![dim!] 3) !#!T!H!Q!A) ))
    ((Undotted Torsion Trace 2 - form)         !#!T!H!Q!T!U )
    ((Undotted Antisymmetric Torsion 2 - form) !#!T!H!Q!A!U )
    ((Undotted Traceless Torsion 2 - form)     !#!T!H!Q!C!U )
    ((Undotted Torsion 2 - forms)  ( !#!T!H!Q!C!U !#!T!H!Q!T!U !#!T!H!Q!A!U ))
% Nonmetricity ...
    ((Nonmetricity)          !#!N     )
    ((Nonmetricity Defect )  !#!K!N   )
    ((Weyl Vector)           !#!N!N!W )
    ((Nonmetricity Trace)    !#!N!N!T )
    ((Symmetric Nonmetricity 1 - form)      !#!N!C )
    ((Antisymmetric Nonmetricity 1 - form)  !#!N!A )
    ((Nonmetricity Trace  1 - form)         !#!N!T )
    ((Weyl Nonmetricity 1 - form)           !#!N!W )
    ((Nonmetricity 1 - forms) ( !#!N!C
                                ((geq ![dim!] 3) !#!N!A)
                                !#!N!T
                                !#!N!W  ))
% Curvature ...
    ((Curvature)             !#!O!M!E!G!A    )
    ((Undotted Curvature)    !#!O!M!E!G!A!U  )
    ((Dotted Curvature)      !#!O!M!E!G!A!D  )
    ((Spinorial Curvature) ( !#!O!M!E!G!A!U !#!O!M!E!G!A!D ))
    ((Riemann Tensor)        !#!R!I!M )
    ((Ricci Tensor)          !#!R!I!C )
    ((A - Ricci Tensor)      !#!R!I!C!A )
    ((S - Ricci Tensor)      !#!R!I!C!S )
    ((Homothetic Curvature)  !#!O!M!E!G!A!H )
    ((Scalar Curvature)      !#!R!R   )
    ((Einstein Tensor)       !#!G!T   )
    ((Weyl Spinor)                !#!R!W)
    ((Traceless Ricci Spinor)     !#!R!C)
    ((Ricanti Spinor)             !#!R!A)
    ((Traceless Deviation Spinor) !#!R!B)
    ((Scalar Deviation)           !#!R!D)
    ((Curvature Spinors)    (          !#!R!W !#!R!C !#!R!R
                            (!*torsion !#!R!B !#!R!A !#!R!D ) ))
    ((Curvature Components) (          !#!R!W !#!R!C !#!R!R
                            (!*torsion !#!R!B !#!R!A !#!R!D ) ))
    ((Undotted Weyl 2 - form)                !#!O!M!W!U )
    ((Undotted Traceless Ricci 2 - form)     !#!O!M!C!U )
    ((Undotted Scalar Curvature 2 - form)    !#!O!M!R!U )
    ((Undotted Ricanti 2 - form)             !#!O!M!A!U )
    ((Undotted Traceless Deviation 2 - form) !#!O!M!B!U )
    ((Undotted Scalar Deviation 2 - form)    !#!O!M!D!U )
    ((Undotted Curvature 2 - forms)
      ( !#!O!M!W!U !#!O!M!C!U !#!O!M!R!U (!*torsion !#!O!M!A!U !#!O!M!B!U !#!O!M!D!U )))
    ((Weyl 2 - form)                        !#!O!M!W )
    ((Traceless Ricci 2 - form)             !#!O!M!C )
    ((Scalar Curvature 2 - form)            !#!O!M!R )
    ((Ricanti 2 - form)                     !#!O!M!A )
    ((Traceless Deviation 2 - form)         !#!O!M!B )
    ((Antisymmetric Curvature 2 - form)     !#!O!M!D )
    ((Homothetic Curvature 2 - form)        !#!O!S!H )
    ((Antisymmetric S - Ricci 2 - form)     !#!O!S!A )
    ((Traceless S - Ricci 2 - form)         !#!O!S!C )
    ((Antisymmetric S - Curvature 2 - form) !#!O!S!V )
    ((Symmetric S - Curvature 2 - form)     !#!O!S!U )
    ((Curvature 2 - forms) (
        ((geq ![dim!] 4) !#!O!M!W )
	((geq ![dim!] 3) !#!O!M!C )
	  	         !#!O!M!R
	((and (or !*torsion !*nonmetr) (geq ![dim!] 3)) !#!O!M!A )
	((and (or !*torsion !*nonmetr) (geq ![dim!] 4)) !#!O!M!B )
	((and (or !*torsion !*nonmetr) (geq ![dim!] 4)) !#!O!M!D )
        (!*nonmetr                       !#!O!S!H )
        ((and !*nonmetr (geq ![dim!] 3)) !#!O!S!A )
        (!*nonmetr                       !#!O!S!C )
        ((and !*nonmetr (geq ![dim!] 4)) !#!O!S!V )
        ((and !*nonmetr (geq ![dim!] 3)) !#!O!S!U )
	))
% Various constants ...
    ((A - Constants)   !#!A!C!O!N!S!T )
    ((L - Constants)   !#!L!C!O!N!S!T )
    ((M - Constants)   !#!M!C!O!N!S!T )
% Scalar field ...
    ((Scalar Equation)        !#!S!C!q   )
    ((Scalar Field)           !#!F!I     )
    ((Scalar Action)          !#!S!A!C!T )
    ((Minimal Scalar Action)  !#!S!A!C!T!M!I!N )
    ((Minimal Scalar Energy - Momentum Tensor) !#!T!S!C!L!M!I!N )
% EM field ...
    % for all dim ...
    ((EM Potential)       !#!A )
    ((Current 1 - form)     !#!J )
    ((EM Action)          !#!E!M!A!C!T )
    ((EM 2 - form)        !#!F!F       )
    ((EM Tensor)          !#!F!T       )
    ((First Maxwell Equation)       !#!M!W!F!q   )
    ((Second Maxwell Equation)      !#!M!W!S!q   )
    ((Maxwell Equations)          ( !#!M!W!F!q !#!M!W!S!q ))
    ((Continuity Equation)          !#!C!O!q     )
    ((EM Energy - Momentum Tensor)  !#!T!E!M     )
    % dim=4 only ...
    ((First EM Scalar)              !#!S!C!F     )
    ((Second EM Scalar)             !#!S!C!S     )
    ((EM Scalars)                 ( !#!S!C!F !#!S!C!S ))
    ((Selfduality Equation)         !#!S!D!q     )
    ((Complex EM 2 - form)          !#!F!F!U     )
    ((Complex Maxwell Equation)     !#!M!W!U!q   )
    ((Undotted EM Spinor)           !#!F!I!U     )
    ((Complex EM Scalar)            !#!S!C!U     )
    ((EM Energy - Momentum Spinor)  !#!T!E!M!S   )
% YM field ...
    ((YM Potential)         !#!A!Y!M       )
    ((Structural Constants) !#!S!C!O!N!S!T )
    ((YM Action)            !#!Y!M!A!C!T   )
    ((YM 2 - form)          !#!F!F!Y!M     )
    ((YM Tensor)            !#!F!T!Y!M     )
    ((First YM Equation)    !#!Y!M!F!q     )
    ((Second YM Equation)   !#!Y!M!S!q     )
    ((YM Equations)       ( !#!Y!M!F!q !#!Y!M!S!q ))
    ((YM Energy - Momentum Tensor)  !#!T!Y!M )
% Dirac field ...
    ((Phi Spinor)     !#!P!H!I )
    ((Chi Spinor)     !#!C!H!I )
    ((Dirac Spinor) ( !#!P!H!I !#!C!H!I ))
    ((Dirac Action 4 - form)  !#!D!A!C!T )
    ((Undotted Dirac Spin 3 - Form) !#!S!P!D!I!U )
    ((Dirac Energy - Momentum Tensor) !#!T!D!I )
    ((Phi Dirac Equation)   !#!D!P!q )
    ((Chi Dirac Equation)   !#!D!C!q )
    ((Dirac Equation)     ( !#!D!P!q !#!D!C!q ))
% Geodesics and congruences ...
    ((Geodesic Equation)   !#!G!E!O!q )
% Null congruence ...
    ((Congruence)                     !#!K!V         )
    ((Null Congruence Condition)      !#!N!C!o       )
    ((Geodesics Congruence Condition) !#!G!C!o       )
    ((Congruence Expansion)           !#!t!h!e!t!a!O     )
    ((Congruence Squared Rotation)    !#!o!m!e!g!a!S!Q!O )
    ((Congruence Squared Shear)       !#!s!i!g!m!a!S!Q!O )
    ((Optical Scalars)
      (!#!t!h!e!t!a!O !#!o!m!e!g!a!S!Q!O !#!s!i!g!m!a!S!Q!O ))
% Kinematics ...
    ((Velocity Vector)    !#!U!V         )
    ((Velocity)           !#!U!U         )
    ((Velocity Square)    !#!U!S!Q       )
    ((Projector)          !#!P!R         )
    ((Acceleration)       !#!a!c!c!U     )
    ((Vorticity)          !#!o!m!e!g!a!U )
    ((Volume Expansion)   !#!t!h!e!t!a!U )
    ((Shear)              !#!s!i!g!m!a!U )
    ((Kinematics)
      ( !#!a!c!c!U !#!o!m!e!g!a!U !#!t!h!e!t!a!U  !#!s!i!g!m!a!U ))
% Ideal Fluid ...
    ((Pressure)                              !#!P!R!E!S )
    ((Energy Density)                        !#!E!N!E!R )
    ((Ideal Fluid Energy - Momentum Tensor)  !#!T!I!F!L )
% Spin Fluid ...
    ((Spin Fluid Energy - Momentum Tensor)  !#!T!S!F!L    )
    ((Spin Density)                         !#!S!P!F!L!T  )
    ((Spin Density 2 - form)                !#!S!P!F!L    )
    ((Undotted Fluid Spin 3 - form)         !#!S!P!F!L!U  )
    ((Frenkel Condition)                    !#!F!C!o      )
% Total Energy-Momentum and Spin ...
    ((Total Energy - Momentum Tensor)  !#!T!E!N!M!O!M   )
    ((Total Energy - Momentum Spinor)  !#!T!E!N!M!O!M!S )
    ((Total Energy - Momentum Trace)   !#!T!E!N!M!O!M!T )
    ((Total Undotted Spin 3 - form)    !#!S!P!I!N!U     )
% Einstein Equations ...
    ((Einstein Equation)            !#!E!E!q   )
    ((Traceless Einstein Equation)  !#!C!E!E!q )
    ((Trace of Einstein Equation)   !#!T!E!E!q )
    ((Spinor Einstein Equations)  ( !#!C!E!E!q !#!T!E!E!q ))
% Gravitational Equations ...
    ((Action)                      !#!L!A!C!T       )
    ((Undotted Curvature Momentum) !#!P!O!M!E!G!A!U )
    ((Torsion Momentum)            !#!P!T!H!E!T!A   )
    ((Metric Equation)             !#!M!E!T!R!q     )
    ((Torsion Equation)            !#!T!O!R!S!q     )
    ((Gravitational Equations) (          !#!M!E!T!R!q
                               (!*torsion !#!T!O!R!S!q )))
))

(prog ( ![idatl!] )
  (foreach!> ![www!] in ![datl!] do
     (cond ((atom (cadr ![www!]))
       (setq ![idatl!] (cons (cadr ![www!]) ![idatl!] )))))
  (global ![idatl!])
  (flag ![idatl!] '!+ivar))


%-------   Plural   ----------------------------------------------------

(flag '(
  ![cord!] ![const!] ![fun!]
  !#!T !#!b !#!S !#!S!U !#!S!D
  !#!A!C!O!N!S!T !#!M!C!O!N!S!T !#!L!C!O!N!S!T
) '!+pl)


%-------- Equations ----------------------------------------------------

(flag '(
  ![sol!]
  !#!S!C!q
  !#!D!P!q !#!D!C!q
  !#!Y!M!F!q !#!Y!M!S!q
  !#!M!W!F!q !#!M!W!S!q !#!C!O!q !#!S!D!q !#!M!W!U!q
  !#!G!E!O!q
  !#!N!C!o !#!G!C!o !#!F!C!o
  !#!E!E!q !#!T!E!E!q !#!C!E!E!q
  !#!M!E!T!R!q !#!T!O!R!S!q
) '!+equ)

%-------- Total Enargy-Momentum and Spin -------------------------------

(setq ![tlst!] '( !#!T!D!I !#!T!E!M !#!T!Y!M !#!T!S!C!L!M!I!N
                   !#!T!I!F!L !#!T!S!F!L ))
(setq ![slst!] '( !#!S!P!D!I!U !#!S!P!F!L!U ))

%-------- Properties of the Built-In Objects ---------------------------

(put '![sol!] '!=type 0)

% word!!! in =way

% Metric, Farame, Volume ...

(put '!#!T '!=type 1)
(put '!#!T '!=idxl '(t))
(put '!#!T '!=way '( ((By Default) nil (frame0!>)          )
		     ((From Vector Frame) nil (frame1!>) (t !#!D) )  ))
(put '!#!T '!=tex "\theta")

(put '!#!D '!=type -1)
(put '!#!D '!=idxl '(nil))
(put '!#!D '!=way '( ((From Frame)
                         nil (iframe1!>) !#!V!O!L !#!T )  ))
(put '!#!D '!=tex '("\partial" . 182))

(put '!#!G '!=type 0)
(put '!#!G '!=idxl  '(nil nil))
(put '!#!G '!=sidxl '((s 1 2)))
(put '!#!G '!=way '( ((By Default)          nil (metr0!>)            )
                     ((From Inverse Metric) nil (metr1!>) (t !#!G!I) )   ))
(put '!#!G '!=tex '!g)

(put '!#!G!I '!=type 0)
(put '!#!G!I '!=idxl '(t t))
(put '!#!G!I '!=sidxl '((s 1 2)))
(put '!#!G!I '!=way '( ((From Metric) nil (imetr1!>) !#!G ) ))
(put '!#!G!I '!=tex '!g)

(put '!#!d!e!t!G '!=type 0)
(put '!#!d!e!t!G '!=way '( (nil nil (detg1!>) !#!G ) ))
(put '!#!d!e!t!G '!=dens '(nil nil nil -2))
(put '!#!d!e!t!G '!=tex '!g)

(put '!#!d!e!t!g '!=type 0)
(put '!#!d!e!t!g '!=way '( (nil nil (dethg1!>) !#!G !#!T ) ))
(put '!#!d!e!t!g '!=dens '(nil -2 nil nil))
(put '!#!d!e!t!g '!=tex '!g)

(put '!#!s!d!e!t!G '!=type 0)
(put '!#!s!d!e!t!G '!=way '((nil nil (sdetg1!>) !#!G ) ))
(put '!#!s!d!e!t!G '!=dens '(nil nil t -1))
(put '!#!s!d!e!t!G '!=tex "\sqrt{-g}")

(put '!#!V!O!L '!=type '![dim!]) % Variable Type !!!
(put '!#!V!O!L '!=way '((nil nil (vol0!>) !#!s!d!e!t!G !#!T ) ))
(put '!#!V!O!L '!=dens '(t nil t nil))
(put '!#!V!O!L '!=tex "\upsilon")

(put '!#!b '!=type 1)
(put '!#!b '!=idxl '((n)))
(put '!#!b '!=way '(((From Frame) nil (base!>)  !#!V!O!L !#!T )
                    ((From Vector Basis) nil (base1!>) (t !#!e)  )  ))
(put '!#!e '!=type -1)
(put '!#!e '!=idxl '((n)))
(put '!#!e '!=way '(((From Basis) nil (ibase!>) !#!b ) ))

(put '!#!S '!=type 2)
(put '!#!S '!=idxl '(t t))
(put '!#!S '!=sidxl '((a 1 2)))
(put '!#!S '!=way '((nil nil (makesforms!>) !#!T)))

% Rotation matrices ...

(put '!#!L '!=type 0)
(put '!#!L '!=idxl '(t nil))
(put '!#!L '!=tex '!L)

(put '!#!L!S '!=type 0)
(put '!#!L!S '!=idxl '((u . 1) (uu . 1)))
(put '!#!L!S '!=tex  '("\Lambda" . 76))

% Spinorial S-forms ...

(put '!#!S!U '!=type 2)
(put '!#!S!U '!=idxl '((u . 2)))
(put '!#!S!U '!=way '( (nil nil (ssform!> '!#!S!U 2 3) !#!T ) ))
(put '!#!S!U '!=constr '((sp!>)))
(put '!#!S!D '!=type 2)
(put '!#!S!D '!=idxl '((d . 2)))
(put '!#!S!D '!=way '( (nil nil (ssform!> '!#!S!D 3 2) !#!T ) ))
(put '!#!S!D '!=constr '((sp!>)))

% Connection and related objects ...

(flag '( !#!G!A!M!M!A !#!o!m!e!g!a !#!o!m!e!g!a!u !#!o!m!e!g!a!d )
      '!+noncov)

(flag '( !#!R!G!A!M!M!A !#!r!o!m!e!g!a !#!r!o!m!e!g!a!u !#!r!o!m!e!g!a!d )
      '!+noncov)


(put  '!#!G!A!M!M!A   '!=type 1)
(put  '!#!G!A!M!M!A   '!=idxl '(1 0))
(put  '!#!G!A!M!M!A   '!=way '(
   ((From Frame Connection) nil
      (gfromo!>) !#!T !#!D !#!o!m!e!g!a )
   ))
(flag '(!#!G!A!M!M!A) '!+hconn)
(put  '!#!G!A!M!M!A   '!=tex  '("\Gamma" . 71))

(put  '!#!R!G!A!M!M!A   '!=type 1)
(put  '!#!R!G!A!M!M!A   '!=idxl '(1 0))
(put  '!#!R!G!A!M!M!A   '!=way '(
   ((From Riemann Frame Connection) nil
      (rgfromro!>) !#!T !#!D !#!r!o!m!e!g!a )
   ))
(flag '(!#!R!G!A!M!M!A) '!+hconn)

(put  '!#!o!m!e!g!a   '!=type 1)
(put  '!#!o!m!e!g!a   '!=idxl '(t nil))
(put  '!#!o!m!e!g!a   '!=way '(
   (nil nil (connec!>) !#!T !#!D !#!G !#!G!I
                       (!*torsion !#!T!H!E!T!A)
		       (!*nonmetr !#!N))
   ((From Spinorial Connection) (sp!-n!>)
      (ofromos!> '!#!o!m!e!g!a !#!o!m!e!g!a!u !#!o!m!e!g!a!d)
      (t !#!o!m!e!g!a!u) !#!o!m!e!g!a!d )
   ((From Connection Defect) (tttqandn!>)
                     (connecplus!> !#!K) !#!T !#!D !#!G !#!G!I (t !#!K))
   ((From Contorsion) (tttq!>)
                     (connecplus!> !#!K!Q) !#!T !#!D !#!G !#!G!I (t !#!K!Q))
   ((From Nonmetricity Defect) (tttn!>)
                     (connecplus!> !#!K!N) !#!T !#!D !#!G !#!G!I (t !#!K!N))
   ((From Holonomic Connection) nil
		     (ofromg!>) !#!T !#!D !#!G!A!M!M!A )
   ))
(flag '(!#!o!m!e!g!a) '!+fconn)

(put  '!#!r!o!m!e!g!a   '!=type 1)
(put  '!#!r!o!m!e!g!a   '!=idxl '(t nil))
(put  '!#!r!o!m!e!g!a   '!=way '(
   (nil nil (connecplus!> nil) !#!T !#!D !#!G !#!G!I) ))
(put  '!#!r!o!m!e!g!a   '!=constr '((tttqorn!>)))
(flag '(!#!r!o!m!e!g!a) '!+fconn)

(put  '!#!o!m!e!g!a!u   '!=type 1)
(put  '!#!o!m!e!g!a!u   '!=idxl '((u . 2)))
(put  '!#!o!m!e!g!a!u   '!=way '(
  (nil nil (uconnec!>) !#!T !#!S!U !#!V!O!L (!*torsion !#!K!U))
  ((By Conjugation) nil
     (conj3!> '!#!o!m!e!g!a!u !#!o!m!e!g!a!d) !#!o!m!e!g!a!d)
  ((From Frame Connection) nil
     (oufromo!> '!#!o!m!e!g!a!u !#!o!m!e!g!a) !#!o!m!e!g!a )
  ))
(put  '!#!o!m!e!g!a!u   '!=tex "\omega")
(put  '!#!o!m!e!g!a!u   '!=constr '((sp!-n!>)))
(flag '(!#!o!m!e!g!a!u) '!+uconn)

(put  '!#!o!m!e!g!a!d   '!=type 1)
(put  '!#!o!m!e!g!a!d   '!=idxl '((d . 2)))
(put  '!#!o!m!e!g!a!d   '!=way '(
  (nil nil (dconnec!>) !#!T !#!S!D !#!V!O!L (!*torsion !#!K!D))
  ((By Conjugation) nil
     (conj3!> '!#!o!m!e!g!a!d !#!o!m!e!g!a!u) !#!o!m!e!g!a!u)
  ((From Frame Connection) nil
     (odfromo!> '!#!o!m!e!g!a!d !#!o!m!e!g!a) !#!o!m!e!g!a )
  ))
(put  '!#!o!m!e!g!a!d   '!=tex "\omega")
(put  '!#!o!m!e!g!a!d   '!=constr '((sp!-n!>)))
(flag '(!#!o!m!e!g!a!d) '!+dconn)

(put  '!#!r!o!m!e!g!a!u   '!=type 1)
(put  '!#!r!o!m!e!g!a!u   '!=idxl '((u . 2)))
(put  '!#!r!o!m!e!g!a!u   '!=way '(
   (nil nil (ruconnec!>) !#!T !#!S!U !#!V!O!L) ))
(put  '!#!r!o!m!e!g!a!u   '!=constr '((tttqorn!>) (sp!>)))
(flag '(!#!r!o!m!e!g!a!u) '!+uconn)

(put  '!#!r!o!m!e!g!a!d   '!=type 1)
(put  '!#!r!o!m!e!g!a!d   '!=idxl '((d . 2)))
(put  '!#!r!o!m!e!g!a!d   '!=way '(
   (nil nil (rdconnec!>) !#!T !#!S!D !#!V!O!L) ))
(put  '!#!r!o!m!e!g!a!d   '!=constr '((tttqorn!>) (sp!>)))
(flag '(!#!r!o!m!e!g!a!d) '!+dconn)


% Torsion ...

(put '!#!T!H!E!T!A   '!=type 2)
(put '!#!T!H!E!T!A   '!=idxl '(t))
(put '!#!T!H!E!T!A   '!=constr '((tttq!>)))
(put '!#!T!H!E!T!A   '!=way '(
   ((From Connection Defect) (tttqandn!>) (qfromk!> '!#!K) !#!T !#!K )
   ((From Contorsion) (tttq!>) (qfromk!> '!#!K!Q) !#!T !#!K!Q )
   ))
(put '!#!T!H!E!T!A   '!=tex '("\Theta" . 81))

(put '!#!Q!Q '!=type 1)
(put '!#!Q!Q '!=way '((nil nil (qqq!>) !#!T!H!E!T!A !#!D )))
(put '!#!Q!Q '!=constr '((tttq!>)))

(put '!#!Q!Q!A '!=type 1)
(put '!#!Q!Q!A '!=way '((nil nil (qqqa!>) !#!T!H!E!T!A !#!T )))
(put '!#!Q!Q!A '!=constr '((dg2!>)(tttq!>)))

(put '!#!K!Q   '!=type 1)
(put '!#!K!Q   '!=idxl '(t nil))
(put '!#!K!Q   '!=way '(
   ((From Torsion) nil (contor!>) !#!T !#!D !#!G !#!G!I !#!T!H!E!T!A )
   ((From Spinorial Contorsion) (sp!>)
      (ofromos!> '!#!K!Q !#!K!U !#!K!D) (t !#!K!U) (t !#!K!D) )
   ))
(put '!#!K!Q   '!=constr '((tttq!>)))

(put  '!#!K!U   '!=type 1)
(put  '!#!K!U   '!=idxl '((u . 2)))
(put  '!#!K!U   '!=way '(
  ((From Contorsion) (sp!>) (oufromo!> '!#!K!U !#!K!Q) !#!K!Q)
  ((By Conjugation) nil (conj3!> '!#!K!U !#!K!D) (t !#!K!D))
  ))
(put  '!#!K!U   '!=constr '((tttq!>)(sp!>)))

(put  '!#!K!D   '!=type 1)
(put  '!#!K!D   '!=idxl '((d . 2)))
(put  '!#!K!D   '!=way '(
  ((From Contorsion) (sp!>) (odfromo!> '!#!K!D !#!K!Q) !#!K!Q)
  ((By Conjugation) nil (conj3!> '!#!K!D !#!K!U) (t !#!K!U))
  ))
(put  '!#!K!D   '!=constr '((tttq!>)(sp!>)))

(put '!#!Q!T '!=type 0)
(put '!#!Q!T '!=idxl '(t))
(put '!#!Q!T '!=way '(
    ((From Torsion using Spinors) (sp!>) (qtfromthsp!>)
       !#!T!H!E!T!A !#!S!U !#!S!D !#!V!O!L )
    ((From Torsion Trace 1 - form) nil (qtfromqq!>)
       !#!Q!Q !#!D !#!G!I )
    ))
(put '!#!Q!T '!=constr '((tttq!>)))

(put '!#!Q!P '!=type 0)
(put '!#!Q!P '!=idxl '(t))
(put '!#!Q!P '!=way '(
    ((From Torsion using Spinors) (sp!>) (qpfromthsp!>)
       !#!T!H!E!T!A !#!S!U !#!S!D !#!V!O!L )
    ((From Antisymmetric Torsion 3 - form) (ttt4!>) (qpfromqqa!>)
       !#!Q!Q!A !#!D !#!G!I !#!T !#!G )
))
(put '!#!Q!P '!=constr '((tttq!>)(ttt4!>)))

(put '!#!Q!C '!=type 0)
(put '!#!Q!C '!=idxl '((u . 3)(d . 1)))
(put '!#!Q!C '!=way '(
   ((From Torsion) (sp!>) (qcfromth!>) !#!T!H!E!T!A !#!S!U !#!V!O!L) ))
(put '!#!Q!C '!=constr '((tttq!>)(sp!>)))

(put '!#!T!H!Q!C '!=type 2)
(put '!#!T!H!Q!C '!=idxl '(t))
(put '!#!T!H!Q!C '!=way '(
  (nil nil (qcfcomp!>) !#!T!H!E!T!A !#!T!H!Q!T !#!T!H!Q!A )))
(put '!#!T!H!Q!C '!=constr '((tttq!>)(dg2!>)))

(put '!#!T!H!Q!T '!=type 2)
(put '!#!T!H!Q!T '!=idxl '(t))
(put '!#!T!H!Q!T '!=way '(
  (nil nil (qtfcomp!>) !#!Q!Q !#!T )))
(put '!#!T!H!Q!T '!=constr '((tttq!>)))

(put '!#!T!H!Q!A '!=type 2)
(put '!#!T!H!Q!A '!=idxl '(t))
(put '!#!T!H!Q!A '!=way '(
  (nil nil (qafcomp!>) !#!Q!Q!A !#!D !#!G!I )))
(put '!#!T!H!Q!A '!=constr '((tttq!>)(dg2!>)))

(put '!#!T!H!Q!C!U '!=type 2)
(put '!#!T!H!Q!C!U '!=idxl '(t))
(put '!#!T!H!Q!C!U '!=way '(
    (nil (sp!>) (trfr!> '!#!T!H!Q!C!U 'gcf!> '!#!S!U) !#!S!U !#!Q!C )  ))
(put '!#!T!H!Q!C!U '!=constr '((sp!>)(tttq!>)))

(put '!#!T!H!Q!T!U '!=type 2)
(put '!#!T!H!Q!T!U '!=idxl '(t))
(put '!#!T!H!Q!T!U '!=way '(
    (nil (sp!>) (trfr!> '!#!T!H!Q!T!U 'gqf!> '!#!S!U) !#!S!U !#!Q!T )  ))
(put '!#!T!H!Q!T!U '!=constr '((sp!>)(tttq!>)))

(put '!#!T!H!Q!A!U '!=type 2)
(put '!#!T!H!Q!A!U '!=idxl '(t))
(put '!#!T!H!Q!A!U '!=way '(
  (nil (sp!>) (trfr!> '!#!T!H!Q!A!U  'gpf!> '!#!S!U) !#!S!U !#!Q!P )  ))
(put '!#!T!H!Q!A!U '!=constr '((sp!>)(tttq!>)))


% Nonmetricity ...

(put '!#!N  '!=type 1)
(put '!#!N  '!=idxl '(nil nil))
(put '!#!N  '!=sidxl '((s 1 2)))
(put '!#!N  '!=way '(
   ((From Connection Defect) (tttqandn!>) (nfromk!> '!#!K) !#!G !#!K )
   ((From Nonmetricity Defect) (tttn!>) (nfromk!> '!#!K!N) !#!G !#!K!N )
   ))
(put '!#!N  '!=constr '((tttn!>)))

(put '!#!K!N   '!=type 1)
(put '!#!K!N   '!=idxl '(t nil))
(put '!#!K!N   '!=way '(
   ((From Nonmetricity) nil (nondef!>) !#!T !#!D !#!G !#!G!I !#!N )
   ))
(put '!#!K!N   '!=constr '((tttn!>)))

(put '!#!K   '!=type 1)
(put '!#!K   '!=idxl '(t nil))
(put '!#!K   '!=way '(
   (nil nil (conndef!>) !#!T !#!D !#!G !#!G!I !#!T!H!E!T!A !#!N )
   ))
(put '!#!K   '!=constr '((tttqandn!>)))

(put '!#!N!N!W '!=type 1)
(put '!#!N!N!W '!=way '( (nil nil (compnnw!>) !#!N !#!G!I )) )
(put '!#!N!N!W '!=constr '((tttn!>)))

(put '!#!N!N!T '!=type 1)
(put '!#!N!N!T '!=way '(
  (nil nil (compnnt!>) !#!N !#!G!I !#!D !#!T !#!N!N!W )) )
(put '!#!N!N!T '!=constr '((tttn!>)))

(put '!#!N!W '!=type 1)
(put '!#!N!W '!=idxl '(nil nil))
(put '!#!N!W '!=sidxl '((s 1 2)))
(put '!#!N!W '!=way '(
  (nil nil (compnw!>) !#!G !#!N!N!W )) )
(put '!#!N!W '!=constr '((tttn!>)))

(put '!#!N!T '!=type 1)
(put '!#!N!T '!=idxl '(nil nil))
(put '!#!N!T '!=sidxl '((s 1 2)))
(put '!#!N!T '!=way '(
  (nil nil (compnt!>) !#!G !#!T !#!N!N!T )) )
(put '!#!N!T '!=constr '((tttn!>)))

(put '!#!N!A '!=type 1)
(put '!#!N!A '!=idxl '(nil nil))
(put '!#!N!A '!=sidxl '((s 1 2)))
(put '!#!N!A '!=way '(
  (nil nil (compna!>) !#!D !#!T !#!N !#!N!W !#!N!T )))
(put '!#!N!A '!=constr '((tttn!>)(dg2!>)))

(put '!#!N!C '!=type 1)
(put '!#!N!C '!=idxl '(nil nil))
(put '!#!N!C '!=sidxl '((s 1 2)))
(put '!#!N!C '!=way '(
  (nil nil (compnc!>) !#!N ((geq ![dim!] 3) !#!N!A) !#!N!T !#!N!W )))
(put '!#!N!C '!=constr '((tttn!>)))


% Curvature ...

(put  '!#!O!M!E!G!A   '!=type 2)
(put  '!#!O!M!E!G!A   '!=idxl '(t nil))
(put  '!#!O!M!E!G!A   '!=way '(
  (nil nil (curvature!>) !#!o!m!e!g!a )
  ((From Spinorial Curvature) (sp!-n!>)
      (ofromos!> '!#!O!M!E!G!A !#!O!M!E!G!A!U !#!O!M!E!G!A!D)
      (t !#!O!M!E!G!A!U) !#!O!M!E!G!A!D )
  ))
(put  '!#!O!M!E!G!A   '!=tex '("\Omega" . 87))


(put  '!#!O!M!E!G!A!U   '!=type 2)
(put  '!#!O!M!E!G!A!U   '!=idxl '((u . 2)))
(put  '!#!O!M!E!G!A!U   '!=way '(
  (nil nil (scurvature!> '!#!O!M!E!G!A!U !#!o!m!e!g!a!u)
		    !#!o!m!e!g!a!u )
  ((By Conjugation) nil
     (conj3!> '!#!O!M!E!G!A!U !#!O!M!E!G!A!D) !#!O!M!E!G!A!D)
  ((From Curvature) nil
     (oufromo!> '!#!O!M!E!G!A!U !#!O!M!E!G!A) !#!O!M!E!G!A )
  ))
(put  '!#!O!M!E!G!A!U   '!=constr '((sp!-n!>)))
(put  '!#!O!M!E!G!A!U   '!=tex '("\Omega" . 87))

(put  '!#!O!M!E!G!A!D   '!=type 2)
(put  '!#!O!M!E!G!A!D   '!=idxl '((d . 2)))
(put  '!#!O!M!E!G!A!D   '!=way '(
  (nil nil (scurvature!> '!#!O!M!E!G!A!D !#!o!m!e!g!a!d)
		    !#!o!m!e!g!a!d )
  ((By Conjugation) nil
     (conj3!> '!#!O!M!E!G!A!D !#!O!M!E!G!A!U) !#!O!M!E!G!A!U)
  ((From Curvature) nil
     (odfromo!> '!#!O!M!E!G!A!D !#!O!M!E!G!A) !#!O!M!E!G!A )
  ))
(put  '!#!O!M!E!G!A!D   '!=constr '((sp!-n!>)))
(put  '!#!O!M!E!G!A!D   '!=tex '("\Omega" . 87))

(put '!#!R!I!M  '!=type 0)
(put '!#!R!I!M  '!=idxl '(t nil nil nil))
(put '!#!R!I!M  '!=sidxl '((a 3 4)))
(put '!#!R!I!M  '!=way '(
  (nil nil (riemm!>) !#!D !#!O!M!E!G!A ) ))
(put '!#!R!I!M  '!=tex '!R)

(put '!#!R!I!C  '!=type 0)
(put '!#!R!I!C  '!=idxl '( nil nil))
(put '!#!R!I!C  '!=sidxl '((s 1 2)))
(put '!#!R!I!C  '!=way '(
  ((From Curvature) nil (riccio!>) !#!D !#!G !#!G!I !#!O!M!E!G!A )
  ((From Riemann Tensor) nil (ricci!>) !#!R!I!M ) ))
(put '!#!R!I!C  '!=tex '!R)

(put '!#!R!I!C!A  '!=type 0)
(put '!#!R!I!C!A  '!=idxl '( nil nil))
(put '!#!R!I!C!A  '!=way '(
  ((From Curvature) nil (riccioa!>) !#!D !#!G !#!G!I !#!O!M!E!G!A )))
(put '!#!R!I!C!A  '!=constr '((tttn!>)))

(put '!#!R!R  '!=type 0)
(put '!#!R!R  '!=way '(
  ((From A - Ricci Tensor) (tttn!>) (rscalara!>)  !#!G!I (t !#!R!I!C!A) )
  ((From Ricci Tensor) nil (rscalar!>)  !#!G!I !#!R!I!C )
  ((From Spinor Curvature) (sp!-n!>)
     (rrsp!>) (t !#!O!M!E!G!A!U) !#!S!U !#!V!O!L )
  ))
(put '!#!R!R  '!=tex '!R)

(put '!#!G!T  '!=type 0)
(put '!#!G!T  '!=idxl '( nil nil))
(put '!#!G!T  '!=sidxl '((s 1 2)))
(put '!#!G!T  '!=way '(
  (nil nil (gtensor!>)  !#!G !#!R!R !#!R!I!C ) ))

(put '!#!R!W '!=type 0)
(put '!#!R!W '!=idxl '((u . 4)))
(put '!#!R!W '!=way '(
   ((From Spinor Curvature) nil (rwsp!>) !#!O!M!E!G!A!U !#!S!U !#!V!O!L)))
(put '!#!R!W '!=tex '!C)
(put '!#!R!W '!=constr '((sp!-n!>)))

(put '!#!R!C '!=type 0)
(put '!#!R!C '!=idxl '((u . 2)(d . 2)))
(put '!#!R!C '!=sidxl '((h 1 2)))
(put '!#!R!C '!=way '(
    ((From Spinor Curvature) nil (rcsp!>)
        !#!O!M!E!G!A!U !#!S!D !#!V!O!L
        (!*torsion !#!O!M!E!G!A!D !#!S!U))))
(put '!#!R!C '!=tex '!C)
(put '!#!R!C '!=constr '((sp!-n!>)))

(put '!#!R!A '!=type 0)
(put '!#!R!A '!=idxl '((u . 2)))
(put '!#!R!A '!=way '(
    ((From Spinor Curvature) nil (rasp!>) !#!O!M!E!G!A!U !#!S!U !#!V!O!L)))
(put '!#!R!A '!=tex '!A)
(put '!#!R!A '!=constr '((tttq!>)(sp!-n!>)))

(put '!#!R!B '!=type 0)
(put '!#!R!B '!=idxl '((u . 2)(d . 2)))
(put '!#!R!B '!=sidxl '((h 1 2)))
(put '!#!R!B '!=way '(
    ((From Spinor Curvature) nil
       (rbsp!>) !#!O!M!E!G!A!U !#!O!M!E!G!A!D !#!S!U !#!S!D !#!V!O!L)))
(put '!#!R!B '!=tex '!B)
(put '!#!R!B '!=constr '((tttq!>)(sp!-n!>)))

(put '!#!R!D '!=type 0)
(put '!#!R!D '!=way '(
    ((From Spinor Curvature) (sp!-n!>)
       (rdsp!>) !#!O!M!E!G!A!U !#!S!U !#!V!O!L)))
(put '!#!R!D '!=tex '!D)
(put '!#!R!D '!=constr '((tttq!>)(ttt4!>)))

(put '!#!O!M!W!U '!=type 2)
(put '!#!O!M!W!U '!=idxl '((u . 2)))
(put '!#!O!M!W!U '!=way '(
    (nil nil (crfr!> '!#!O!M!W!U 'gwf!> '!#!S!U) !#!S!U !#!R!W )  ))
(put '!#!O!M!W!U '!=constr '((sp!-n!>)))

(put '!#!O!M!C!U '!=type 2)
(put '!#!O!M!C!U '!=idxl '((u . 2)))
(put '!#!O!M!C!U '!=way '(
    (nil nil (crfr!> '!#!O!M!C!U 'gtf!> '!#!S!D) !#!S!D !#!R!C )  ))
(put '!#!O!M!C!U '!=constr '((sp!-n!>)))

(put '!#!O!M!R!U '!=type 2)
(put '!#!O!M!R!U '!=idxl '((u . 2)))
(put '!#!O!M!R!U '!=way '(
    (nil nil (crfr!> '!#!O!M!R!U 'gsf!> '!#!S!U) !#!S!U !#!R!R )  ))
(put '!#!O!M!R!U '!=constr '((sp!-n!>)))

(put '!#!O!M!A!U '!=type 2)
(put '!#!O!M!A!U '!=idxl '((u . 2)))
(put '!#!O!M!A!U '!=way '(
    (nil nil (crfr!> '!#!O!M!A!U 'gaf!> '!#!S!U) !#!S!U !#!R!A )  ))
(put '!#!O!M!A!U '!=constr '((sp!-n!>)(tttqnotn!>)))

(put '!#!O!M!B!U '!=type 2)
(put '!#!O!M!B!U '!=idxl '((u . 2)))
(put '!#!O!M!B!U '!=way '(
    (nil nil (crfr!> '!#!O!M!B!U 'gbf!> '!#!S!D) !#!S!D !#!R!B )  ))
(put '!#!O!M!B!U '!=constr '((sp!-n!>)(tttqnotn!>)))

(put '!#!O!M!D!U '!=type 2)
(put '!#!O!M!D!U '!=idxl '((u . 2)))
(put '!#!O!M!D!U '!=way '(
    (nil nil (crfr!> '!#!O!M!D!U 'gdf!> '!#!S!U) !#!S!U !#!R!D )  ))
(put '!#!O!M!D!U '!=constr '((sp!-n!>)(tttqnotn!>)))

(put '!#!O!M!W '!=type 2)
(put '!#!O!M!W '!=idxl '(nil nil))
(put '!#!O!M!W '!=sidxl '((a 1 2)))
(put '!#!O!M!W '!=way '(
  (nil nil (mkrwf!>) !#!G !#!O!M!E!G!A !#!O!M!C !#!O!M!R
		      ((or !*torsion !*nonmetr) !#!O!M!A !#!O!M!B !#!O!M!D )
  )))
(put '!#!O!M!W '!=constr '((dg3!>)))

(put '!#!O!M!C '!=type 2)
(put '!#!O!M!C '!=idxl '(nil nil))
(put '!#!O!M!C '!=sidxl '((a 1 2)))
(put '!#!O!M!C '!=way '(
  (nil nil (mkrcf!>) !#!G !#!T (!*nonmetr !#!R!I!C!A)
			       ((not !*nonmetr) !#!R!I!C) !#!R!R )))
(put '!#!O!M!C '!=constr '((dg2!>)))

(put '!#!O!M!R '!=type 2)
(put '!#!O!M!R '!=idxl '(nil nil))
(put '!#!O!M!R '!=sidxl '((a 1 2)))
(put '!#!O!M!R '!=way '(
  (nil nil (mkrrf!>) !#!G !#!S !#!R!R )))
(put '!#!O!M!R '!=sidxl '((a 1 2)))

(put '!#!O!M!A '!=type 2)
(put '!#!O!M!A '!=idxl '(nil nil))
(put '!#!O!M!A '!=sidxl '((a 1 2)))
(put '!#!O!M!A '!=way '(
  (nil nil (mkraf!>) !#!G !#!T (!*nonmetr !#!R!I!C!A)
			       ((not !*nonmetr) !#!R!I!C) )))
(put '!#!O!M!A '!=constr '((tttqorn!>)(dg2!>)))

(put '!#!O!M!B '!=type 2)
(put '!#!O!M!B '!=idxl '(nil nil))
(put '!#!O!M!B '!=sidxl '((a 1 2)))
(put '!#!O!M!B '!=way '(
  (nil nil (mkrbf!>) !#!G !#!T !#!D !#!O!M!E!G!A
                     !#!O!M!R !#!O!M!C !#!O!M!A !#!O!M!D  )))
(put '!#!O!M!B '!=constr '((tttqorn!>)(dg3!>)))

(put '!#!O!M!D '!=type 2)
(put '!#!O!M!D '!=idxl '(nil nil))
(put '!#!O!M!D '!=sidxl '((a 1 2)))
(put '!#!O!M!D '!=way '(
  (nil nil (mkrdf!>) !#!G !#!D !#!S !#!T !#!O!M!E!G!A )))
(put '!#!O!M!D '!=constr '((tttqorn!>)(dg3!>)))

(put '!#!R!I!C!S '!=type 0)
(put '!#!R!I!C!S '!=idxl '(nil nil))
(put '!#!R!I!C!S  '!=way '(
  ((From Curvature) nil (riccios!>) !#!D !#!G !#!G!I !#!O!M!E!G!A )))
(put '!#!R!I!C!S '!=constr '((tttn!>)))

(put '!#!O!M!E!G!A!H '!=type 2)
(put '!#!O!M!E!G!A!H '!=way '(
  (nil nil (mkomegah!>) !#!O!M!E!G!A )))
(put '!#!O!M!E!G!A!H '!=constr '((tttn!>)))

(put '!#!O!S!H '!=type 2)
(put '!#!O!S!H '!=idxl '(nil nil))
(put '!#!O!S!H '!=sidxl '((s 1 2)))
(put '!#!O!S!H '!=way '(
  (nil (deq2!>) (mkrshf2!>) !#!G !#!T !#!O!M!E!G!A #!O!S!C )
  (nil (dg2!>)  (mkrshf!>) !#!G !#!O!M!E!G!A!H )
% (nil (dg2!>)  (mkrshf!>) !#!G !#!T !#!O!M!E!G!A!H )
  ))
(put '!#!O!S!H '!=constr '((tttn!>)))

(put '!#!O!S!A '!=type 2)
(put '!#!O!S!A '!=idxl '(nil nil))
(put '!#!O!S!A '!=sidxl '((s 1 2)))
(put '!#!O!S!A '!=way '(
  (nil nil (mkrsaf!>) !#!G !#!T !#!S !#!D !#!R!I!C!S !#!O!M!E!G!A!H )
% (nil nil (mkrsaf!>) !#!G !#!T !#!S !#!R!I!C!S )
  ))
(put '!#!O!S!A '!=constr '((tttn!>)(dg2!>)))

(put '!#!O!S!C '!=type 2)
(put '!#!O!S!C '!=idxl '(nil nil))
(put '!#!O!S!C '!=sidxl '((s 1 2)))
(put '!#!O!S!C '!=way '(
  (nil nil (mkrscf!>) !#!G !#!T !#!S !#!R!I!C!S )))
(put '!#!O!S!C '!=constr '((tttn!>)))

(put '!#!O!S!V '!=type 2)
(put '!#!O!S!V '!=idxl '(nil nil))
(put '!#!O!S!V '!=sidxl '((s 1 2)))
(put '!#!O!S!V '!=way '(
  (nil nil (mkrsvf!>) !#!T !#!D !#!G !#!O!S!H !#!O!S!A !#!O!S!C )))
(put '!#!O!S!V '!=constr '((tttn!>)(dg3!>)))

(put '!#!O!S!U '!=type 2)
(put '!#!O!S!U '!=idxl '(nil nil))
(put '!#!O!S!U '!=sidxl '((s 1 2)))
(put '!#!O!S!U '!=way '(
  (nil nil (mkrsuf!>) !#!G !#!O!M!E!G!A
                      ((geq ![dim!] 4) !#!O!S!V )
                      !#!O!S!H !#!O!S!A !#!O!S!C )))
(put '!#!O!S!U '!=constr '((tttn!>)(dg2!>)))


% Dirac field ...

(put '!#!P!H!I '!=type 0)
(put '!#!P!H!I '!=idxl '((u . 1)))
(put '!#!C!H!I '!=type 0)
(put '!#!C!H!I '!=idxl '((u . 1)))
(put '!#!P!H!I '!=tex "\phi")
(put '!#!C!H!I '!=tex "\chi")
(put '!#!P!H!I '!=constr '((sp!-n!>)))
(put '!#!C!H!I '!=constr '((sp!-n!>)))

(put '!#!D!A!C!T '!=type 4)
(put '!#!D!A!C!T '!=way '(
    (nil (sp!-n!>) (dact!>)
         !#!P!H!I !#!C!H!I !#!D !#!T !#!o!m!e!g!a!u !#!G
         !#!V!O!L !#!s!d!e!t!G !#!G!I (!*torsion !#!Q!Q))     ))
(put '!#!D!A!C!T  '!=dens '(t nil t nil))

(put '!#!T!D!I '!=type 0)
(put '!#!T!D!I '!=idxl '(nil nil))
(put '!#!T!D!I '!=sidxl '((s 1 2)))
(put '!#!T!D!I '!=way '(
    (nil (sp!-n!>) (tdi!>)
       !#!T !#!D !#!G !#!G!I !#!s!d!e!t!G !#!V!O!L
       !#!D!A!C!T !#!P!H!I !#!C!H!I !#!o!m!e!g!a!u )   ))
(put '!#!T!D!I '!=constr '((tttnotn!>)))

(put '!#!S!P!D!I!U '!=type 3)
(put '!#!S!P!D!I!U '!=idxl '((u . 2)))
(put '!#!S!P!D!I!U '!=way '(
  (nil nil (spinsd!>)
      !#!C!H!I !#!P!H!I !#!T !#!s!d!e!t!G !#!G !#!V!O!L ) ))
(put '!#!S!P!D!I!U '!=constr '((sp!-n!>)))

(put '!#!D!P!q '!=type 0)
(put '!#!D!P!q '!=idxl '((d . 1)))
(put '!#!D!P!q '!=way '(
    (nil nil (dequ!> !#!P!H!I !#!C!H!I '!#!D!P!q t)
        !#!P!H!I !#!C!H!I !#!D !#!o!m!e!g!a!u (!*torsion !#!Q!Q)) ))
(put '!#!D!P!q '!=constr '((sp!-n!>)))

(put '!#!D!C!q '!=type 0)
(put '!#!D!C!q '!=idxl '((d . 1)))
(put '!#!D!C!q '!=way '(
    (nil nil (dequ!> !#!C!H!I !#!P!H!I '!#!D!C!q nil)
        !#!P!H!I !#!C!H!I !#!D !#!o!m!e!g!a!u (!*torsion !#!Q!Q)) ))
(put '!#!D!C!q '!=constr '((sp!-n!>)))


% EM field ...

(put '!#!A '!=type 1)
(put '!#!A '!=constr '((dg2!>)))

(put '!#!F!F '!=type 2)
(put '!#!F!F '!=way '(
    ((From EM Potential) nil (fffroma!>) !#!A)
    ((From EM Tensor) nil (fffromft!>) !#!S (t !#!F!T))
    ((From Complex EM 2 - form) (sp!>) (fffromffu!>) (t !#!F!F!U))
    ))
(put '!#!F!F '!=constr '((dg2!>)))

(put '!#!J '!=type 1)
(put '!#!J '!=way '(
    ((From Dirac Spinor) (sp!>) (dcurr!>) !#!P!H!I !#!C!H!I !#!T )  ))
(put '!#!J '!=constr '((dg2!>)))

(put '!#!F!T '!=type 0)
(put '!#!F!T '!=idxl '(nil nil))
(put '!#!F!T '!=sidxl '((a 1 2)))
(put '!#!F!T '!=way '(
    (nil nil (ftfromff!>) !#!D !#!F!F)  ))
(put '!#!F!T '!=constr '((dg2!>)))

(put '!#!E!M!A!C!T '!=type '![dim!])
(put '!#!E!M!A!C!T '!=way '(
    (nil nil (emact!>) !#!F!F !#!V!O!L !#!s!d!e!t!G !#!T !#!G )
    ))
(put '!#!E!M!A!C!T '!=constr '((dg2!>)))
(put '!#!E!M!A!C!T  '!=dens '(t nil t nil))

(put '!#!T!E!M '!=type 0)
(put '!#!T!E!M '!=idxl '(nil nil))
(put '!#!T!E!M '!=sidxl '((s 1 2)))
(put '!#!T!E!M '!=way '(
  (nil nil (tembydef!>)
     !#!G!I !#!G !#!V!O!L !#!F!T !#!E!M!A!C!T )
  ))
(put '!#!T!E!M '!=constr '((dg2!>)))

(put '!#!M!W!F!q '!=type '![dim1!])
(put '!#!M!W!F!q '!=way '(
    (nil nil (firstmw!>) !#!T !#!G !#!s!d!e!t!G !#!V!O!L !#!F!F ) ))
(put '!#!M!W!F!q '!=constr '((dg2!>)))

(put '!#!M!W!S!q '!=type 3)
(put '!#!M!W!S!q '!=way '(
    (nil nil (secondmw!>) !#!F!F )))
(put '!#!M!W!S!q '!=constr '((dg2!>)))

(put '!#!C!O!q  '!=type '![dim!])
(put '!#!C!O!q '!=way '(
    (nil nil (contineq!>) !#!T !#!G !#!s!d!e!t!G !#!V!O!L !#!J ) ))
(put '!#!C!O!q '!=constr '((dg2!>)))

(put '!#!S!C!F '!=type 0)
(put '!#!S!C!F '!=way '(
    (nil (ttt4!>) (firstscal!>)
       !#!T !#!G !#!s!d!e!t!G !#!V!O!L !#!F!F ) ))
(put '!#!S!C!F '!=constr '((ttt4!>)))

(put '!#!S!C!S '!=type 0)
(put '!#!S!C!S '!=way '(
    (nil (ttt4!>) (secondscal!>)
       !#!T !#!G !#!s!d!e!t!G !#!V!O!L !#!F!F ) ))
(put '!#!S!C!F '!=constr '((ttt4!>)))

(put '!#!F!I!U '!=type 0)
(put '!#!F!I!U '!=idxl '((u . 2)))
(put '!#!F!I!U '!=way '(
  ((From Complex EM 2 - form) nil (fiufromffu!>) !#!S!U !#!V!O!L !#!F!F!U )
  ((From EM 2 - form) nil (fiufromff!>) !#!S!U !#!V!O!L !#!F!F )
  ))
(put '!#!F!I!U '!=constr '((sp!>)))
(put '!#!F!I!U '!=tex '("\Phi" . 70))

(put '!#!S!D!q '!=type 4)
(put '!#!S!D!q '!=idxl '((d . 2)))
(put '!#!S!D!q '!=way '(
  (nil nil (sduality!>) !#!S!D !#!F!F!U )
  ))
(put '!#!S!D!q '!=constr '((sp!>)))

(put '!#!F!F!U '!=type 2)
(put '!#!F!F!U '!=way '(
  ((From EM 2 - form) nil (ffufromff!>)
     !#!V!O!L !#!T !#!G !#!s!d!e!t!G !#!F!F )
  ((From EM Spinor) (sp!>) (ffufromfiu!>)
     !#!S!U !#!F!I!U )
  ))
(put '!#!F!F!U '!=constr '((sp!>)))
(put '!#!F!F!U '!=tex '("\Phi" . 70))

(put '!#!S!C!U '!=type 0)
(put '!#!S!C!U '!=way '(
  ((From EM Spinor) nil (scufromfiu!>) !#!F!I!U )
  ((From Complex EM 2 - form) nil (scufromffu!>) !#!V!O!L !#!F!F!U )
  ))
(put '!#!S!C!U '!=constr '((sp!>)))

(put '!#!M!W!U!q '!=type 3)
(put '!#!M!W!U!q '!=way '(
    (nil nil (complexmw!>)
       !#!T !#!G !#!s!d!e!t!G !#!V!O!L !#!F!F!U ) ))
(put '!#!M!W!U!q '!=constr '((sp!>)))

(put '!#!T!E!M!S '!=type 0)
(put '!#!T!E!M!S '!=idxl '((u . 2)(d . 2)))
(put '!#!T!E!M!S '!=sidxl '((h 1 2)))
(put '!#!T!E!M!S '!=way '(
  (nil nil (tems!>) !#!F!I!U )))
(put '!#!T!E!M!S '!=constr '((sp!>)))

% YM field ...

(put '!#!A!Y!M '!=type 1)
(put '!#!A!Y!M '!=idxl '((n . 9)))
(put '!#!A!Y!M '!=constr '((dg2!>)))

(put '!#!S!C!O!N!S!T '!=type 0)
(put '!#!S!C!O!N!S!T '!=idxl '((n . 9)(n . 9)(n . 9)))
(put '!#!S!C!O!N!S!T '!=sidxl '((a 1 2 3)))
(put '!#!S!C!O!N!S!T '!=constr '((dg2!>)))

(put '!#!F!F!Y!M '!=type 2)
(put '!#!F!F!Y!M '!=idxl '((n . 9)))
(put '!#!F!F!Y!M '!=way '(
    ((From YM Potential) nil (ffymfromaym!>) !#!A!Y!M !#!S!C!O!N!S!T )
    ((From YM Tensor) nil (ffymfromftym!>) !#!S (t !#!F!T!Y!M))
    ))
(put '!#!F!F!Y!M '!=constr '((dg2!>)))

(put '!#!F!T!Y!M '!=type 0)
(put '!#!F!T!Y!M '!=idxl '((n . 9) nil nil))
(put '!#!F!T!Y!M '!=sidxl '((a 2 3)))
(put '!#!F!T!Y!M '!=way '(
    (nil nil (ftymfromffym!>) !#!D !#!F!F!Y!M)  ))
(put '!#!F!T!Y!M '!=constr '((dg2!>)))

(put '!#!Y!M!A!C!T '!=type '![dim!])
(put '!#!Y!M!A!C!T '!=way '(
    (nil nil (ymact!>) !#!F!F!Y!M !#!V!O!L !#!s!d!e!t!G !#!T !#!G )
    ))
(put '!#!Y!M!A!C!T '!=constr '((dg2!>)))
(put '!#!Y!M!A!C!T  '!=dens '(t nil t nil))

(put '!#!T!Y!M '!=type 0)
(put '!#!T!Y!M '!=idxl '(nil nil))
(put '!#!T!Y!M '!=sidxl '((s 1 2)))
(put '!#!T!Y!M '!=way '(
  (nil nil (tymbydef!>)
     !#!G!I !#!G !#!V!O!L !#!F!T!Y!M !#!Y!M!A!C!T )
  ))
(put '!#!T!Y!M '!=constr '((dg2!>)))

(put '!#!Y!M!F!q '!=type '![dim1!])
(put '!#!Y!M!F!q '!=idxl '((n . 9)))
(put '!#!Y!M!F!q '!=way '(
    (nil nil (firstym!>)
      !#!T !#!G !#!s!d!e!t!G !#!V!O!L !#!F!F!Y!M !#!S!C!O!N!S!T ) ))
(put '!#!Y!M!F!q '!=constr '((dg2!>)))

(put '!#!Y!M!S!q '!=type 3)
(put '!#!Y!M!S!q '!=idxl '((n . 9)))
(put '!#!Y!M!S!q '!=way '(
    (nil nil (secondym!>) !#!F!F!Y!M !#!S!C!O!N!S!T ) ))
(put '!#!Y!M!S!q '!=constr '((dg2!>)))


% Scalar field ...

(put '!#!F!I '!=type 0)

(put '!#!S!A!C!T!M!I!N '!=type '![dim!])
(put '!#!S!A!C!T!M!I!N '!=way '(
   (nil nil (sactmin!>) !#!F!I !#!V!O!L !#!G!I !#!D )))
(put '!#!S!A!C!T!M!I!N '!=dens '(t nil t nil))

(put '!#!S!A!C!T '!=type '![dim!])
(put '!#!S!A!C!T '!=way '(
   (nil nil (sact!>) !#!F!I !#!V!O!L !#!G!I !#!D
                              (!*nonmin !#!R!R !#!A!C!O!N!S!T ) )))
(put '!#!S!A!C!T '!=dens '(t nil t nil))

(put '!#!S!C!q '!=type 0)
(put '!#!S!C!q '!=way '(
    (nil nil (kgeq!>)  !#!V!O!L !#!s!d!e!t!G !#!D !#!T !#!F!I
                                (!*nonmin !#!A!C!O!N!S!T !#!R!R ))))



(put '!#!T!S!C!L!M!I!N '!=type 0)
(put '!#!T!S!C!L!M!I!N '!=idxl '(nil nil))
(put '!#!T!S!C!L!M!I!N '!=sidxl '((s 1 2)))
(put '!#!T!S!C!L!M!I!N '!=way '(
   (nil nil (tsclmin!>)
      !#!F!I !#!V!O!L !#!G !#!D !#!S!A!C!T!M!I!N )))


% Constants ...

(put '!#!A!C!O!N!S!T '!=type 0)
(put '!#!A!C!O!N!S!T '!=idxl '((n . 2)))
(put '!#!A!C!O!N!S!T '!=way '((nil nil (aconst!>))))
(put '!#!M!C!O!N!S!T '!=type 0)
(put '!#!M!C!O!N!S!T '!=idxl '((n . 3)))
(put '!#!M!C!O!N!S!T '!=way '((nil nil (mconst!>))))
(put '!#!L!C!O!N!S!T '!=type 0)
(put '!#!L!C!O!N!S!T '!=idxl '((n . 6)))
(put '!#!L!C!O!N!S!T '!=way '((nil nil (lconst!>))))

% Einstein Equations ...

(put '!#!E!E!q '!=type 0)
(put '!#!E!E!q '!=idxl '(nil nil))
(put '!#!E!E!q '!=sidxl '((s 1 2)))
(put '!#!E!E!q '!=way '(
  (nil nil (einstein!>) !#!G !#!R!I!C !#!R!R !#!T!E!N!M!O!M )))
(put '!#!E!E!q '!=constr '((tttnotqn!>)))

(put '!#!T!E!E!q '!=type 0)
(put '!#!T!E!E!q '!=way '(
  (nil nil (einsteint!>) !#!R!R !#!T!E!N!M!O!M!T )))
(put '!#!T!E!E!q '!=constr '((sp!>)(tttnotqn!>)))

(put '!#!C!E!E!q '!=type 0)
(put '!#!C!E!E!q '!=idxl '((u . 2)(d . 2)))
(put '!#!C!E!E!q '!=sidxl '((h 1 2)))
(put '!#!C!E!E!q '!=way '(
  (nil nil (einsteinc!>) !#!R!C !#!T!E!N!M!O!M!S )))
(put '!#!C!E!E!q '!=constr '((sp!>)(tttnotqn!>)))

% Gravitational Equations ...

(put '!#!P!O!M!E!G!A!U '!=type 2)
(put '!#!P!O!M!E!G!A!U '!=idxl '((u . 2)))
(put '!#!P!O!M!E!G!A!U '!=way '(
  (nil nil (pomegau!>)  !#!L!C!O!N!S!T !#!S!U
                        (!*nonmin !#!A!C!O!N!S!T !#!F!I) )))
(put '!#!P!O!M!E!G!A!U '!=constr '((sp!-n!>)))

(put '!#!P!T!H!E!T!A '!=type 2)
(put '!#!P!T!H!E!T!A '!=idxl '(t))
(put '!#!P!T!H!E!T!A '!=way '(
  (nil nil (ptheta!>)  !#!M!C!O!N!S!T )))
(put '!#!P!T!H!E!T!A '!=constr '((sp!-n!>)(tttq!>)))

(put '!#!L!A!C!T '!=type 4)
(put '!#!L!A!C!T '!=way '(
  (nil (sp!-n!>) (lact!>)
       !#!V!O!L !#!R!R !#!L!C!O!N!S!T
       !#!P!O!M!E!G!A!U !#!O!M!E!G!A!U
       (!*torsion !#!P!T!H!E!T!A !#!T!H!E!T!A)
       (!*nonmin !#!A!C!O!N!S!T !#!F!I) )
  ))
(put '!#!L!A!C!T '!=constr '((ttt4!>)))
(put '!#!L!A!C!T '!=dens '(t nil t nil))

(put '!#!M!E!T!R!q '!=type 0)
(put '!#!M!E!T!R!q '!=idxl '(nil nil))
(put '!#!M!E!T!R!q '!=sidxl '((s 1 2)))
(put '!#!M!E!T!R!q '!=way '(
  (nil nil (metrequation!>)
    !#!D !#!T !#!S!U !#!V!O!L !#!L!A!C!T !#!T!E!N!M!O!M
    !#!o!m!e!g!a!u !#!o!m!e!g!a!d
    !#!O!M!E!G!A!U !#!P!O!M!E!G!A!U
    (!*torsion !#!T!H!E!T!A !#!P!T!H!E!T!A ) )
  ))
(put '!#!M!E!T!R!q '!=constr '((sp!-n!>)))

(put '!#!T!O!R!S!q '!=type 3)
(put '!#!T!O!R!S!q '!=idxl '((u . 2)))
(put '!#!T!O!R!S!q '!=way '(
  (nil nil (torsequation!>)
    !#!T !#!S!U !#!o!m!e!g!a!u !#!P!O!M!E!G!A!U !#!P!T!H!E!T!A
    !#!S!P!I!N!U )
    ))
(put '!#!T!O!R!S!q '!=constr '((sp!-n!>)(tttq!>)))


% Geodesics and congruences ...

(put '!#!G!E!O!q '!=type 0)
(put '!#!G!E!O!q '!=idxl '(1))
(put '!#!G!E!O!q '!=way '(
   (nil (tttapar!>) (geodesics!>) !#!G !#!G!I !#!T !#!D )))
(put '!#!G!E!O!q '!=constr '((tttapar!>)))


% Null congruence ...

(put '!#!K!V '!=type -1)

(put '!#!N!C!o '!=type 0)
(put '!#!N!C!o '!=way '(
  (nil nil (ncnq!>) !#!T !#!D !#!G !#!G!I !#!K!V )))

(put '!#!G!C!o '!=type 0)
(put '!#!G!C!o '!=idxl '(t))
(put '!#!G!C!o '!=way '(
  (nil nil (ncgq!>)
     !#!T !#!D !#!G !#!G!I !#!K!V
     ((or !*torsion !*nonmetr) !#!r!o!m!e!g!a)
     ((and (null !*torsion) (null !*nonmetr)) !#!o!m!e!g!a) )
))

(put '!#!t!h!e!t!a!O '!=type 0)
(put '!#!t!h!e!t!a!O '!=way '(
  (nil nil (nctheta!>)
     !#!T !#!D !#!G !#!G!I !#!K!V !#!N!C!o !#!G!C!o
     ((or !*torsion !*nonmetr) !#!r!o!m!e!g!a)
     ((and (null !*torsion) (null !*nonmetr)) !#!o!m!e!g!a) )
))
(put '!#!t!h!e!t!a!O '!=constr '((ttt4!>)))

(put '!#!o!m!e!g!a!S!Q!O '!=type 0)
(put '!#!o!m!e!g!a!S!Q!O '!=way '(
  (nil nil (ncomega!>)
     !#!T !#!D !#!G !#!G!I !#!K!V !#!N!C!o !#!G!C!o
     ((or !*torsion !*nonmetr) !#!r!o!m!e!g!a)
     ((and (null !*torsion) (null !*nonmetr)) !#!o!m!e!g!a) )
))
(put '!#!o!m!e!g!a!S!Q!O '!=constr '((ttt4!>)))


(put '!#!s!i!g!m!a!S!Q!O '!=type 0)
(put '!#!s!i!g!m!a!S!Q!O '!=way '(
  (nil nil (ncsigma!>)
     !#!T !#!D !#!G !#!G!I !#!K!V !#!t!h!e!t!a!O
     !#!N!C!o !#!G!C!o
     ((or !*torsion !*nonmetr) !#!r!o!m!e!g!a)
     ((and (null !*torsion) (null !*nonmetr)) !#!o!m!e!g!a) )
))
(put '!#!s!i!g!m!a!S!Q!O '!=constr '((ttt4!>)))

% Kinematics ...

(put '!#!U!V '!=type -1)
(put '!#!U!V '!=way '((nil nil (uvfromuup!>) !#!D !#!U!U )))

(put '!#!U!U '!=type 0)
(put '!#!U!U '!=idxl '(t))
(put '!#!U!U '!=way '(
  ((By Default) (tttdiag!>) (uudefault!>) )
  ((From Velocity Vector) nil (uupfromuv!>) !#!T (t !#!U!V) )
  ))

(put '!#!U!S!Q '!=type 0)
(put '!#!U!S!Q '!=way '((nil nil (usquare!>) !#!U!U !#!G )))

(put '!#!P!R '!=type 0)
(put '!#!P!R '!=idxl '(t nil))
(put '!#!P!R '!=way '((nil nil (projector!>) !#!U!U !#!U!S!Q )))

(put '!#!a!c!c!U '!=type 0)
(put '!#!a!c!c!U '!=idxl '(t))
(put '!#!a!c!c!U '!=way '(
  (nil nil (accelerat!>)
     !#!T !#!D !#!G !#!G!I !#!U!U
     ((or !*torsion !*nonmetr) !#!r!o!m!e!g!a)
     ((and (null !*torsion) (null !*nonmetr)) !#!o!m!e!g!a) )
))

(put '!#!o!m!e!g!a!U '!=type 0)
(put '!#!o!m!e!g!a!U '!=idxl '(nil nil))
(put '!#!o!m!e!g!a!U '!=sidxl '((a 1 2)))
(put '!#!o!m!e!g!a!U '!=way '(
  (nil nil (uomega!>)
     !#!T !#!D !#!G !#!G!I !#!U!U !#!P!R
     ((or !*torsion !*nonmetr) !#!r!o!m!e!g!a)
     ((and (null !*torsion) (null !*nonmetr)) !#!o!m!e!g!a) )
))

(put '!#!s!i!g!m!a!U '!=type 0)
(put '!#!s!i!g!m!a!U '!=idxl '(nil nil))
(put '!#!s!i!g!m!a!U '!=sidxl '((s 1 2)))
(put '!#!s!i!g!m!a!U '!=way '(
  (nil nil (usigma!>)
     !#!T !#!D !#!G !#!G!I !#!U!U !#!P!R !#!t!h!e!t!a!U
     ((or !*torsion !*nonmetr) !#!r!o!m!e!g!a)
     ((and (null !*torsion) (null !*nonmetr)) !#!o!m!e!g!a) )
))

(put '!#!t!h!e!t!a!U '!=type 0)
(put '!#!t!h!e!t!a!U '!=way '(
  (nil nil (utheta!>)
     !#!T !#!D !#!G !#!G!I !#!U!U
     ((or !*torsion !*nonmetr) !#!r!o!m!e!g!a)
     ((and (null !*torsion) (null !*nonmetr)) !#!o!m!e!g!a) )
))

% Ideal Fluid ...

(put '!#!P!R!E!S '!=type 0)

(put '!#!E!N!E!R '!=type 0)

(put '!#!T!I!F!L '!=type 0)
(put '!#!T!I!F!L '!=idxl '(nil nil))
(put '!#!T!I!F!L '!=sidxl '((s 1 2)))
(put '!#!T!I!F!L '!=way '(
  (nil nil (tfli!>) !#!G !#!U!S!Q !#!U!U !#!E!N!E!R !#!P!R!E!S )))

% Spin Fluid ...

(put '!#!T!S!F!L '!=type 0)
(put '!#!T!S!F!L '!=idxl '(nil nil))
(put '!#!T!S!F!L '!=sidxl '((s 1 2)))
(put '!#!T!S!F!L '!=way '(
  (nil nil (tsfluid!>)  !#!T !#!D !#!G !#!G!I
                        !#!U!S!Q !#!U!U !#!U!V !#!E!N!E!R !#!P!R!E!S
                        !#!S!P!F!L!T !#!F!C!o !#!o!m!e!g!a )))
(put '!#!T!S!F!L '!=constr '((tttnotn!>)))

(put '!#!S!P!F!L '!=type 2)
(put '!#!S!P!F!L '!=way '(
  ((From Spin Density) nil (spfl!>) !#!S !#!S!P!F!L!T )))

(put '!#!S!P!F!L!T '!=type 0)
(put '!#!S!P!F!L!T '!=idxl '(nil nil))
(put '!#!S!P!F!L!T '!=sidxl '((a 1 2)))
(put '!#!S!P!F!L!T '!=way '(
  ((From Spin Density 2 - form) nil (spflt!>) !#!D !#!S!P!F!L )))

(put '!#!S!P!F!L!U '!=type 3)
(put '!#!S!P!F!L!U '!=idxl '((u . 2)))
(put '!#!S!P!F!L!U '!=way '(
  (nil nil (spflu!>) !#!D !#!T !#!G !#!G!I !#!S!P!F!L!T !#!U!U !#!F!C!o )))
(put '!#!S!P!F!L!U '!=constr '((sp!>)))

(put '!#!F!C!o '!=type 1)
(put '!#!F!C!o '!=way '(
  (nil nil (frenkel!>) !#!U!V !#!S!P!F!L )))


% Total Energy-Momentum and Spin ...

(put '!#!T!E!N!M!O!M '!=type 0)
(put '!#!T!E!N!M!O!M '!=idxl '(nil nil))
(put '!#!T!E!N!M!O!M '!=sidxl '((s 1 2)))
(put '!#!T!E!N!M!O!M '!=way '((nil nil (tenmom!>) )))

(put '!#!T!E!N!M!O!M!T '!=type 0)
(put '!#!T!E!N!M!O!M!T '!=way '(
  (nil nil (tenmomt!>) !#!G!I !#!T!E!N!M!O!M )))

(put '!#!T!E!N!M!O!M!S '!=type 0)
(put '!#!T!E!N!M!O!M!S '!=idxl '((u . 2)(d . 2)))
(put '!#!T!E!N!M!O!M!S '!=sidxl '((h 1 2)))
(put '!#!T!E!N!M!O!M!S '!=way '(
  (nil nil (tenmoms!>) !#!G !#!T!E!N!M!O!M !#!T!E!N!M!O!M!T )))
(put '!#!T!E!N!M!O!M!S '!=constr '((sp!>)))

(put '!#!S!P!I!N!U '!=type  3)
(put '!#!S!P!I!N!U '!=idxl '((u . 2)))
(put '!#!S!P!I!N!U '!=way '((nil nil (spinu!>) )))
(put '!#!S!P!I!N!U '!=constr '((sp!>)))



%------ Macros ---------------------------------------------------------
% Macro Functions. Work like functions.  Recognized by property =MACROS
% its value is the evaluator function. Additional flags:
% +MACROS  - marks corresponding evaluating functions
% +grgmac  - protect external names from additional usage

(flag '(
   ima!> re!> getsoln!>
) '!+macros)

(flag '(
   !I!m !R!e !S!o!l !E!R!R!O!R
) '!+grgmac)

(put '!I!m     '!=macros 'ima!>)
(put '!R!e     '!=macros 're!>)
(put '!S!o!l   '!=macros 'getsoln!>)

%--- Macros 2 and 3 ----------------------------------------------------
% Macro Tensor. Work in expressions like tensors.
% They are flagged by +MACROS2 falg. They have their properties:
%  =type  =idxl - as usual, and in addition properties:
%  =evf - function evaluator for the component
%  =ndl - list of required data

(flag '(
  !#!x !#!X !#!d!i!m !#!s!i!g!n !#!s!g!n!t !#!s!d!i!a!g
  !#!p!m!s!g!n !#!m!p!s!g!n
  !#!h !#!h!i !#!g !#!g!i
  !#!d!e!l !#!d!e!l!h !#!e!p!s !#!e!p!s!i !#!e!p!s!h !#!e!p!s!i!h
  !#!E!P!S !#!E!P!S!I !#!D!E!L !#!s!i!g!m!a !#!s!i!g!m!a!i
  !#!C!H!R !#!C!H!R!F !#!C!H!R!T !#!S!P!C!O!E!F !#!c!c!i
  !#!P!H!I!N!P !#!P!S!I!N!P
  !#!a!l!p!h!a!n!p !#!b!e!t!a!n!p !#!g!a!m!m!a!n!p !#!e!p!s!i!l!o!n!n!p
  !#!k!a!p!p!a!n!p !#!r!h!o!n!p !#!s!i!g!m!a!n!p !#!t!a!u!n!p
  !#!m!u!n!p !#!n!u!n!p !#!l!a!m!b!d!a!n!p !#!p!i!n!p
  !#!D!D !#!D!T !#!d!d !#!d!u
) '!+macros2)

% Coordinates
(put '!#!x '!=type 0)
(put '!#!x '!=idxl '(1))
(put '!#!x '!=evf 'x!>)
(put '!#!X '!=type 0)
(put '!#!X '!=idxl '(1))
(put '!#!X '!=evf 'x!>)

% Conjugate spinorial index
(put '!#!c!c!i '!=type 0)
(put '!#!c!c!i '!=idxl '((n . 3)))
(put '!#!c!c!i '!=evf 'ccin!>)

% Signature
(put '!#!s!d!i!a!g '!=type 0)
(put '!#!s!d!i!a!g '!=idxl '((n)))
(put '!#!s!d!i!a!g '!=evf 'diagonal!>)

% Frame components
(put '!#!h '!=type 0)
(put '!#!h '!=idxl '(t 0))
(put '!#!h '!=ndl '(!#!T))
(put '!#!h '!=evf 'ham!>)
(put '!#!h!i '!=type 0)
(put '!#!h!i '!=idxl '(nil 1))
(put '!#!h!i '!=ndl '(!#!D))
(put '!#!h!i '!=evf 'hiam!>)

% Holonomic metric
(put '!#!g '!=type 0)
(put '!#!g '!=idxl  '(0 0))
(put '!#!g '!=sidxl '((s 1 2)))
(put '!#!g '!=ndl '(!#!G !#!T))
(put '!#!g '!=evf 'gmetr!>)
(put '!#!g '!=tex '!g)
(put '!#!g!i '!=type 0)
(put '!#!g!i '!=idxl  '(1 1))
(put '!#!g!i '!=sidxl '((s 1 2)))
(put '!#!g!i '!=ndl '(!#!G!I !#!D))
(put '!#!g!i '!=evf 'gimetr!>)
(put '!#!g!i '!=tex '!g)

% Delta symbols
(put '!#!d!e!l '!=type 0)
(put '!#!d!e!l '!=idxl '(t nil))
(put '!#!d!e!l '!=evf 'delta!>)
(put '!#!d!e!l '!=tex "\delta")
(put '!#!d!e!l!h '!=type 0)
(put '!#!d!e!l!h '!=idxl '(1 0))
(put '!#!d!e!l!h '!=evf 'delta!>)
(put '!#!d!e!l!h '!=tex "\delta")
(put '!#!D!E!L '!=type 0)
(put '!#!D!E!L '!=idxl '((uu . 1) (u . 1)))
(put '!#!D!E!L '!=evf 'delta!>)
(put '!#!D!E!L '!=tex "\delta")

% Antysymmetric tensors
(put '!#!e!p!s '!=type 0)
(put '!#!e!p!s '!=idxl '(nil nil nil nil))
(put '!#!e!p!s '!=sidxl '((a 1 2 3 4)))
(put '!#!e!p!s '!=ndl '(!#!s!d!e!t!G))
(put '!#!e!p!s '!=evf 'epsilf!>)
(put '!#!e!p!s '!=tex '!E)
(put '!#!e!p!s!i '!=type 0)
(put '!#!e!p!s!i '!=idxl '(t t t t))
(put '!#!e!p!s!i '!=sidxl '((a 1 2 3 4)))
(put '!#!e!p!s!i '!=ndl '(!#!s!d!e!t!G))
(put '!#!e!p!s!i '!=evf 'epsiuf!>)
(put '!#!e!p!s!i '!=tex '!E)
(put '!#!e!p!s!h '!=type 0)
(put '!#!e!p!s!h '!=idxl '(0 0 0 0))
(put '!#!e!p!s!h '!=sidxl '((a 1 2 3 4)))
(put '!#!e!p!s!h '!=ndl '(!#!d!e!t!g))
(put '!#!e!p!s!h '!=evf 'epsilh!>)
(put '!#!e!p!s!h '!=tex '!E)
(put '!#!e!p!s!i!h '!=type 0)
(put '!#!e!p!s!i!h '!=idxl '(1 1 1 1))
(put '!#!e!p!s!i!h '!=sidxl '((a 1 2 3 4)))
(put '!#!e!p!s!i!h '!=ndl '(!#!d!e!t!g))
(put '!#!e!p!s!i!h '!=evf 'epsiuh!>)
(put '!#!e!p!s!i!h '!=tex '!E)

(put '!#!E!P!S '!=type 0)
(put '!#!E!P!S '!=idxl '((u . 1) (u . 1)))
(put '!#!E!P!S '!=sidxl '((a 1 2)))
(put '!#!E!P!S '!=evf 'epss!>)
(put '!#!E!P!S '!=constr '((sp!>)))
(put '!#!E!P!S '!=tex "\epsilon")
(put '!#!E!P!S!I '!=type 0)
(put '!#!E!P!S!I '!=idxl '((uu . 1) (uu . 1)))
(put '!#!E!P!S!I '!=sidxl '((a 1 2)))
(put '!#!E!P!S!I '!=evf 'epss!>)
(put '!#!E!P!S!I '!=constr '((sp!>)))
(put '!#!E!P!S!I '!=tex "\epsilon")

% Sigma matrices
(put '!#!s!i!g!m!a '!=type 0)
(put '!#!s!i!g!m!a '!=idxl '(t (u . 1) (d . 1)))
(put '!#!s!i!g!m!a '!=evf 'sigma!>)
(put '!#!s!i!g!m!a '!=constr '((sp!>)))
(put '!#!s!i!g!m!a '!=tex '"\sigma")
(put '!#!s!i!g!m!a!i '!=type 0)
(put '!#!s!i!g!m!a!i '!=idxl '(nil (uu . 1) (ud . 1)))
(put '!#!s!i!g!m!a!i '!=evf 'sigmai!>)
(put '!#!s!i!g!m!a!i '!=constr '((sp!>)))
(put '!#!s!i!g!m!a!i '!=tex '"\sigma")

% Christoffel symbols
%  of first kind
(put '!#!C!H!R!F '!=type 0)
(put '!#!C!H!R!F '!=idxl '(0 0 0))
(put '!#!C!H!R!F '!=sidxl '((s 2 3)))
(put '!#!C!H!R!F '!=evf 'chrf!>)
(put '!#!C!H!R!F '!=ndl '( !#!G !#!D ))
%  of second kind
(put '!#!C!H!R '!=type 0)
(put '!#!C!H!R '!=idxl '(1 0 0))
(put '!#!C!H!R '!=sidxl '((s 2 3)))
(put '!#!C!H!R '!=evf 'chr!>)
(put '!#!C!H!R '!=ndl '( !#!G !#!D !#!G!I !#!T ))
%  trace
(put '!#!C!H!R!T '!=type 0)
(put '!#!C!H!R!T '!=idxl '(0))
(put '!#!C!H!R!T '!=evf 'chrt!>)
(put '!#!C!H!R!T '!=ndl '( !#!d!e!t!g ))

(put '!#!S!P!C!O!E!F '!=type  0)
(put '!#!S!P!C!O!E!F '!=idxl '((u . 2) nil))
(put '!#!S!P!C!O!E!F '!=constr '((sp!-n!>)))
(put '!#!S!P!C!O!E!F '!=evf 'spcoef!>)
(put '!#!S!P!C!O!E!F '!=ndl '( !#!D !#!o!m!e!g!a!u ))

(put '!#!P!H!I!N!P '!=type 0)
(put '!#!P!H!I!N!P '!=idxl '((u . 2)(d . 2)))
(put '!#!P!H!I!N!P '!=evf 'phinp!>)
(put '!#!P!H!I!N!P '!=ndl '( !#!R!C ))

(put '!#!P!S!I!N!P '!=type 0)
(put '!#!P!S!I!N!P '!=idxl '((u . 4)))
(put '!#!P!S!I!N!P '!=evf 'psinp!>)
(put '!#!P!S!I!N!P '!=ndl '( !#!R!W ))


% Macros 3 extension of Macros 2. For quantities without indices.

(flag '(
  !#!d!i!m !#!s!i!g!n !#!s!g!n!t
  !#!p!m!s!g!n !#!m!p!s!g!n
  !#!a!l!p!h!a!n!p !#!b!e!t!a!n!p !#!g!a!m!m!a!n!p !#!e!p!s!i!l!o!n!n!p
  !#!k!a!p!p!a!n!p !#!r!h!o!n!p !#!s!i!g!m!a!n!p !#!t!a!u!n!p
  !#!m!u!n!p !#!n!u!n!p !#!l!a!m!b!d!a!n!p !#!p!i!n!p
  !#!D!D !#!D!T !#!d!d !#!d!u
) '!+macros3)

(flag '( !d!i!m !s!i!g!n !s!g!n!t ) '!+grgmac)

% Dimension
(put '!#!d!i!m '!=type 0)
(put '!#!d!i!m '!=evf 'dim!>)

% Signature
(put '!#!s!i!g!n '!=type 0)
(put '!#!s!i!g!n '!=evf 'sigprod!>)
(put '!#!s!g!n!t '!=type 0)
(put '!#!s!g!n!t '!=evf 'sigprod!>)
(put '!#!p!m!s!g!n '!=type 0)
(put '!#!p!m!s!g!n '!=evf 'pmsgn!>)
(put '!#!m!p!s!g!n '!=type 0)
(put '!#!m!p!s!g!n '!=evf 'mpsgn!>)

% NP spin coefficients

(put '!#!a!l!p!h!a!n!p      '!=type 0)
(put '!#!b!e!t!a!n!p        '!=type 0)
(put '!#!g!a!m!m!a!n!p      '!=type 0)
(put '!#!e!p!s!i!l!o!n!n!p  '!=type 0)
(put '!#!k!a!p!p!a!n!p      '!=type 0)
(put '!#!r!h!o!n!p          '!=type 0)
(put '!#!s!i!g!m!a!n!p      '!=type 0)
(put '!#!t!a!u!n!p          '!=type 0)
(put '!#!m!u!n!p            '!=type 0)
(put '!#!n!u!n!p            '!=type 0)
(put '!#!l!a!m!b!d!a!n!p    '!=type 0)
(put '!#!p!i!n!p            '!=type 0)

(put '!#!a!l!p!h!a!n!p      '!=evf 'alphanp!>)
(put '!#!b!e!t!a!n!p        '!=evf 'betanp!>)
(put '!#!g!a!m!m!a!n!p      '!=evf 'gammanp!>)
(put '!#!e!p!s!i!l!o!n!n!p  '!=evf 'epsilonnp!>)
(put '!#!k!a!p!p!a!n!p      '!=evf 'kappanp!>)
(put '!#!r!h!o!n!p          '!=evf 'rhonp!>)
(put '!#!s!i!g!m!a!n!p      '!=evf 'sigmanp!>)
(put '!#!t!a!u!n!p          '!=evf 'taunp!>)
(put '!#!m!u!n!p            '!=evf 'munp!>)
(put '!#!n!u!n!p            '!=evf 'nunp!>)
(put '!#!l!a!m!b!d!a!n!p    '!=evf 'lambdanp!>)
(put '!#!p!i!n!p            '!=evf 'pinp!>)

(put '!#!a!l!p!h!a!n!p      '!=ndl '( !#!D !#!o!m!e!g!a!u ))
(put '!#!b!e!t!a!n!p        '!=ndl '( !#!D !#!o!m!e!g!a!u ))
(put '!#!g!a!m!m!a!n!p      '!=ndl '( !#!D !#!o!m!e!g!a!u ))
(put '!#!e!p!s!i!l!o!n!n!p  '!=ndl '( !#!D !#!o!m!e!g!a!u ))
(put '!#!k!a!p!p!a!n!p      '!=ndl '( !#!D !#!o!m!e!g!a!u ))
(put '!#!r!h!o!n!p          '!=ndl '( !#!D !#!o!m!e!g!a!u ))
(put '!#!s!i!g!m!a!n!p      '!=ndl '( !#!D !#!o!m!e!g!a!u ))
(put '!#!t!a!u!n!p          '!=ndl '( !#!D !#!o!m!e!g!a!u ))
(put '!#!m!u!n!p            '!=ndl '( !#!D !#!o!m!e!g!a!u ))
(put '!#!n!u!n!p            '!=ndl '( !#!D !#!o!m!e!g!a!u ))
(put '!#!l!a!m!b!d!a!n!p    '!=ndl '( !#!D !#!o!m!e!g!a!u ))
(put '!#!p!i!n!p            '!=ndl '( !#!D !#!o!m!e!g!a!u ))

(put '!#!D!D '!=type -1)
(put '!#!D!T '!=type -1)
(put '!#!d!d '!=type -1)
(put '!#!d!u '!=type -1)

(put '!#!D!D '!=ndl '( !#!D ))
(put '!#!D!T '!=ndl '( !#!D ))
(put '!#!d!d '!=ndl '( !#!D ))
(put '!#!d!u '!=ndl '( !#!D ))

(put '!#!D!D '!=evf 'dddop!>)
(put '!#!D!T '!=evf 'dtop!>)
(put '!#!d!d '!=evf 'ddop!>)
(put '!#!d!u '!=evf 'duop!>)


%-----------------------------------------------------------------------



%====== (3) Other Internals =============================================

%---------   Properties for Scaner   -----------------------------------

(flag '( !/ !* !_ !~ !< !> !- !| !. ) '!=fc)

%%--------   Properties for Translator   -------------------------------

(put '!*   '!=op2 'times2!>)
(put '!/   '!=op2 'quoti!>)
(put '!/!\ '!=op2 'dfpr2!>)
(put '!_!| '!=op2 'inpr!>)
(put '!|   '!=op2 'vef!>)
(put '!.   '!=op2 'vpr!>)

(flag '(times2!> quoti!> dfpr2!> inpr!> vef!> vpr!>) '!+multop2)

(put '!@ '!=sysfun 'bvec!>)
(put '!# '!=sysfun 'dualis!>)
(put '!d '!=sysfun 'dx!>)
(put '!~ '!=sysfun 'co!>)

(put '!S!u!m     '!=spectr 'sumtr!>)
(put '!P!r!o!d   '!=spectr 'prodtr!>)
(put '!L!i!e     '!=spectr 'lietr!>)
(put '!D!c       '!=spectr 'dctran!>)
(put '!D!f!c     '!=spectr 'dfctran!>)
%(put '!L!i!m     '!=spectr 'limtr!>)
%(put '!L!i!m!M   '!=spectr 'limtrm!>)
%(put '!L!i!m!P   '!=spectr 'limtrp!>)
(put '!L!H!S     '!=spectr 'lhs0!>)
(put '!R!H!S     '!=spectr 'rhs0!>)
(put 'sub        '!=spectr 'subtr!>)
(put '!I!f       '!=spectr 'iftran!>)
%(put '!D!f       '!=spectr 'pdftra!>)
%(put '!D!f!p     '!=spectr 'dfptra!>)
(put '!E!R!R!O!R '!=spectr 'errortr!>)

(flag '(
  funapply!> sumexec!> prodexec!> lhs!> rhs!> dummyvar!>
  subexec!> ifexec!> lieexec!> dcexec!> dfcexec!> error!>
  % limexec!>
) '!+specexec)

%-------   Boollean Expressions   --------------------------------------

(put '!O!B!J!E!C!T      '!=boolmac 'objexe!>)
(put '!O!N              '!=boolmac 'onexe!>)
(put '!O!F!F            '!=boolmac 'offexe!>)
(put '!Z!E!R!O          '!=boolmac 'zeroexe!>)
(put '!H!A!S!V!A!L!U!E  '!=boolmac 'valexe!>)
(put '!N!U!L!L!M        '!=boolmac 'nullexe!>)

(flag '(objexe!> onexe!> offexe!> zeroexe!> valexe!> nullexe!>)
      '!+specbexe)

%-------   Flags and properties which must be cleared   ----------------

(setq ![rpfl!] '(

  ((![const!]) (!+grg !+grgvar used!* constant)
               !=conj )

  ((![fun!])   ( !+grg subfn !+grgvar !+fun used!*
                  symmetric antisymmetric odd even)
               simpfn kvalue klist narg !=conj !=depend)

  ((![cord!])  (!+grg !+grgvar used!*)
               !=cord !=conj)

  ((![ocord!]) (!+grg !+grgvar used!*)
               !=cord !=conj)

  ((![apar!])  (!+grg !+grgvar used!*) )

))

(setq ![rpflcr!] '(
  ((![cord!]) (!+grg !+grgvar used!*) !=cord !=conj)
))

(setq ![rpflcn!] '(
  ((![const!]) (!+grg !+grgvar used!* constant) !=conj)
))

(setq ![rpflap!] '(
  ((![apar!]) (!+grg !+grgvar used!* constant) )
))

(setq ![rpflfu!] '(
  ((![fun!]) (  !+grg subfn !+grgvar !+fun used!*
                symmetric antisymmetric odd even dfp!_commute )
	     subfunc generic!_function
             simpfn kvalue klist narg !=conj !=depend)
))

%-------  List of Flags and Props important for Load/Unload ------

(setq ![allflags!]
  '( !+equ !+pl !+ivar !+abbr !+noncov !+hconn !+fconn !+uconn !+dconn ))

(setq ![allprops!]
  '( !=type !=idxl !=sidxl !=constr !=tex !=dens ))


%-------  Commands  ----------------------------------------------

% word!!!

(setq ![instr!] '(
   (On        !!!!  onoff!> t)
   (Off       !!!!  onoff!> nil)
   (Quit      !!    grgquit!>)
   (System    !!!!  grgsystem!>)
   (Stop      !!    stop!>)
   (Find      !!!!  find!>)
%   (Calculate !!!!  find!>)
   (Write     !!!!  write!>)
   (Zero      !!!!  zero!>)
%   (Nullify   !!!!  zero!>)
   (Print     !!!!  printi!>)
   (Evaluate  !!!!  evalcomm!> (function evel!>))
%   (Simplify  !!!!  evalcomm!> (function evel!>))
   (Erase     !!!!  erase!>)
%   (Delete    !!!!  erase!>)
   (Let       !!!!  leti!> t)
   (Match     !!!!  matchi!> t)
   (Clear     !!!!  cleri!> t)
   (For       !!!!  forinstrs!>)
   (Input     !!!!  from!>)
   (Dimension !!!!  dimension!>)
   (!%        !!!!  comment!>)
   (Comment   !!!!  comment!>)
   (File      !!!!  showfil!>)
   (Factor    !!!!  orfare!> 'factor)
   (RemFac    !!!!  orfare!> 'remfac)
   (Order     !!!!  orfare!> 'order)
   (Holonomic   !!    turnbg!> nil)
   (Anholonomic !!    turnbg!>   t)
   (Show      !!!!  shcommands!>)
   (ds2       !!    showlinel!>)
   (Time      !!    timei!>)
   (GC
      (Time   !!    gctime!>))
   (Switch    !!!!  sflag!>)
   (Status    !!    shstatus!>)
   (Load      !!!!  loa!>)
%   (Restore   !!!!  loa!>)
   (Unload    !!!!  unl!>)
%   (Save      !!!!  unl!>)
   (Next      !!    next!>)
   (Pause     !!    pause!>)
   (Normalize !!!!  evalcomm!> (function normel!>))
   (Classify  !!!!  classify!>)
   (Output    !!!!  grgout!>)
   (Symmetric !!!!  funsym!> 0)
   (Antisymmetric
              !!!!  funsym!> 1)
   (Odd       !!!!  funsym!> 2)
   (Even      !!!!  funsym!> 3)
   (Coordinates
              !!!!  datrc!> '![cord!] ![dim!])
   (Constants !!!!  datrc!> '![const!] nil)
   (Functions !!!!  fun!>)
   (Generic
     (Functions
              !!!!  genfun!>))
   (New       !!!!  newcommands!>)
   (Object    !!!!  obdec!> 0)
   (Equation  !!!!  obdec!> 1)
   (Connection
              !!!!  obdec!> 2)
   (Line
     (!-
       (Element
              !!    showlinel!>))
     (Length  !!!!  setlinel!>) )
   (Make
     (Spinorial
       (Rotation
              !!!!  rotas!>))
     (Rotation
              !!!!  rotat!> nil))
   (Spinorial
     (Rotation
              !!!!  rotas!>))
   (Rotation  !!!!  rotat!> nil)
   (Change
     (Metric  !!!!  rotat!> t))
   (Forget    !!!!  forget!>)
   (Solve     !!!!  solvei!>)
   (EndO      !!    closewrite!>)
   (EndW      !!    closewrite!>)
   (EndU      !!    closeunload!>)
   (End (of
     (Output  !!    closewrite!>)
     (Write   !!    closewrite!>)
     (Unload  !!    closeunload!>) ))
   (Inverse   !!!!  invi!>)
   (Null
     (Metric  !!    nullmetric!>))
   (Package   !!!!  loadpack!> t)
   (Hold      !!!!  hold!> t)
   (Release   !!!!  hold!> nil)
   (Affine
     (Parameter
	      !!!!  affpar!>))
   (copyright !!    copyrzw!>)
   (lisp      !!    lisp!>)
   (debug     !!    otladka!>)
))

%-------  Commands allowed as composites  ------------------------------

% word!!!

(setq ![icompos!] '(
  Find Write Calculate Nullify Zero Save Unload Forget
  Erase Delete Simplify Evaluate Normalize Hold Release
))

%-------  Unlocked Commands when coordinates are undefined  ------------

(flag '(
  onoff!> grgquit!> stop!> pause!> next!> from!> showfil!>
  erase!> zero!> comment!> timei!> datrc!> fun!> copyrzw!>
  loa!> sflag!> shobj!> obdec!> gctime!> shstatus!> shall!>
  forget!> grgsystem!> grgout!> setlinel!> hold!>
  lisp!> loadpack!> closewrite!> closeunload!>
  shcommands!> dimension!> otladka!>
) '!+unloc)

%-------  Reserved Variables  ------------------------------------------

(setq ![rconstl!] '(
  e i pi infinity failed
  !E!C!O!N!S!T !D!M!A!S!S !S!M!A!S!S !G!C!O!N!S!T !C!C!O!N!S!T
  !L!C0 !L!C1 !L!C2 !L!C3 !L!C4 !L!C5 !L!C6
  !A!C0 !M!C1 !M!C2 !M!C3
))

(operator '(arbcomplex!~))

(put 'arbcomplex   '!=conj  'arbcomplex!~)
(put 'arbcomplex!~ '!=conj  'arbcomplex)

% These can not be used in new declarations ...
(flag '( df nil sub
         !d !L!H!S !R!H!S !S!u!m !P!r!o!d
         !L!i!e !D!c !I!f !D!f !D!f!p !E!R!R!O!R
         % !L!i!m !L!i!m!M !L!i!m!P
       )  '!+grg)

(flag ![rconstl!] '!+grg)
(flag ![rconstl!] 'used!*)
(flag ![rconstl!] '!+grgvar)
(flag ![rconstl!] 'constant)


%----  Specially Prohibiting the usage of some symbols in GRG  ---------

(flag '( conj repart impart fix floor round interpol
         ceiling set ws evenp list factorize ) '!+redbad )

%-------   GRG Switches   ----------------------------------------------

% GRG switches :
(global '(
  !*aeval          % If On REVAL(AEVAL()) else REVAL()       (Off)
  !*wrs            % Evaluate expression before Write if On  (On)
  !*wmatr          % Print 2-index scalars as matrices       (Off)
  !*torsion        % Torsion                                 (Off)
  !*nonmetr        % Nonmetricity                            (Off)
  !*unlcord        % Saves coordinates in Save/Unload        (On)
  !*auto           % Automatical data calculation in expr    (On)
  !*trace          % Tracing evaluation process              (On)
  !*showcommands   % Show composite commands expansion       (Off)
  !*expandsym      % Sym Asym and othre in expr              (Off)
  !*dfpcommute     % Commutativity ofr DFP                   (On)
  !*nonmin         % Nonminimal Interaction                  (Off)
  !*nofreevars     % Prohibites free vars. in Print command  (Off)
  !*cconst         % Include cosm.-const. in equation or not (Off)
  !*full           % Control number of components in Metr.Eq.(Off)
  !*latex          %  O
  !*grg            %  u
  !*reduce         %  t
  !*maple          %  p
  !*math           %  u
  !*macsyma        %  t
  !*dfindexed      % DF in indexed form                      (Off)
  !*batch          % Batch mode                              (Off)
  !*holonomic      % Keeps farme holonomic during cord.      (On)
                   % and frame transformations
  !*showexpr       % If On then values of nonzero expr is    (Off)
         	   % shown in the process of classification
))
% oftenly this is already fluid
(cond ((not (or (fluidp '!*debug) (globalp '!*debug)))
  (global '(
  !*debug          % Otladka
))))

(setq ![flagl!] '(
  aeval          % If On REVAL(AEVAL()) else REVAL()       (Off)
  wrs            % Evaluate expression before Write if On  (On)
  wmatr          % Print 2-index scalars as matrices       (Off)
  torsion        % Torsion                                 (Off)
  nonmetr        % Nonmetricity                            (Off)
  unlcord        % Saves coordinates in Save/Unload        (On)
  auto           % Automatical data calculation in expr    (On)
  trace          % Tracing evaluation process              (On)
  showcommands   % Show composite commands expansion       (Off)
  expandsym      % Sym Asym and othre in expr              (Off)
  dfpcommute     % Commutativity ofr DFP                   (On)
  nonmin         % Nonminimal Interaction                  (Off)
  nofreevars     % Prohibites free vars. in Print command  (Off)
  cconst         % Include cosm.-const. in equation or not (Off)
  full           % Control number of components in Metr.Eq.(Off)
  latex          %  O
  grg            %  u
  reduce         %  t
  maple          %  p
  math           %  u
  macsyma        %  t
  dfindexed      % DF in indexed form                      (Off)
  batch          % Batch mode                              (Off)
  holonomic      % Keeps farme holonomic during cord.      (On)
                 % and frame transformations
  showexpr       % If On then values of nonzero expr is    (Off)
          	 % shown in the process of classification
  debug          % Otladka                                 (Off)
))

(flag ![flagl!] '!+switch)

% Set these initially to Off position ...
(setq ![flagnil!]
   '( !*torsion !*nonmetr !*gc !*echo !*batch !*showcommands
      !*expandsym !*dfindexed !*aeval !*wmatr !*showexpr
      !*nonmin !*nofreevars !*cconst !*full
      !*debug ))

% Set these initially to On position ...
(setq ![flagt!]
   '( !*unlcord !*wrs !*trace !*auto !*holonomic
      !*dfpcommute ))

% Output switches ...
(setq ![flaglo!] '(
  grg reduce maple math macsyma
))

% Switches tuning ...
(put 'torsion   '!=tuning 'tunetorsion!>)
(put 'nonmetr   '!=tuning 'tunenonmetr!>)
(put 'fancy     '!=tuning 'tunefancy!>)
(put 'latex     '!=tuning 'tunetex!>)
(put 'grg       '!=tuning 'tunegrg!>)
(put 'reduce    '!=tuning 'tunereduce!>)
(put 'maple     '!=tuning 'tunemaple!>)
(put 'math      '!=tuning 'tunemath!>)
(put 'macsyma   '!=tuning 'tunemacsyma!>)
(put 'dfindexed '!=tuning 'tunedfindexed!>)
(put 'debug     '!=tuning 'swotladka!>)


%----------   Special Treatment for Write   ----------------------------

(put '![cord!]  '!=datl '((datlc!> ![cord!]  "Coordinates" t)))
(put '![const!] '!=datl '((datlc!> ![const!] "Constants"   t)))
(put '![apar!]  '!=datl '((datlc!> ![apar!]  "Affine Parameter"  nil)))
(put '![fun!]   '!=datl '((funl!>)))
(put '![sol!]   '!=datl '((solwri!>)))

%----------   Special Actions For Load/Unload   ------------------------

(put '![cord!]  '!=unl
  '((putpnu!> nil ![cord!] (used!* !+grgvar !+grg) !=cord 1)))

(put '![const!] '!=unl
  '((putpnu!> nil ![const!] (used!* !+grgvar !+grg) nil 2)))

(put '![apar!] '!=unl
  '((putpnu!> nil ![apar!] (used!* !+grgvar !+grg) nil 4)))

(put '![fun!]   '!=unl
  '((putpnu!> (putfndp!>) ![fun!] (used!* !+fun !+grg) nil 3)))

%----------   Standard Null Metric   -----------------------------------

% Signature (-,+,+,+)
(setq ![nullm!] '( ( nil -1  nil nil )
                   ( nil nil nil nil )
                   ( nil nil nil 1   )
                   ( nil nil nil nil ) ))

% Signature (+,-,-,-)
(setq ![nullm1!] '( ( nil 1   nil nil )
                    ( nil nil nil nil )
                    ( nil nil nil -1  )
                    ( nil nil nil nil ) ))

%----------   For Nice Printing   --------------------------------------

(flag '( !. !, !; !_ !/!\ !* !** !+ !- !_!| ![ !:!\!  !=
         !#!  !]  !^ !/  !' !.!. !'!' !# !| !@ !> !<  !~
         !>!= !<!= !:
) '!+nonsp)

(put 'em  '!=printas '!E!M)
(put 'ym  '!=printas '!Y!M)

%----------   Indices Manipulations   ----------------------------------

(flag '( !' !^ !. !_ ) '!+indexman)

%-----------------------------------------------------------------------

(put '!a '!=uc '!A) (put '!A '!=lc '!a)
(put '!b '!=uc '!B) (put '!B '!=lc '!b)
(put '!c '!=uc '!C) (put '!C '!=lc '!c)
(put '!d '!=uc '!D) (put '!D '!=lc '!d)
(put '!e '!=uc '!E) (put '!E '!=lc '!e)
(put '!f '!=uc '!F) (put '!F '!=lc '!f)
(put '!g '!=uc '!G) (put '!G '!=lc '!g)
(put '!h '!=uc '!H) (put '!H '!=lc '!h)
(put '!i '!=uc '!I) (put '!I '!=lc '!i)
(put '!j '!=uc '!J) (put '!J '!=lc '!j)
(put '!k '!=uc '!K) (put '!K '!=lc '!k)
(put '!l '!=uc '!L) (put '!L '!=lc '!l)
(put '!m '!=uc '!M) (put '!M '!=lc '!m)
(put '!n '!=uc '!N) (put '!N '!=lc '!n)
(put '!o '!=uc '!O) (put '!O '!=lc '!o)
(put '!p '!=uc '!P) (put '!P '!=lc '!p)
(put '!q '!=uc '!Q) (put '!Q '!=lc '!q)
(put '!r '!=uc '!R) (put '!R '!=lc '!r)
(put '!s '!=uc '!S) (put '!S '!=lc '!s)
(put '!t '!=uc '!T) (put '!T '!=lc '!t)
(put '!u '!=uc '!U) (put '!U '!=lc '!u)
(put '!v '!=uc '!V) (put '!V '!=lc '!v)
(put '!w '!=uc '!W) (put '!W '!=lc '!w)
(put '!x '!=uc '!X) (put '!X '!=lc '!x)
(put '!y '!=uc '!Y) (put '!Y '!=lc '!y)
(put '!z '!=uc '!Z) (put '!Z '!=lc '!z)

%------- Trigonometric Functions ---------------------------------------

(flag '(  sin    cos    tan    cot    sec    csc
          sinh   cosh   tanh   coth   sech   csch
          asin   acos   atan   acot   asec   acsc
          asinh  acosh  atanh  acoth  asech  acsch ) '!+trig)

%============ End of GRGdecl.sl =========================================%

