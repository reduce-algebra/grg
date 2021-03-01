%==========================================================================%
%  GRG 3.2 Global Configuration File       (C) 1988-96 Vadim V. Zhytnikov  %
%==========================================================================%

% Default Dimensionality and Signature.
% You can modify this line but newer remove it!
(signature!> - + + + )

% Uncomment the line below if one need to start GRG
% manually using two commands
%   load grg;
%   grg;
% instead of default (causes trouble on some systems)
%   load grg;
%(setq ![autostart!] nil)

% Changing the default on/off switch position:
%(on!> page)

% Pre-loading the packages:
%(package!> specfn)

% Command synonymy:
(synonymous!>
  ( Affine Aff                             )
  ( Anholonomic Nonholonomic AMode ABasis  )
  ( Antisymmetric Asy                      )
  ( Change Transform                       )
  ( Classify Class                         )
  ( Components Comp                        )
  ( Connection Con                         )
  ( Constants Const Constant               )
  ( Coordinates Cord                       )
  ( Curvature Cur                          )
  ( Dimension Dim                          )
  ( Dotted Do                              )
  ( Equation Equations Eq                  )
  ( Erase Delete Del                       )
  ( Evaluate Eval Simplify                 )
  ( Find F Calculate Calc                  )
  ( Form Forms                             )
  ( Functions Fun Function                 )
  ( Generic Gen                            )
  ( Gravitational Gravity Gravitation Grav )
  ( Holonomic HMode HBasis                 )
  ( Inverse Inv                            )
  ( Load Restore                           )
  ( Next N                                 )
  ( Normalize Normal                       )
  ( Object Obj                             )
  ( Output Out                             )
  ( Parameter Par                          )
  ( Rotation Rot                           )
  ( Scalar Scal                            )
  ( Show ?                                 )
  ( Signature Sig                          )
  ( Solutions Solution Sol                 )
  ( Spinor Spin Spinorial Sp               )
  ( standardlisp lisp                      )
  ( Switch Sw                              )
  ( Symmetries Sym Symmetric               )
  ( Tensor Tensors Tens                    )
  ( Torsion Tors                           )
  ( Transformation Trans                   )
  ( Undotted Un                            )
  ( Unload Save                            )
  ( Vector Vec                             )
  ( Write W                                )
  ( Zero Nullify                           )
)

%======= End of GRGcfg.sl =================================================%

