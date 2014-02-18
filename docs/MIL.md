% Monadic Intermediate Language
% Dmytro Lypai (900620-7113, lypai@student.chalmers.se)

Syntax
======

---------------  ------------------
        `t ::=`  `x`

                 `\x : T -> t`

                 `t t`

                 `/\X :: K . t`

                 `t [T]`

        `v ::=`  `\x : T -> t`

                 `/\X :: K . t`

        `T ::=`  `X`

                 `T -> T`

                 `forall X :: K . T`

                 `/\X :: K . T`

                 `T T`

 $\Gamma$ `::=`  $\varnothing$

                 $\Gamma$`, x : T`

                 $\Gamma$`, X :: K`

        `K ::=`  `*`

                 `K => K`
---------------  ------------------

Typing
======

\infrule[T-Var]{x : T \in \Gamma}{\Gamma \vdash x : T}

\infrule[T-Abs]{\Gamma \vdash T_1 :: * \andalso \Gamma, x : T_1 \vdash t_2 : T_2}{\Gamma \vdash \lambda x : T_1 \to t_2 : T_1 \to T_2}

\infrule[T-App]{\Gamma \vdash t_1 : T_{11} \to T_{12} \andalso \Gamma \vdash t_2 : T_{11}}{\Gamma \vdash t_1\ t_2 : T_{12}}

\infrule[T-TAbs]{\Gamma, X :: K_1 \vdash t_2 : T_2}{\Gamma \vdash \Lambda X :: K_1\ .\ t_2 : forall\ X :: K_1\ .\ T_2}

\infrule[T-TApp]{\Gamma \vdash t_1 : forall\ X :: K_{11}\ .\ T_{12} \andalso \Gamma \vdash T_2 :: K_{11}}{\Gamma \vdash t_1\ [T_2] : [X \mapsto T_2]T_{12}}

\infrule[T-Eq]{\Gamma \vdash t : S \andalso S \equiv T \andalso \Gamma \vdash T :: *}{\Gamma \vdash t : T}

Kinding
=======

\infrule[K-TVar]{X :: K \in \Gamma}{\Gamma \vdash X :: K}

\infrule[K-Abs]{\Gamma, X :: K_1 \vdash T_2 :: K_2}{\Gamma \vdash \Lambda X :: K_1\ .\ T_2 :: K_1 \Rightarrow K_2}

\infrule[K-App]{\Gamma \vdash T_1 :: K_{11} \Rightarrow K_{12} \andalso \Gamma \vdash T_2 :: K_{11}}{\Gamma \vdash T_1\ T_2 :: K_{12}}

\infrule[K-Arrow]{\Gamma \vdash T_1 :: * \andalso \Gamma \vdash T_2 :: *}{\Gamma \vdash T_1 \to T_2 :: *}

\infrule[K-All]{\Gamma, X :: K_1 \vdash T_2 :: *}{\Gamma \vdash forall\ X :: K_1\ .\ T_2 :: *}

Type equivalence (parallel reduction)
=====================================

\infax[QR-Refl]{T \Rrightarrow T}

\infrule[QR-Arrow]{S_1 \Rrightarrow T_1 \andalso S_2 \Rrightarrow T_2}{S_1 \to S_2 \Rrightarrow T_1 \to T_2}

\infrule[QR-All]{S_2 \Rrightarrow T_2}{forall\ X :: K_1\ .\ S_2 \Rrightarrow forall\ X :: K_1\ .\ T_2}

\infrule[QR-Abs]{S_2 \Rrightarrow T_2}{\Lambda X :: K_1\ .\ S_2 \Rrightarrow \Lambda X :: K_1\ .\ T_2}

\infrule[QR-App]{S_1 \Rrightarrow T_1 \andalso S_2 \Rrightarrow T_2}{S_1\ S_2 \Rrightarrow T_1\ T_2}

\infrule[QR-AppAbs]{S_{12} \Rrightarrow T_{12} \andalso S_2 \Rrightarrow T_2}{(\Lambda X :: K_{11}\ .\ S_{12})\ S_2 \Rrightarrow [X \mapsto T_2]T_{12}}

