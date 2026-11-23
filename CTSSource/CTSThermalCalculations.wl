(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)


BeginPackage["CTS`ThermalCalculations`"];


    ThermalCalculationsR::usage = "ThermalCalculationsR[x, n] is a subfunction for calculations for thermal fractions.";
   ThermalCalculationsB0::usage = "ThermalCalculationsB0[r_List, n] is a subfunction for calculations for thermal fractions.";
    ThermalCalculationsY::usage = "ThermalCalculationsY[W_List, Wc_, Vt_, kp_, n_] is a subfunction for calculations for thermal fractions.";
ThermalCalculationsS0Exp::usage = "ThermalCalculationsS0Exp[y_List, r_List, n_] is a subfunction for calculations for thermal fractions.";
  ThermalCalculationsS0Z::usage = "ThermalCalculationsS0Z[y_List, r_List, n_] is a subfunction for calculations for thermal fractions.";


Begin["`Private`"];


Needs["CTS`PlasmaDispersionFunction`"];


(*
The method is described in:
Forward Modelling of Collective Thomson Scattering for Wendelstein 7-X Plasmas: Electrostatic Approximation, the W7-X team, Review of Scientific Instruments 90, 023501 (2019)
*)


ThermalCalculationsR = Compile[ { { x, _Real }, { n, _Integer } }, Module[ { r },
r = Table[ 1.0, {n+2} ];
Do[ r[[i]] = 1 / ( r[[i+1]] + 2 i / x ), { i, n + 1, 1, -1} ];
(*Return*) r
]
, CompilationTarget->"C" ];


ThermalCalculationsB0 = Compile[ { { r, _Real, 1 }, { n, _Integer } }, Module[ { B },
B = 0.0;
Do[ B = r[[i]] ( 1 + B ), { i, n+1, 1, -1} ];
(*Return*) B
]
, CompilationTarget->"C" ];


ThermalCalculationsY[W_List, Wc_, Vt_, kp_, n_] := Table[ (W[[j]] - i Wc)/(Sqrt[2] kp Vt), {i, -n, n}, {j, 1, Length[W]}];


ThermalCalculationsS0Exp = Compile[ { { y, _Real, 2 }, { r, _Real, 1 }, { n, _Integer } }, Module[ { S },
S = Table[ 0.0,{i, Dimensions[y][[2]]} ];
Do[ S = r[[i]] ( S + Exp[- y[[n + i + 1]]^2] + Exp[- y[[n - i + 1]]^2]) , { i, n, 1, -1}];
(*Return*) S
]
, CompilationTarget->"C" ];


ThermalCalculationsS0Z[y_List, r_List, n_ ] := Module[ { Z },

Z = PlasmaDispersionFunctionZ[y];

Fold[ r[[#2]] (#1 + Z[[n + #2 + 1]] + Z[[n - #2 + 1]]) &, 0, Reverse[Range[1, n]]]
];


End[];


EndPackage[];
