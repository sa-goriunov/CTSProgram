(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)


BeginPackage["CTS`Electrons`Thermal`"];


ThermalElectronsParamsInitialization::usage = "ThermalElectronsParamsInitialization[] initializes the parametrs of the thermal electrons.";


Begin["`Private`"];


Needs["CTS`Constants`"];
Needs["CTS`PlasmaDispersionFunction`"]


r = Compile[ {{x,_Real},{n,_Integer}}, Module[ {r},
r = Table[ 1.0, {n+2} ];
Do[ r[[i+1]] = 1 / ( r[[i+2]] + 2 i / x ), { i, n, 0, -1} ];
(*Return*) r
]
, CompilationTarget->"C" ];


B0 = Compile[ {{r,_Real,1},{n,_Integer}}, Module[ {B},
B = Table[ 0.0, {n+1} ];
Do[ B[[i+1]] = r[[i+2]] ( 1 + B[[i+2]] ), { i, n-1 , 0, -1} ];
(*Return*)B[[1]]
]
, CompilationTarget->"C" ];


y[w_, Wc_, Vt_, kp_, n_] := Table[ (w - i Wc)/(Sqrt[2] kp Vt), {i, -n, n}];


S0[r_List, y_, f_, n_ ] := Last[ FoldList[ 
                                          r[[#2 + 2]] (#1 + f[y[[ n + 2 + #2]]] + f[y[[n - #2]]]) &, 
                                      0, Range[ n-1, 0, -1]]
];


(*SExp0 = Compile[{{x, _Real}, {y, _Real, 2}, {r, _Real, 1}, { B0, _Real}, {n, _Integer}},
Module[ { S, S0 },

(*y = Table[(w[[i]] - l Wce)/x, {l, -n+1, n-1}, {i, 1, Length[w]}];*)

S = Table[0.0, {n}, {i, 1, n}];
Do[ Do[ S[[i + 1, j]] = r[[i + 2]] (S[[i + 2, j]] + Exp[- y[[n + i + 1, j]]^2] + Exp[- y[[n - i - 1, j]]^2]),{i, n - 2, 0, -1}], {j, 1, Dimensions[y][[2]]}];

(*Return*) S0 = Table[S[[1, j]], {j, 1, Dimensions[y][[2]]}]

],
CompilationTarget->"C", Parallelization->True
];*)


SpectralDensityS0[ElectronsThermalParams_Association, PlasmaParams_Association, K_, W_, delta_] := Module[{ L, Y, R, b0 }, 

L = Ceiling[
        (Sqrt[2 BoltzmannK PlasmaParams["Te"] K["kp"]^2 / ElectronMass] InverseErfc[Min[1, delta * PlasmaParams["Wce"] Pi^(-3/2) / 2 ]]) 
         / PlasmaParams["Wce"] 
    ];

 R = r[(K["ks"] PlasmaParams["Vte"])^2 / 2, L];
b0 = B0[R, L];

Y = y[#, PlasmaParams["Wce"], PlasmaParams["Vte"], K["kp"], L] & /@ W;


Table[ Sqrt[2 Pi] / (PlasmaParams["Vte"] Abs[K["kp"]]) ( S0[R, Y[[i]], Exp[-#^2] &, L] + Exp[-Y[[i, L + 1]]^2])/(1 + 2 b0), {i, 1, Length[W]}]

];


SusceptibilityH[ElectronsThermalParams_Association, PlasmaParams_Association, K_, W_, delta_] := 0;


ThermalElectronsParamsInitialization[] := 

<|
   "SpectralDensityS0" -> SpectralDensityS0,
     "SusceptibilityH" -> SusceptibilityH
|>;


End[];


EndPackage[];
