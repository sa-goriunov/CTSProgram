(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)


BeginPackage["CTS`Electrons`Thermal`"];


ThermalElectronsParamsInitialization::usage = "ThermalElectronsParamsInitialization[] initializes the parametrs of the thermal electrons.";


Begin["`Private`"];


Needs["CTS`Constants`"];
Needs["CTS`PlasmaDispersionFunction`"]


r[x_, n_] := Reverse[ FoldList[ 1/(#1 + 2 #2 / x) &, 1, Range[n, 0, -1]] ];


B0[r_List, n_] := Last[ FoldList[ r[[#2 + 2]]( #1 + 1 ) &, 0, Range[n, 0, -1]] ];


y[w_, Wc_, Vt_, kp_, n_] := Table[ (w - i Wc)/(Sqrt[2] kp Vt), {i, -n, n}];


S0[r_List, y_, f_, n_ ] := Last[ FoldList[ 
                                          r[[#2 + 2]] (#1 + f[y[[ n + 2 + #2]]] + f[y[[n - #2]]]) &, 
                                      0, Range[ n-1, 0, -1]]
];


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
