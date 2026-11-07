(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)


BeginPackage["CTS`Electrons`Thermal`"];


ThermalElectronsParamsInitialization::usage = "ThermalElectronsParamsInitialization[PlasmaParams_Association, N] initializes the parametrs of the thermal electrons.";


Begin["`Private`"];


r := Compile[{x, N}, {

r[[N+2]] = 1;
For[i = N + 1, i > -1, i--, r[[i]] = 1 / (r[[i+1]] + 2 i / x) ];
(*Return*) r
}
, CompilationTarget->"C"]


SpectralDensityS0[ElectronsThermalParams_Association, PlasmaParams_Association, K_, W_] := 0;


SusceptibilityH[ElectronsThermalParams_Association, PlasmaParams_Association, K_, W_] := 0;


ThermalElectronsParamsInitialization[PlasmaParams_Association, N_] := 

<|
"SpectralDensityS0" -> SpectralDensityS0,
  "SusceptibilityH" -> SusceptibilityH
|>;


End[];


EndPackage[];
