(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)

BeginPackage["CTS`WaveVectors`"];

PlasmaRefractiveIndexN::usage = "PlasmaRefractiveIndexN[W, theta, ModeType, PlasmaParams_Association] is a refractive index N for an electromagnetic wave with frequency W and polarization ModeType, propagating in a uniform magnetized plasma PlasmaParams with angle theta to the local B-field.";

ScatteringWaveVector::usage = "ScatteringWaveVector[DiagnosticParams_Association, PlasmaParams_Association] is a scattering wave vector in a uniform magnetized plasma in the scattering geometry DiagnosticParams.";


Begin["`Private`"];

Needs[           "CTS`Constants`"];
Needs["CTS`ParamsInitialisation`"];


PolarizationLambda["O"] =  1; (*     Ordinary wave*)
PolarizationLambda["X"] = -1; (*Extraordinary wave*)


PlasmaRefractiveIndexN[W_, theta_, ModeType_, PlasmaParams_Association] := Module[{ u, v, gamma, lambda},

u = (PlasmaParams["Wce"])^2 / W^2;
v = (PlasmaParams["Wpe"])^2 / W^2;

 gamma = (u Sin[theta]^2)^2 + 4 u (1 - v)^2 Cos[theta]^2;
lambda = PolarizationLambda[ModeType];

(*Return*)
Sqrt[1 - (2 v (1 - v)/ (2 (1 - v) - u Sin[theta]^2 + lambda Sqrt[gamma]))]
];


ScatteringWaveVector[DiagnosticParams_Association, PlasmaParams_Association] := Module[{k2, k1, kp2, kp1, ks2, ks1, dkp, dks},


k1 = ( DiagnosticParams["WDiag"] / SpeedOfLightC ) * PlasmaRefractiveIndexN[DiagnosticParams["WDiag"], DiagnosticParams["alphaIn"], DiagnosticParams["ModeIn"], PlasmaParams]; 
k2 = ( DiagnosticParams["WDiag"] / SpeedOfLightC ) * PlasmaRefractiveIndexN[DiagnosticParams["WDiag"], DiagnosticParams["alphaSc"], DiagnosticParams["ModeSc"], PlasmaParams]; 


kp1 = k1 Cos[DiagnosticParams["alphaIn"]]; kp2 = k2 Cos[DiagnosticParams["alphaSc"]];
ks1 = k1 Sin[DiagnosticParams["alphaIn"]]; ks2 = k2 Sin[DiagnosticParams["alphaSc"]];


dks = Sqrt[ks2 ^ 2 + ks1 ^ 2 - 2 ks2 ks1 Cos[DiagnosticParams["phi"]]];
dkp = kp2 - kp1;


(*Return*)
<| "k" -> Sqrt[ dks ^ 2 + dkp ^ 2], (*norm of the scattering wave vector*)
  "kp" -> dkp, (*the parallel to the local B-field component*)
  "ks" -> dks  (*the perpendicular to the local B-field component*)
|>
];


End[];

EndPackage[];
