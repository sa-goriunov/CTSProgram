(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)

BeginPackage["CTS`PlasmaFunctions`"];

PlasmaDispersionFunctionZ::usage = "PlasmaDispersionFunctionZ[x] calculates the plasma dispersion function with real argument x.";

ColdPlasmaDielectricTensorE::usage = "ColdPlasmaDielectricTensorE[PlasmaParams_Association, W_] calculates the dielectric tensor for cold plasma PlasmaParams for wave with frequency W. Axe Z is parallel with the local B-field.";


Begin["`Private`"];


PlasmaDispersionFunctionZ[x_] := If[ Abs[x] <= 25, 
                                     Sqrt[Pi] (I - Erfi[x]) Exp[- x^2], 
                                 (*else*) 
                                     I Sqrt[Pi] Exp[- x^2] - 1 / x - 1 / (2 x^3)
                                 ];



ColdPlasmaDielectricTensorE[PlasmaParams_Association, W_] := Module[{ue, ve, ui, vi, 
                                                                          e1, e2, e3},


ue = (PlasmaParams["Wce"])^2 / W^2;
ve = (PlasmaParams["Wpe"])^2 / W^2;



ui = ((#["Wci"])^2 / W^2) & /@ PlasmaParams["Ions"];
vi = ((#["Wpi"])^2 / W^2) & /@ PlasmaParams["Ions"];


e1 =           ( ve / (1 - ue)) + Total[ vi / (1 - ui)];
e2 = ( ve Sqrt[ue] / (1 - ue) ) + Total[ vi Sqrt[ui] / (1 - ui) ];
e3 =                          ve + Total[ vi ];


(*Return*)
IdentityMatrix[3] - {{e1, -I e2, 0}, {I e2, e1, 0}, {0, 0, e3}}
];


End[];

EndPackage[];
