(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)


BeginPackage["CTS`Ions`Thermal`"];


ThermalIonsParamsInitialization::usage = "ThermalIonsParamsInitialization initializes the parametrs of the thermal ions.";


Begin["`Private`"];


Needs[               "CTS`Constants`"];
Needs["CTS`PlasmaDispersionFunction`"];
Needs[     "CTS`ThermalCalculations`"];


SpectralDensityS0[ThermalIonsParams_Association, PlasmaParams_Association, K_, W_, delta_] := Module[{ X, L, Y, R, b0 },

X = Table[ (K["ks"] PlasmaParams["Ions"][[i]]["LarmorRi"])^2 / 2,  { i, Length[PlasmaParams["Ions"]]}];

L = Table[
        Ceiling[
            X[[i]] Sinh[beta] /. FindRoot[ Cosh[beta] - beta Sinh[beta] == 
                                         Log[delta Abs[K["kp"]] PlasmaParams["Ions"][[i]]["Vti"] / (2 Sqrt[2 Pi] PlasmaParams["Ions"][[i]]["Z"]^2 PlasmaParams["Ions"][[i]]["ni/ne"])] / X[[i]], {beta, 1.5}]
    ],
    { i, Length[PlasmaParams["Ions"]]}];

 R = Table[ ThermalCalculationsR[X[[i]], L[[i]]] , { i, Length[PlasmaParams["Ions"]]}];
b0 = Table[ ThermalCalculationsB0[R[[i]], L[[i]]] , { i, Length[PlasmaParams["Ions"]]}];

Y = Table[ ThermalCalculationsY[W, PlasmaParams["Ions"][[i]]["Wci"], PlasmaParams["Ions"][[i]]["Vti"], K["kp"], L[[i]]], { i, Length[PlasmaParams["Ions"]]}];


Sqrt[2 Pi] / Abs[K["kp"]] * Sum[ PlasmaParams["Ions"][[i]]["Z"]^2 PlasmaParams["Ions"][[i]]["ni/ne"] (ThermalCalculationsS0Exp[ Y[[i]], R[[i]], L[[i]]] + Exp[-Y[[i, L[[i]] + 1]]^2]) / 
((1 + 2 b0[[i]]) PlasmaParams["Ions"][[i]]["Vti"]), {i, Length[PlasmaParams["Ions"]]} ]

];


SusceptibilityH[ThermalIonsParams_Association, PlasmaParams_Association, K_, W_, delta_] := Module[{ X, L, Y, R, b0 },

X = Table[ (K["ks"] PlasmaParams["Ions"][[i]]["LarmorRi"])^2 / 2,  { i, Length[PlasmaParams["Ions"]]}];

L = Table[
        Ceiling[
            X[[i]] Sinh[beta] /. FindRoot[ Cosh[beta] - beta Sinh[beta] == 
                                         Log[delta Abs[K["kp"]] PlasmaParams["Ions"][[i]]["Vti"] / (2 Sqrt[2 Pi] PlasmaParams["Ions"][[i]]["Z"]^2 PlasmaParams["Ions"][[i]]["ni/ne"])] / X[[i]], {beta, 1.5}]
    ],
    { i, Length[PlasmaParams["Ions"]]}];
    

 R = Table[ ThermalCalculationsR[X[[i]], L[[i]]] , { i, Length[PlasmaParams["Ions"]]}];
b0 = Table[ ThermalCalculationsB0[R[[i]], L[[i]]] , { i, Length[PlasmaParams["Ions"]]}];

Y = Table[ ThermalCalculationsY[W, PlasmaParams["Ions"][[i]]["Wci"], PlasmaParams["Ions"][[i]]["Vti"], K["kp"], L[[i]]], { i, Length[PlasmaParams["Ions"]]}];


SalpeterAlpha = 1 / (K["k"] PlasmaParams["DebyeL"]);

SalpeterAlpha^2 Sum[PlasmaParams["Ions"][[i]]["Z"]^2 PlasmaParams["Ions"][[i]]["ni/ne"](PlasmaParams["Te"]/PlasmaParams["Ti"])
                    (1 + ( W / (Sqrt[2] PlasmaParams["Ions"][[i]]["Vti"] Abs[K["kp"]]) ) * ( ThermalCalculationsS0Z[ Y[[i]], R[[i]], L[[i]]] + PlasmaDispersionFunctionZ[Y[[i, L[[i]] + 1]]] ) / ( 1 + 2 b0[[i]] ) ), { i, Length[PlasmaParams["Ions"]]}]

];


ThermalIonsParamsInitialization[PlasmaParams_Association, DiagnosticParams_Association, ThermalIonsData_Association] :=

<|
   "SpectralDensityS0" -> SpectralDensityS0,
     "SusceptibilityH" -> SusceptibilityH
|>;


End[];


EndPackage[];
