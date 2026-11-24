(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)


BeginPackage["CTS`Electrons`Thermal`"];


ThermalElectronsParamsInitialization::usage = "ThermalElectronsParamsInitialization initializes the parametrs of the thermal electrons.";


Begin["`Private`"];


Needs[               "CTS`Constants`"];
Needs["CTS`PlasmaDispersionFunction`"];
Needs[     "CTS`ThermalCalculations`"];


SpectralDensityS0[ElectronsThermalParams_Association, PlasmaParams_Association, K_, W_, delta_] := Module[{ L, Y, R, b0 },

L = Ceiling[
        (Sqrt[2 BoltzmannK PlasmaParams["Te"] K["kp"]^2 / ElectronMass] InverseErfc[Min[1, delta * PlasmaParams["Wce"] Pi^(-3/2) / 2 ]]) 
         / PlasmaParams["Wce"] 
    ];

 R = ThermalCalculationsR[(K["ks"] PlasmaParams["LarmorRe"])^2 / 2, L];
b0 = ThermalCalculationsB0[R, L];

Y = ThermalCalculationsY[W, PlasmaParams["Wce"], PlasmaParams["Vte"], K["kp"], L];


Sqrt[2 Pi] / (PlasmaParams["Vte"] Abs[K["kp"]]) ( ThermalCalculationsS0Exp[Y, R, L] + Exp[-Y[[L + 1]]^2])/(1 + 2 b0)

];


SusceptibilityH[ElectronsThermalParams_Association, PlasmaParams_Association, K_, W_, delta_] := Module[{ L, Y, R, b0 },

L = Ceiling[
        (Sqrt[2 BoltzmannK PlasmaParams["Te"] K["kp"]^2 / ElectronMass] InverseErfc[Min[1, delta * PlasmaParams["Wce"] Pi^(-3/2) / 2 ]]) 
         / PlasmaParams["Wce"] 
    ];

 R = ThermalCalculationsR[(K["ks"] PlasmaParams["LarmorRe"])^2 / 2, L];
b0 = ThermalCalculationsB0[R, L];

Y = ThermalCalculationsY[W, PlasmaParams["Wce"], PlasmaParams["Vte"], K["kp"], L];


SalpeterAlpha = 1 / (K["k"] PlasmaParams["DebyeL"]);


SalpeterAlpha^2 (
   1 + ( W / (Sqrt[2] PlasmaParams["Vte"] K["kp"]) ) * (( ThermalCalculationsS0Z[ Y, R, L] + PlasmaDispersionFunctionZ[Y[[L + 1]]] ) / ( 1 + 2 b0 ))
)

];


ThermalElectronsParamsInitialization[PlasmaParams_Association, DiagnosticParams_Association, ThermalElectronsData_Association] :=

<|
   "SpectralDensityS0" -> SpectralDensityS0,
     "SusceptibilityH" -> SusceptibilityH
|>;


End[];


EndPackage[];
