(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)

BeginPackage["CTS`BrightnessTemperature`"];

CTSBrightnessTemperatureT::usage = "CTSBrightnessTemperatureT[F_List, PlasmaParams_Association, DiagnosticParams_Association, ElectronsFractionsParams_List, IonsFractionsParams_List] is the temperature at which a black body would have to be in order to duplicate the observed received intensity of the plasma PlasmaParams at frequencies F.";


Begin["`Private`"];

Needs[            "CTS`Constants`"];
Needs[          "CTS`WaveVectors`"];
Needs["CTS`GeometricalFormFactor`"];


CTSBrightnessTemperatureT[F_List, PlasmaParams_Association, DiagnosticParams_Association, ElectronsFractionsParams_List, IonsFractionsParams_List] := Module[{W, K, G, He, Hi, Se0, Si0,
                                                                                                                                                        LongitudinalDielectricFunctionE},


W = 2 Pi F 10^(9); (*rad/sec*)
K =   ScatteringWaveVector[DiagnosticParams, PlasmaParams];
G = GeometricalFormFactorG[DiagnosticParams, PlasmaParams];


He = Total[ SusceptibilityH[ #["Fraction"], #, PlasmaParams, K, W ] & /@ ElectronsFractionsParams];
Hi = Total[ SusceptibilityH[ #["Fraction"], #, PlasmaParams, K, W ] & /@      IonsFractionsParams];

LongitudinalDielectricFunctionE = 1 + He + Hi;


Se0 = Total[ SpectralDensityS0[ #["Fraction"], #, PlasmaParams, K, W ] & /@ ElectronsFractionsParams];
Si0 = Total[ SpectralDensityS0[ #["Fraction"], #, PlasmaParams, K, W ] & /@      IonsFractionsParams];


(*Return*) 
ClassicalRe^2 (2 Pi SpeedOfLightC / DiagnosticParams["WDiag"])^2 PlasmaParams["ne"] DiagnosticParams["PIn"] DiagnosticParams["Ob"] G *
( (Abs[1 - He/LongitudinalDielectricFunctionE])^2 Se0 + (Abs[He/LongitudinalDielectricFunctionE])^2 Si0 )
];

End[];

EndPackage[];
