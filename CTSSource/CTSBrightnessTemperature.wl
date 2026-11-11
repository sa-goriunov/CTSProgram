(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)


BeginPackage["CTS`BrightnessTemperature`"];


CTSBrightnessTemperatureT::usage = "CTSBrightnessTemperatureT[F_List, PlasmaParams_Association, DiagnosticParams_Association, ElectronsFractions_List, IonsFractions_List, delta] is the temperature at which a black body would have to be in order to duplicate the observed received intensity of the plasma PlasmaParams with particals fractions ElectronsFractions and IonsFractions at frequencies F with precision delta.";


Begin["`Private`"];


Needs[            "CTS`Constants`"];
Needs[    "CTS`ConversionFactors`"];
Needs[           "CTS`WaveVector`"];
Needs["CTS`GeometricalFormFactor`"];


CTSBrightnessTemperatureT[F_List, PlasmaParams_Association, DiagnosticParams_Association, ElectronsFractionsParams_List, IonsFractionsParams_List, delta_] := Module[{w, W, K, G, He, Hi, Se0, Si0,
                                                                                                                                                                 LongitudinalDielectricFunctionE},


w = F * GHztoradpersec; (*rad/sec*)
W = w - DiagnosticParams["WDiag"]; (*rad/sec*)
K = ScatteringWaveVector[DiagnosticParams, PlasmaParams];
G = GeometricalFormFactorG[w, DiagnosticParams, PlasmaParams];


He = Total[ #["SusceptibilityH"][ #, PlasmaParams, K, W, delta ] & /@ ElectronsFractionsParams ];
Hi = Total[ #["SusceptibilityH"][ #, PlasmaParams, K, W, delta ] & /@      IonsFractionsParams ];


LongitudinalDielectricFunctionE = 1 + He + Hi;


Se0 = Total[ #["SpectralDensityS0"][ #, PlasmaParams, K, W, delta ] & /@ ElectronsFractionsParams ];
Si0 = Total[ #["SpectralDensityS0"][ #, PlasmaParams, K, W, delta ] & /@      IonsFractionsParams ];


(*Return*) 
ClassicalElectronRadius^2 (2 Pi SpeedOfLightC / DiagnosticParams["WDiag"])^2 PlasmaParams["ne"] DiagnosticParams["PIn"] DiagnosticParams["Ob"] G *
( (Abs[1 - He/LongitudinalDielectricFunctionE])^2 Se0 + (Abs[He/LongitudinalDielectricFunctionE])^2 Si0 )
];


End[];


EndPackage[];
