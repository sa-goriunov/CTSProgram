(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)

BeginPackage["CTS`WaveVectors`"];

PlasmaRefractiveIndexN::usage = "PlasmaRefractiveIndexN[W, theta, ModeType, PlasmaParams_Association] is a refractive index N for an electromagnetic wave with frequency W and polarization ModeType, propagating in a uniform magnetized plasma PlasmaParams with angle theta to the local B-field.";

ScatteringWaveVector::usage = "ScatteringWaveVector[DiagnosticParams_Association, PlasmaParams_Association] is a scattering wave vector in a uniform magnetized plasma in the scattering geometry DiagnosticParams.";


Begin["`Private`"];

Needs["CTS`Constants`", "CTS`ParamsInitialisation`"]

PolarizationLambda["O"] =  1; (*     Ordinary wave*)
PolarizationLambda["X"] = -1; (*Extraordinary wave*)

PlasmaRefractiveIndexN[W_, theta_, ModeType_, PlasmaParams_Association] := Module[{ u, v, gamma, lambda},

u = (PlasmaParams["Wce"])^2 / W^2;
v = (PlasmaParams["Wpe"])^2 / W^2;

 gamma = (u Sin[theta]^2)^2 + 4 u (1 - v)^2 Cos[theta]^2;
lambda = PolarizationLambda[ModeType];

Sqrt[1 - (2 v (1 - v)/ (2 (1 - v) - u Sin[theta]^2 + lambda Sqrt[gamma]))]
]




End[];

EndPackage[];

