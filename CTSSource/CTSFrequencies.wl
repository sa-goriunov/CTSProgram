(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)

BeginPackage["CTSFrequencies`"];

Wce::usage =    "Electron gyrofrequency (cgs)";
Wci::usage =         "Ion gyrofrequency (cgs)";
Wpe::usage = "Electrom plasma frequency (cgs)";
Wpi::usage =      "Ion plasma frequency (cgs)";

Begin["`Private`"];

Needs["CTSConstants`"];

Wce[PlasmaParams_Association]                 := ElementaryCharge * PlasmaParams["B"] / (ElectronMass * SpeedOfLightC); (*rad/sec*)
Wci[PlasmaParams_Association, Ion_Association]:= ElementaryCharge * PlasmaParams["B"] / ( Ion["Mass"] * SpeedOfLightC); (*rad/sec*)

Wpe[PlasmaParams_Association]:= Sqrt[4 Pi PlasmaParams["n0"]                          ElementaryCharge^2 / ElectronMass];(*rad/sec*)
Wpi[PlasmaParams_Association, Ion_Association]:=
							    Sqrt[4 Pi PlasmaParams["n0"] Ion["ni/ne"] Ion["Z"]^2 ElementaryCharge^2 /  Ion["Mass"]];(*rad/sec*)

End[];

EndPackage[];



