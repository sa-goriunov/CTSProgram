(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)


BeginPackage["CTS`GeometricalFormFactor`"];


GeometricalFormFactorG::usage = "GeometricalFormFactorG[W_List, DiagnosticParams_Association, PlasmaParams_Association] is a geometrical form factor for diagnostic DiadnosticParams in the plasma PlasmaParams at the scattered frequencies W.";


Begin["`Private`"];


Needs["CTS`WaveVectors`"];


GeometricalFormFactorG[W_List, DiagnosticParams_Association, PlasmaParams_Association] := Module[{ uIn, vIn, uSc, vSc,
                                                                                    epsilon1In, epsilon2In, epsilon3In,
                                                                                    epsilon1Sc, epsilon2Sc, epsilon3Sc,
                                                                                          nIn, nSc, EIn, ESc, eIn, eSc,
                                                                                    gammaIn, gammaSc, deltaIn, deltaSc},


uIn = (PlasmaParams["Wce"])^2 / DiagnosticParams["WDiag"]^2;
vIn = (PlasmaParams["Wpe"])^2 / DiagnosticParams["WDiag"]^2;


epsilon1In =       1 - vIn / (1 - uIn);
epsilon2In = vIn Sqrt[uIn] / (1 - uIn);
epsilon3In =                  1 - vIn ;


uSc = (PlasmaParams["Wce"])^2 / W^2;
vSc = (PlasmaParams["Wpe"])^2 / W^2;


epsilon1Sc =       1 - vSc / (1 - uSc);
epsilon2Sc = vSc Sqrt[uSc] / (1 - uSc);
epsilon3Sc =                  1 - vSc ;


(*Refractive indexes*)
nIn = PlasmaRefractiveIndexN[DiagnosticParams["WDiag"], DiagnosticParams["alphaIn"], DiagnosticParams["ModeIn"], PlasmaParams];
nSc = PlasmaRefractiveIndexN[#, DiagnosticParams["alphaSc"], DiagnosticParams["ModeSc"], PlasmaParams] & /@ W;


(*E is the dielectric tensor for cold plasma PlasmaParams for wave with frequency W. Axe Z is parallel with the local B-field*)
EIn = {{epsilon1In, I epsilon2In, 0}, {- I epsilon2In, epsilon1In, 0}, {0, 0, epsilon3In}};
ESc = Table[ {{epsilon1Sc[[i]], I epsilon2Sc[[i]], 0}, {- I epsilon2Sc[[i]], epsilon1Sc[[i]], 0}, {0, 0, epsilon3Sc[[i]]}}, {i, 1, Length[W]} ];


(*e is the un-normalized polarization vector for an electromagnetic wave with frequency W and polarization ModeType, propagating in a uniform magnetized plasma PlasmaParams with angle alpha to the local B-field*)
eIn = { 1, I epsilon2In / (epsilon1In - nIn^2), nIn^2 Sin[DiagnosticParams["alphaIn"]] Cos[DiagnosticParams["alphaIn"]] / (nIn^2 Sin[DiagnosticParams["alphaIn"]]^2 - epsilon3In)};
eSc = Table[ RotationMatrix[DiagnosticParams["phi"], {0, 0, 1}] .
{ 1, I epsilon2Sc[[i]] / (epsilon1Sc[[i]] - nSc[[i]]^2), nSc[[i]]^2 Sin[DiagnosticParams["alphaSc"]] Cos[DiagnosticParams["alphaSc"]] / (nSc[[i]]^2 Sin[DiagnosticParams["alphaSc"]]^2 - epsilon3Sc[[i]])}, {i, 1, Length[W]} ];


gammaIn = (uIn Sin[DiagnosticParams["alphaIn"]]^2)^2 + 4 uIn (1 - vIn)^2 Cos[DiagnosticParams["alphaIn"]]^2;
gammaSc = (uSc Sin[DiagnosticParams["alphaSc"]]^2)^2 + 4 uSc (1 - vSc)^2 Cos[DiagnosticParams["alphaSc"]]^2;


(*The angles between the direction of the wave vector and that of the ray*)
deltaIn = ArcTan[ - PolarizationLambda[DiagnosticParams["ModeIn"]] (1 - nIn^2) uIn Cos[DiagnosticParams["alphaIn"]] Sin[DiagnosticParams["alphaIn"]] / Sqrt[gammaIn]];
deltaSc = ArcTan[ - PolarizationLambda[DiagnosticParams["ModeSc"]] (1 - nSc^2) uSc Cos[DiagnosticParams["alphaSc"]] Sin[DiagnosticParams["alphaSc"]] / Sqrt[gammaSc]];


(*Return*)
(DiagnosticParams["WDiag"]^3 W / PlasmaParams["Wpe"]^4) nIn nSc Abs[Cos[deltaIn] Cos[deltaSc]] Table[ Abs[Conjugate[eSc[[i]]] . (ESc[[i]] - IdentityMatrix[3]) . eIn]^2 / (Conjugate[eSc[[i]]] . ESc[[i]] . eSc[[i]]), {i, 1, Length[W]}] / (Conjugate[eIn] . EIn . eIn)
];


End[];


EndPackage[];
