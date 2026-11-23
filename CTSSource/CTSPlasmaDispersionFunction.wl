(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)


BeginPackage["CTS`PlasmaDispersionFunction`"];


PlasmaDispersionFunctionZ::usage = "PlasmaDispersionFunctionZ[x] calculates the plasma dispersion function with real argument x.";


Begin["`Private`"];


PlasmaDispersionFunctionZ[x_] := I Sqrt[Pi] Exp[- x^2] - 2 DawsonF[x];


End[];


EndPackage[];
