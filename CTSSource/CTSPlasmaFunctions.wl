(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)


BeginPackage["CTS`PlasmaFunctions`"];


PlasmaDispersionFunctionZ::usage = "PlasmaDispersionFunctionZ[x] calculates the plasma dispersion function with real argument x.";


Begin["`Private`"];


PlasmaDispersionFunctionZ[x_] := If[ Abs[x] <= 25, 
                                     Sqrt[Pi] (I - Erfi[x]) Exp[- x^2], 
                                 (*else*) 
                                     I Sqrt[Pi] Exp[- x^2] - 1 / x - 1 / (2 x^3)
                                 ];


End[];


EndPackage[];
