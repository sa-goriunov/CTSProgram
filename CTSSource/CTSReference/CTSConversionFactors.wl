(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)


BeginPackage["CTS`ConversionFactors`"];


        MJtoeV::usage =             "MJtoeV is a constant for transfering megajoules to electronvolts.";
          TtoG::usage =                           "TtoG is a constant for transfering teslas to gauss.";
         mtocm::usage =                    "mtocm is a constant for transfering meters to centimeters.";
GHztoradpersec::usage = "GHztoradpersec is a constant for transfering gigahertz to radians per second.";
AtomicMassUnit::usage =                         "AtomicMassUnit is a constant for transfering Da to g.";


Begin["`Private`"];


(*Src: NRL Plasma Formulary revised, J.D. Huba, 2004*)


        MJtoeV = 10^(25) / 1.6022; (*eV/MJ*)
          TtoG = 10^4; (*G/T*)
         mtocm = 10^2; (*cm/m*)
GHztoradpersec = 2 Pi 10^9; (*(rad/sec)/GHz*)
AtomicMassUnit = 1.6605 * 10^(-24); (*g/Da*)


End[];


EndPackage[];
