(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)

BeginPackage["CTSConstants`"];

        BoltzmannK::usage =      "Boltzmann constant, erg/keV";
  ElementaryCharge::usage =  "The charge of an electron (cgs)";
      ElectronMass::usage =              "Electron mass (cgs)";
        ProtonMass::usage =                "Proton mass (cgs)";
     SpeedOfLightC::usage =   "Speed of light in vacuum (cgs)";
ElectronRestEnergy::usage = "Rest energy of an electron (cgs)";
  ProtonRestEnergy::usage =    "Rest energy of a proton (cgs)";
       ClassicalRe::usage =  "Classical electron radius (cgs)";

Begin["`Private`"];

(*Src: NRL Plasma Formulary revised, J.D. Huba, 2004*)

      BoltzmannK = 1.6022 * 10^( -9); (*erg/keV*)
ElementaryCharge = 4.8032 * 10^(-10); (*Fr*)
    ElectronMass = 9.1094 * 10^(-28); (*g*)
      ProtonMass = 1.6726 * 10^(-24); (*g*)
   SpeedOfLightC = 2.9979 * 10^( 10); (*cm/s*)

ElectronRestEnergy = ElectronMass * SpeedOfLightC^2; (*erg*)
  ProtonRestEnergy =   ProtonMass * SpeedOfLightC^2; (*erg*)
  
ClassicalRe = ElementaryCharge^2 / ElectronRestEnergy; (*cm*)

End[]

EndPackage[]
