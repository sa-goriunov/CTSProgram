(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)

BeginPackage["CTS`Constants`"];

        BoltzmannK::usage =                 "BoltzmannK is Boltzmann constant, erg/keV.";
  ElementaryCharge::usage =         "ElementaryCharge is the charge of an electron, Fr.";
      ElectronMass::usage =                          "ElectronMass is electron mass, g.";
    AtomicMassUnit::usage =      "AtomicMassUnit is a constant for transfering Da to g.";
     SpeedOfLightC::usage =     "SpeedOfLightC is the speed of light in vacuum, cm/sec.";
ElectronRestEnergy::usage = "ElectronRestEnergy is the rest energy of an electron, erg.";
       ClassicalRe::usage =          "ClassicalRe is the classical electron radius, cm.";


Begin["`Private`"];


(*Src: NRL Plasma Formulary revised, J.D. Huba, 2004*)

      BoltzmannK = 1.6022 * 10^( -9); (*erg/keV*)
ElementaryCharge = 4.8032 * 10^(-10); (*Fr*)
    ElectronMass = 9.1094 * 10^(-28); (*g*)
  AtomicMassUnit = 1.6605 * 10^(-24); (*g*)
   SpeedOfLightC = 2.9979 * 10^( 10); (*cm/s*)

ElectronRestEnergy = ElectronMass * SpeedOfLightC^2; (*erg*)
  
ClassicalRe = ElementaryCharge^2 / ElectronRestEnergy; (*cm*)


End[];

EndPackage[];
