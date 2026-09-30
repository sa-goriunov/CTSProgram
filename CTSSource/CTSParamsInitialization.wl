(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)

BeginPackage["CTS`ParamsInitialisation`"];

    PlasmaParamsInitialization::usage = "PlasmaParamsInitialization[PlasmaParams_Association] initializes the parametrs of the plasma.";

DiagnosticParamsInitialization::usage = 
                            "DiagnosticParamsInitialization[DiagnosticParams_Association] initializes the parametrs of the diagnostic.";


Begin["`Private`"];

Needs["CTS`Constants`"];


(*IonInitialization[PlasmaParams_Association, IonParams_Association] initializes the parametrs of the ion.*)
IonInitialization[PlasmaParams_Association, IonParams_Association] := Module[{Wci, Vti, IonRestEnergy, ni},

          Wci = IonParams["Z"] ElementaryCharge PlasmaParams["B"] / (IonParams["A"] * AtomicMassUnit * SpeedOfLightC);
IonRestEnergy = IonParams["A"] * AtomicMassUnit * SpeedOfLightC^2;
          Vti = SpeedOfLightC * Sqrt[BoltzmannK * PlasmaParams["Ti"] / IonRestEnergy];
           ni = PlasmaParams["ne"] * IonParams["ni/ne"] * 10^(-6); (*cm^(-3)*)

<|
	         "Z" -> IonParams["Z"],                  (*Nuclear charge number of the ion*)
	         "m" -> IonParams["A"] * AtomicMassUnit, (*g, mass of the ion*)
	"RestEnergy" -> IonRestEnergy,                   (*erg*)
	     "ni/ne" -> IonParams["ni/ne"],              (*the ratio of the concentrations*)
	       "Wci" -> Wci,                             (*rad/sec, ion gyrofrequency*)
	
	"Wpi" -> N[Sqrt[4 Pi ni ElementaryCharge^2 / (IonParams["A"] AtomicMassUnit)]], (*rad/sec, ion plasma frequency*)
	
	     "Vti" -> Vti,                    (*cm/sec, ion thermal velocity*)
	"LarmorRi" -> N[Sqrt[2] * Vti / Wci] (*cm, ion Larmor radius*)
|>
]


PlasmaParamsInitialization[PlasmaParams_Association] := Module[ {Wce, Vte, ne},

Wce = ElementaryCharge PlasmaParams["B"] / (ElectronMass * SpeedOfLightC);
Vte = SpeedOfLightC * Sqrt[BoltzmannK * PlasmaParams["Te"] / ElectronRestEnergy];
 ne = PlasmaParams["ne"] * 10^(-6);

	<|

 
	"Te" -> BoltzmannK * PlasmaParams["Te"], (*erg*)
	"Ti" -> BoltzmannK * PlasmaParams["Ti"], (*erg*)


	"ne" -> ne,                          (*cm^(-3)*)
	 "B" -> PlasmaParams["B"] * 10^( 4), (*G*)

  
	"Wce" -> Wce,                                                                 (*rad/sec, electron gyrofrequency*)
	"Wpe" -> N[Sqrt[4 Pi ne ElementaryCharge^2 / ElectronMass]], (*rad/sec, electrom plasma frequency*)
 
 
	     "Vte" -> Vte,                  (*cm/sec, electron thermal velocity*)
	"LarmorRe" -> N[Sqrt[2] Vte / Wce],        (*cm, electron Larmor radius*)
	  "DebyeL" -> N[Sqrt[BoltzmannK * PlasmaParams["Te"] / (4 Pi ElementaryCharge^2 ne)]], (*cm, Debye radius*)
	  
	  
	  "Ions" -> Map[IonInitialization[PlasmaParams, #] &, PlasmaParams["Ions"]] (*the plasma composition*)
	|> 
]


DiagnosticParamsInitialization[DiagnosticParams_Association] := <|
  "WDiag" -> N[DiagnosticParams["WDiag"] * 2 Pi * 10^(9)], (*rad/sec, the diagnostic frequency*)
  
"alphaIn" -> DiagnosticParams["alphaIn"], (*the angle between the incident wave vector and the lockal B-field*)
 "ModeIn" -> DiagnosticParams["ModeIn"],  (*the type of the incident mode: ordinary (O) or extraordinary (X) *)
"alphaSc" -> DiagnosticParams["alphaSc"], (*the angle between the scattered wave vector and the lockal B-field*)
 "ModeSc" -> DiagnosticParams["ModeSc"],  (*the type of the scattered mode: ordinary (O) or extraordinary (X) *)
    "phi" -> DiagnosticParams["phi"],     (*the angle between the incident and scattered wave vectors*)
     "Ob" -> DiagnosticParams["Ob"],      (*cm^(-1), beam overlap volume*)
     
    "Pin" -> DiagnosticParams["Pin"] * 10^(25) / 1.6022 (*eV/sec, the incident power*) 
|>


End[];

EndPackage[];
