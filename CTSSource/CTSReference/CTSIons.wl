(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)

BeginPackage["CTS`Ions`"];

 IonH::usage =  "IonH_Association is a set of information about the hydrogen ion.";
 IonD::usage = "IonD_Association is a set of information about the deuterium ion.";
 IonT::usage =   "IonT_Association is a set of information about the tritium ion.";
IonLi::usage =  "IonLi_Association is a set of information about the lithium ion.";
 IonO::usage =    "IonO_Association is a set of information about the oxygen ion.";
 IonC::usage =    "IonC_Association is a set of information about the carbon ion.";
 

Begin["`Private`"];


(*Src: Meng Wang et al 2021 Chinese Phys. C 45 030003*)
 IonH = <|"Z" -> 1, "A" ->  1.0078 (*Da*)|>;
 IonD = <|"Z" -> 1, "A" ->  2.0141 (*Da*)|>;
 IonT = <|"Z" -> 1, "A" ->  3.0160 (*Da*)|>;
 

(*Src: https://iupac.qmul.ac.uk/AtWt/*)
IonLi = <|"Z" -> 3, "A" ->  6.946  (*Da*)|>;
 IonO = <|"Z" -> 8, "A" -> 15.9991 (*Da*)|>;
 IonC = <|"Z" -> 6, "A" -> 12.0112 (*Da*)|>;


End[];

EndPackage[];
