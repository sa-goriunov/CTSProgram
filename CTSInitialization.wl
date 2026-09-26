(* ::Package:: *)

(* Copyright 2026 sa-goriunov sa.goriunov@yandex.ru *)
(* Licensed under Apache 2.0 *)

Get[FileNameJoin[{NotebookDirectory[], "CTSSource", "CTSConstants.wl"}]];
Get[FileNameJoin[{NotebookDirectory[], "CTSSource", "CTSFrequencies.wl"}]];

PlasmaInitialization[PlasmaParams_Association]:= <|"Te" -> BoltzmannK PlasmaParams["Te"](*erg*),
                                                   "Ti" -> BoltzmannK PlasmaParams["Ti"](*erg*), 
                                                   "n0" -> PlasmaParams["n0"] 10^(-6)   (*cm^(-3)*),
                                                   "B"  -> PlasmaParams["B"]  10^(4)    (*G*)|>;
