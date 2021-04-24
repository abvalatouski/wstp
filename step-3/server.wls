#!/usr/bin/env wolframscript
(* ::Package:: *)

name = $ScriptCommandLine[[2]];
link = LinkCreate[name, LinkProtocol -> "TCPIP"];


Print["SERVER: Waiting the client to respond."];
LinkActivate[link];


While[True,
    (* Check if any requests have been received. *)
    ready = Quiet[Check[
        LinkReadyQ[link],

        Print["SERVER: Shutdown."];
        Break[]
    ]];

    If[ready,
        request = Quiet[Check[
            LinkRead[link],

            Print["SERVER: Shutdown."];
            Break[]
        ]];

        Print[ToString[
            StringForm["CLIENT: ``", request]
        ]];

        (* Remember that the client code send the string not an expression. *)
        response = ToString[ToExpression[request]];
        Print[ToString[
            StringForm["SERVER: ``", response]
        ]];

        (* No error handling because the client waits a response. *)
        LinkWrite[link, response];
    ];
];