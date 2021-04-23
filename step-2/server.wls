#!/usr/bin/env wolframscript
(* ::Package:: *)

name = $ScriptCommandLine[[2]];
link = LinkCreate[name, LinkProtocol -> "TCPIP"];


Print["SERVER: Waiting the client to respond."];
LinkActivate[link];


While[True,
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
            StringForm["CLIENT: ``", request]]
        ];
        
        response = ToString[ToExpression[request]];
        Print[ToString[
             StringForm["SERVER: ``", response]]
        ];
        
        LinkWrite[link, response];
    ];
];
