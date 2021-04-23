#!/usr/bin/env wolframscript
(* ::Package:: *)

name = $ScriptCommandLine[[2]];
link = LinkConnect[name, LinkProtocol -> "TCPIP"];


LinkActivate[link];


request = $ScriptCommandLine[[3]];
LinkWrite[link, request];


response = LinkRead[link];
Print[response];
