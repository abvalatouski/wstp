# Talking with Wolfram Mathematica via Haskell (and C FFI)

_If you're not interested in Haskell, you can read the article up to "Step 3" or even "Step 2"._

## Brief introduction into WSTP

[WSTP (Wolfram Symbolic Transfer protocol)](https://www.wolfram.com/wstp/) is a protocol
that allows programs and Wolfram Kernel to communicate.
From perspective of this article, WSTP is a thing that will allow us to connect Haskell code
with the Kernel and perform various calculations, such as solving equations
and computing integrals.

All the logic will be split into server and client code.
The server will have access to the Kernel and the client will perform requests to the server
asking it to calculate something and send back a response with the result.
Unfortunately, access to the Kernel can only have Wolfram-ish stuff such as notebook files
or scripts. I'll peek the second one to automate the process. However, the client can be written
in almost any programming language. First, I'll use Wolfram Language for the sake of simplicity,
then I'll switch to C, and then finally to Haskell.

## Step 1. Connecting to Wolfram scripts via WSTP

So, the client code will be incredibly simple.
It gonna **connect** to some link, then activate it, then write a random expression onto it,
and then read a result from it.
The name of the link and the expression will be parametrized out with the command line arguments.

I'll name the script as `client.wls`.

```
name = $ScriptCommandLine[[2]];
link = LinkConnect[name, LinkProtocol -> "TCPIP"];

LinkActivate[link];

request = $ScriptCommandLine[[3]];
LinkWrite[link, request];

response = LinkRead[link];
Print[response];
```

The script will be launched with the following command.
I'll choose the link name as `6000` and I want to request an evaluation of, say, `2 + 2`.

```
wolframscript -file client.wls 6000 "2 + 2"
```

Now let's write a server. I'll name the script as `server.wls`.

First, let's initialize the link like I did in the previous code, but now I'll **create**
the link and parametrize out only its name.

```
name = ScriptCommandLine[[2]];
link = LinkCreate[name, LinkProtocol -> "TCPIP"];

Print["SERVER: Waiting the client to to respond."];
LinkActivate[link];
```

Let's write a loop where the server will handle all the incoming client requests.
Notice, that the client might close the connection, so we need to wrap some code into `Check`
and `Quiet` functions to catch and handle the errors.

```
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

        (* No error handling because the client waits our response. *)
        LinkWrite[link, response];
    ];
];
```

The script will be launched with the following command.

```
wolframscript -file server.wls 6000
```

Let's test the scripts. The output will look like this:

```
| Terminal 1                             | Terminal 2                                    |
|                                        |                                               |
| $ wolframscript -file server.wls 6000  | $ wolframscript -file client.wls 6000 "2 + 2" |
| SERVER: Waiting the client to respond. | 4                                             |
| CLIENT: 2 + 2                          |                                               |
| SERVER: 4                              |                                               |
| SERVER: Shutdown.                      |                                               |
```

## Step 2. Implementing client in C

Now let's write the same program in C. There is WSTP library written in C. It goes with installed
Wolfram Mathematica. On Windows x64 the library file (`wstp64i4.dll`) is placed
in `...\Wolfram Mathematica\SystemFiles\Links\WSTP\DeveloperKit\Windows-x86-64\SystemAdditions`
and the header file (`wstp.h`) is placed
in `...\Wolfram Mathematica\SystemFiles\Links\WSTP\DeveloperKit\Windows-x86-64\CompilerAdditions`.

Unlike previous Wolfram scripts C-client:
- requires explicit resource allocation, deallocation, and error handling
  (for conveniece I'll use a bunch of `goto`s);
- creates a link with explicit passing of command line arguments to the Kernel;
- writes data onto the link with `WSPut`-functions and "packeting".

```c
#include <stdio.h>

#include "wstp.h"

int main(int argc, char *argv[])
{
    WSEnvironmentParameter env_params = NULL;
    WSENV env = WSInitialize(env_params);
    if (env == NULL) {
        fputs("Failed to initialize WSTP library functions.", stderr);
        goto err_1;
    }

    char *link_name = argv[1];
    char *link_argv[] = {
        "-mathlink",
        "-linkname",     link_name,
        "-linkprotocol", "TCPIP",
        "-linkmode",     "connect"
    };
    size_t link_argc = sizeof(link_argv) / sizeof(const char *);
    int err;
    WSLINK link = WSOpenArgcArgv(env, link_argc, link_argv, &err);
    if (link == NULL || err != WSEOK) {
        fputs("Failed to create WSTP connection.", stderr);
        goto err_2;
    }

    if (!WSActivate(link)) {
        goto err_3;
    }

    const char *request = (const char *)argv[2];
    if (!WSPutString(link, request)) {
        fputs("Failed to send request to the server.", stderr);
        goto err_3;
    }
    if (!WSEndPacket(link)) {
        fputs("Failed to send request to the server.", stderr);
        goto err_3;
    }
    if (!WSFlush(link)) {
        fputs("Failed to send request to the server.", stderr);
        goto err_3;
    }

    const char *response;
    if (!WSGetString(link, &response)) {
        fputs("Failed to receive response from the server.", stderr);
        goto err_3;
    }
    puts(response);

err_3:
    WSClose(link);
err_2:
    WSDeinitialize(env);
err_1:
    return 1;
}
```

To compile that program I'll use the following `Makefile`.

```Makefile
all:
	gcc src/main.c \
		-o client \
		-I include \
		-L . \
		-lwstp64i4
```

`wstp.h` is placed into `include` directory and `wstp64i4.dll` is placed into root of the project
(`step-2` directory).

Let's test the program. The output will look like this:

```
| Terminal 1                             | Terminal 2            |
|                                        |                       |
| $ wolframscript -file server.wls 6000  | $ client 6000 "2 + 2" |
| SERVER: Waiting the client to respond. | 4                     |
| CLIENT: 2 + 2                          |                       |
| SERVER: 4                              |                       |
| SERVER: Shutdown.                      |                       |
```

## Step 3. Rewriting client in Haskell
