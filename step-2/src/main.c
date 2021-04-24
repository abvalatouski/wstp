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
    size_t link_argc = sizeof(link_argv) / sizeof(char *);
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
    WSReleaseString(link, response);

err_3:
    WSClose(link);
err_2:
    WSDeinitialize(env);
err_1:
    return 0;
}
