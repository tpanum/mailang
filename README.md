mailang
=======

Erlang Module to use basic functionality of a mail server: IMAP/POP3.

This is still under heavy development, feel free to help out!

[http://tools.ietf.org/html/rfc3501](IMAP Documentation)

### IMAP ###
Basic usage:

    {ok, Pid} = mailconnector:start_link(HOST, imap).
    gen_server:call(Pid, [login, USERNAME, PASSWORD]).