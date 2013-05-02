mailang
=======

Erlang Module to use basic functionality of a mail server: IMAP/POP3.


### IMAP ###
Basic usage:

    {ok, Pid} = mailconnector:start_link(HOST, imap).
    gen_server:call(Pid, [login, USERNAME, PASSWORD]).