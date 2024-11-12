%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, adrianx@163.com
%%% @doc
%%%
%%% @end
%%% Created : 12. 11月 2024 9:38
%%%-------------------------------------------------------------------
-author("adrianx@163.com").
-ifndef(_AMAZE_CLIENT_HRL_).
-define(_AMAZE_CLIENT_HRL_,true).

-record(client_mock_context,
    {
        transport_handle::module(),
        protocol::atom(),
        pack_frame::atom(),
        connection_pid::pid(),
        connection_context::map()
    }).

-endif.