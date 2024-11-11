%%%-------------------------------------------------------------------
%%% @author adrianx@163.com
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 11月 2024 10:10
%%%-------------------------------------------------------------------
-module(amaze_file).
-author("adrianx@163.com").

%% API
-export([ensure_dir/1]).

-spec ensure_dir(Name) -> 'ok' | {'error', file:posix()} when
	Name :: atom() | Name:: binary() | Name :: list().

ensure_dir(Path) when is_binary(Path)->
	ensure_dir(binary_to_list(Path));
ensure_dir(Path) when is_atom(Path)->
	ensure_dir(atom_to_list(Path));
ensure_dir(Path)->
	NewPath = case lists:last(Path) of
		          [] -> "/";
		          $/ -> Path;
		          _ -> Path++ "/"
	          end,
	filelib:ensure_dir(NewPath).
