-module(gol_monitoring).

-export([cpu_load/0, mem_load/0, is_overload/0, save_stat/0, do_save_stat/0]).


-define(MAX_FILE_SIZE, 10485760).
-define(MAX_LOG_FILES, 2).
-define(CPU_STAT, "stat/cpu_stat.log").
-define(MEM_STAT, "stat/mem_stat.log").
-define(CPU_OVER, 80).
-define(MEM_OVER, 80).

%% @doc Resources of server is closed to the end.
is_overload() ->
    cpu_load(5) > ?CPU_OVER orelse mem_load() > ?MEM_OVER.

cpu_load() ->
    cpu_load(1).

-spec cpu_load(N) -> Result when
    N :: 1 | 5 | 15,
    Result :: non_neg_integer().
cpu_load(N) ->
    Load = case N of
        5 -> cpu_sup:avg5();
        15 -> cpu_sup:avg15();
        _ -> cpu_sup:avg1()
    end,
    D = 50,
    100 * (1 - D/(D + Load)).

mem_load() ->
    Ma = mem_allocated(),
    Mt = proplists:get_value(total_memory, memsup:get_system_memory_data(), 0),
    Ma / Mt * 100.

mem_allocated() ->
    lists:foldl(fun({_,X}, Sum) -> X + Sum end, 0, erlang:memory()).

save_stat() ->
    spawn(fun() -> do_save_stat() end),
    ok.

do_save_stat() ->
    archive_log(?CPU_STAT),
    archive_log(?MEM_STAT),
    Cpu = cpu_load(),
    Mem = mem_load(),
    file:write_file(?CPU_STAT, <<(integer_to_binary(Cpu))/binary, "\n">>, [append]),
    file:write_file(?MEM_STAT, <<(integer_to_binary(Mem))/binary, "\n">>, [append]),
    timer:sleep(60000),
    do_save_stat().


archive_log(Filename) ->
    case filelib:file_size(Filename) of
        Size when Size >= ?MAX_FILE_SIZE ->
            archive_log(Filename, ?MAX_LOG_FILES);
        _ ->
            ok
    end.
archive_log(_, -1) ->
    ok;
archive_log(Filename, N) ->
    RotateFile = Filename ++ (case N =:= 0 of true -> ""; _ -> integer_to_list(N) end),
    case filelib:is_file(RotateFile) of
        true ->
            case N =:= ?MAX_LOG_FILES of
                true ->
                    file:delete(RotateFile);
                _ ->
                    file:copy(RotateFile, Filename ++ integer_to_list(N + 1)),
                    case file:open(RotateFile, [write]) of
                        {ok, Handle} ->
                            file:truncate(Handle),
                            file:close(Handle);
                        E2 ->
                            E2
                    end
            end;
        _ ->
            ok
    end,
    archive_log(Filename, N - 1).