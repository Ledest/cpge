-module(cpge).

-export([whereis_name/1,
         get_nearby_members/1, get_nearby_members/2,
         get_nearest_pid/1, get_nearest_pid/2]).

whereis_name({closest, Scope, GroupName}) when is_atom(Scope) ->
    case cpg:get_closest_pid(Scope, GroupName) of
        {ok, _, Pid} -> Pid;
        {error, _} -> undefined
    end;
whereis_name({closest, GroupName}) ->
    case cpg:get_closest_pid(GroupName) of
        {ok, _, Pid} -> Pid;
        {error, _} -> undefined
    end;
whereis_name({nearest, Scope, GroupName}) when is_atom(Scope) ->
    case get_nearest_pid(Scope, GroupName) of
        {ok, _, Pid} -> Pid;
        {error, _} -> undefined
    end;
whereis_name({nearest, GroupName}) ->
    case get_nearest_pid(GroupName) of
        {ok, _, Pid} -> Pid;
        {error, _} -> undefined
    end;
whereis_name(N) -> cpg:whereis_name(N).

get_nearest_pid(Scope, GroupName) ->
    case cpg:get_local_pid(Scope, GroupName) of
        {error, {no_process, _}} = R ->
            case get_nearby_members(Scope, GroupName) of
                {ok, _, {0, _}} -> cpg:get_remote_members(Scope, GroupName);
                {ok, _, {N, L}} -> {ok, GroupName, lists:nth(rand:uniform(N), L)};
                _ -> R
            end;
        R -> R
    end.

get_nearest_pid(GroupName) ->
    case cpg:get_local_pid(GroupName) of
        {error, {no_process, _}} = R ->
            case get_nearby_members(GroupName) of
                {ok, _, {0, _}} -> cpg:get_remote_members(GroupName);
                {ok, _, {N, L}} -> {ok, GroupName, lists:nth(rand:uniform(N), L)};
                _ -> R
            end;
        R -> R
    end.

get_nearby_members(Scope, GroupName) ->
    case cpg:get_remote_members(Scope, GroupName) of
        {ok, _, [_|_] = L} ->
            H = drop_prefix(node()),
            {ok, GroupName, lists:foldl(fun(P, {N, Ps} = A) ->
                                            case drop_prefix(node()) of
                                                H -> {N + 1, [P|Ps]};
                                                _ -> A
                                            end
                                        end, {0, []}, L)};
        R -> R
    end.

get_nearby_members(GroupName) ->
    case cpg:get_remote_members(GroupName) of
        {ok, _, [_|_] = L} ->
            H = drop_prefix(node()),
            {ok, GroupName, lists:foldl(fun(P, {N, Ps} = A) ->
                                            case drop_prefix(node()) of
                                                H -> {N + 1, [P|Ps]};
                                                _ -> A
                                            end
                                        end, {0, []}, L)};
        R -> R
    end.

drop_prefix(A) -> lists:dropwhile(fun(C) -> C =/= $@ end, atom_to_list(A)).
