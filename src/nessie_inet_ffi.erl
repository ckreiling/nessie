-module(nessie_inet_ffi).

-export([ntoa/1, parse_address/1]).

ntoa(Address) ->
    {_, A} = Address,

    case inet:ntoa(A) of
        L when is_list(L) -> {ok, L};
        E -> E
    end.

parse_address(Address) ->
    A = binary:bin_to_list(Address),
    inet:parse_address(A).
