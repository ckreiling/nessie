-module(nessie_inet_res_ffi).

-export([getbyname/3, gethostbyaddr/2, lookup/4, charlist_from_dynamic/1,
         convert_to_atom/1, lookup_soa/3, lookup_mx/3, lookup_ipv4/3, lookup_ipv6/3,
         getbyname_soa/2, getbyname_mx/2, getbyname_ipv4/2, getbyname_ipv6/2]).

getbyname_soa(Name, Timeout) -> getbyname(Name, soa, Timeout).

getbyname_mx(Name, Timeout) -> getbyname(Name, mx, Timeout).

getbyname_ipv4(Name, Timeout) -> getbyname(Name, a, Timeout).

getbyname_ipv6(Name, Timeout) -> getbyname(Name, aaaa, Timeout).

getbyname(Name, Type, Timeout) ->
    T = case Timeout of
            infinity -> infinity;
            {_, Millis} -> Millis
        end,

    BinaryName = binary:bin_to_list(Name),
    inet_res:getbyname(BinaryName, convert_to_atom(Type), T).

gethostbyaddr(Address, Timeout) ->
    T = case Timeout of
            infinity -> infinity;
            {_, Millis} -> Millis
        end,

    {_, A} = Address,
    inet_res:gethostbyaddr(A, T).

lookup_soa(Name, Class, Opts) -> lookup(Name, Class, soa, Opts).

lookup_mx(Name, Class, Opts) -> lookup(Name, Class, mx, Opts).

lookup_ipv4(Name, Class, Opts) -> lookup(Name, Class, a, Opts).

lookup_ipv6(Name, Class, Opts) -> lookup(Name, Class, aaaa, Opts).

lookup(Name, Class, Type, Opts) ->
    CharlistName = binary:bin_to_list(Name),
    inet_res:lookup(CharlistName, Class, convert_to_atom(Type), Opts).

charlist_from_dynamic(Data) when is_list(Data) -> {ok, Data};
charlist_from_dynamic(Data) ->
    {error, [{decode_error, <<"Charlist">>, gleam@dynamic:classify(Data), []}]}.

convert_to_atom(Type) when is_atom(Type) -> Type;
convert_to_atom(Type) -> binary_to_atom(Type, utf8).
