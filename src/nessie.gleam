//// A collection of functions for resolving DNS names in Gleam programs targeting Erlang.
////
//// The functions in this module are implemented using Erlang's built-in
//// `inet_res` module. The names of the functions and their return types 
//// map directly to the corresponding functions & types in the `inet_res` module.

import gleam/list
import gleam/function
import gleam/result
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist.{type Charlist}

/// Nameserver address and port.
pub type Nameserver =
  #(IPAddress, Int)

/// A DNS class type.
pub type DnsClass {
  In
  Chaos
  Hs
  Any
}

/// Options for resolving a DNS name.
pub type ResolverOption {
  Nameservers(opt: List(Nameserver))
  INet6(opt: Bool)
  Recurse(opt: Bool)
  Retry(opt: Int)
  TimeoutMillis(opt: Int)
  NxdomainReply(opt: Bool)
}

/// A DNS SOA record.
pub type SOARecord {
  SOARecord(
    mname: String,
    rname: String,
    serial: Int,
    refresh: Int,
    retry: Int,
    expire: Int,
    minimum: Int,
  )
}

/// A DNS error.
pub type ErrorReason {
  Formerr
  Qfmterr
  Servfail
  Nxdomain
  Notimp
  Refused
  Badvers
  TimeoutErr
  /// Encapuslates any other error, such as Posix errors.
  Other(error: String)
}

/// A DNS MX record.
pub type MXRecord {
  MXRecord(priority: Int, exchange: String)
}

/// Erlang representation of an IPv4 address.
pub type IPV4 =
  #(Int, Int, Int, Int)

/// Erlang representation of an IPv6 address.
pub type IPV6 =
  #(Int, Int, Int, Int, Int, Int, Int, Int)

/// A timeout for DNS lookups.
pub type Timeout {
  Timeout(value: Int)
  Infinity
}

/// A host entry.
///
/// For more information, see the [`inet_res:hostent`](https://www.erlang.org/doc/man/inet#type-hostent) Erlang type.
pub type Hostent(addr) {
  Hostent(
    name: String,
    aliases: List(String),
    addrtype: String,
    length: Int,
    addr_list: List(addr),
  )
}

/// A union type for IPv4 and IPv6 addresses.
pub type IPAddress {
  IPV4(IPV4)
  IPV6(IPV6)
}

/// DNS record types returning plain string data.
pub type StringRecordType {
  CNAME
  TXT
  NS
}

/// Converts an IP address to a string.
pub fn ip_to_string(ip_addr: IPAddress) -> Result(String, String) {
  ip_addr
  |> ntoa()
  |> result.map(charlist.to_string)
  |> result.map_error(atom.to_string)
}

pub fn string_to_ip(ip_addr: String) -> Result(IPAddress, String) {
  ip_addr
  |> parse_address()
  |> result.map(fn(dyn) {
    dyn
    |> dynamic.tuple4(dynamic.int, dynamic.int, dynamic.int, dynamic.int)
    |> result.map(IPV4)
    |> result.lazy_or(fn() {
      dyn
      |> dynamic.decode8(
        fn(i1, i2, i3, i4, i5, i6, i7, i8) { #(i1, i2, i3, i4, i5, i6, i7, i8) },
        dynamic.element(0, dynamic.int),
        dynamic.element(1, dynamic.int),
        dynamic.element(2, dynamic.int),
        dynamic.element(3, dynamic.int),
        dynamic.element(4, dynamic.int),
        dynamic.element(5, dynamic.int),
        dynamic.element(6, dynamic.int),
        dynamic.element(7, dynamic.int),
      )
      |> result.map(IPV6)
    })
    |> result.unwrap(IPV4(#(0, 0, 0, 0)))
  })
  |> result.map_error(atom.to_string)
}

/// Looks up a record of the specified type for the given name.
pub fn lookup(
  name: String,
  class: DnsClass,
  srt: StringRecordType,
  options: List(ResolverOption),
) -> List(String) {
  let erl_options = list.map(options, to_erl_resolver_option)

  name
  |> do_lookup_string(class, srt, erl_options)
  |> list.map(charlist.to_string)
}

/// Looks up an `A` record for the given name.
pub fn lookup_ipv4(
  name: String,
  class: DnsClass,
  options: List(ResolverOption),
) -> List(IPV4) {
  let erl_options = list.map(options, to_erl_resolver_option)
  do_lookup_ipv4(name, class, erl_options)
}

/// Looks up an `AAAA` record for the given name.
pub fn lookup_ipv6(
  name: String,
  class: DnsClass,
  options: List(ResolverOption),
) -> List(IPV6) {
  let erl_options = list.map(options, to_erl_resolver_option)
  do_lookup_ipv6(name, class, erl_options)
}

/// Looks up a SOA record for the given name.
pub fn lookup_soa(
  name: String,
  class: DnsClass,
  options: List(ResolverOption),
) -> List(SOARecord) {
  let erl_options = list.map(options, to_erl_resolver_option)
  name
  |> do_lookup_soa(class, erl_options)
  |> list.map(to_soa)
}

pub fn lookup_mx(
  name: String,
  class: DnsClass,
  options: List(ResolverOption),
) -> List(MXRecord) {
  let erl_options = list.map(options, to_erl_resolver_option)
  name
  |> do_lookup_mx(class, erl_options)
  |> list.map(to_mx)
}

/// Looks up a DNS record of the given type for the given name.
pub fn getbyname(
  name: String,
  srt: StringRecordType,
  timeout: Timeout,
) -> Result(Hostent(String), ErrorReason) {
  name
  |> do_getbyname_string(srt, timeout)
  |> result.map(to_hostent(_, charlist.to_string))
  |> result.map_error(to_dns_err)
}

/// Looks up an `A` record for the given name.
pub fn getbyname_ipv4(
  name: String,
  timeout: Timeout,
) -> Result(Hostent(IPV4), ErrorReason) {
  name
  |> do_getbyname_ipv4(timeout)
  |> result.map(to_hostent(_, function.identity))
  |> result.map_error(to_dns_err)
}

/// Looks up an `AAAA` record for the given name.
pub fn getbyname_ipv6(
  name: String,
  timeout: Timeout,
) -> Result(Hostent(IPV6), ErrorReason) {
  name
  |> do_getbyname_ipv6(timeout)
  |> result.map(to_hostent(_, function.identity))
  |> result.map_error(to_dns_err)
}

/// Looks up a SOA record for the given name.
pub fn getbyname_soa(
  name: String,
  timeout: Timeout,
) -> Result(Hostent(SOARecord), ErrorReason) {
  name
  |> do_getbyname_soa(timeout)
  |> result.map(to_hostent(_, to_soa))
  |> result.map_error(to_dns_err)
}

/// Looks up a DNS MX record for the given name.
pub fn getbyname_mx(
  name: String,
  timeout: Timeout,
) -> Result(Hostent(MXRecord), ErrorReason) {
  name
  |> do_getbyname_mx(timeout)
  |> result.map(to_hostent(_, to_mx))
  |> result.map_error(to_dns_err)
}

/// Looks up a host by IP address.
pub fn gethostbyaddr(
  address: IPAddress,
  timeout: Timeout,
) -> Result(Hostent(String), ErrorReason) {
  address
  |> do_gethostbyaddr(timeout)
  |> result.map(to_hostent(_, charlist.to_string))
  |> result.map_error(to_dns_err)
}

fn to_erl_resolver_option(option: ResolverOption) -> Dynamic {
  case option {
    INet6(inet6) -> dynamic.from(#(atom.create_from_string("inet6"), inet6))
    Recurse(recurse) ->
      dynamic.from(#(atom.create_from_string("recurse"), recurse))
    Retry(retry) -> dynamic.from(#(atom.create_from_string("retry"), retry))
    TimeoutMillis(timeout) ->
      dynamic.from(#(atom.create_from_string("timeout"), timeout))
    NxdomainReply(nxdomain_reply) ->
      dynamic.from(#(atom.create_from_string("nxdomain_reply"), nxdomain_reply))
    Nameservers(nameservers) -> {
      let erl_nameservers =
        list.map(nameservers, fn(ip_port) {
          let #(ip, port) = ip_port
          let ip = case ip {
            IPV4(ip) -> dynamic.from(ip)
            IPV6(ip) -> dynamic.from(ip)
          }

          #(ip, port)
        })
      dynamic.from(#(atom.create_from_string("nameservers"), erl_nameservers))
    }
  }
}

type ErlHostent(r) =
  #(Atom, Dynamic, List(Dynamic), Atom, Int, List(r))

type ErlSoa =
  #(Dynamic, Dynamic, Int, Int, Int, Int, Int)

type ErlMx =
  #(Int, Dynamic)

fn to_dns_err(e: Atom) -> ErrorReason {
  let e = atom.to_string(e)

  case e {
    "formerr" -> Formerr
    "qfmterror" -> Qfmterr
    "servfail" -> Servfail
    "nxdomain" -> Nxdomain
    "notimp" -> Notimp
    "refused" -> Refused
    "badvers" -> Badvers
    "timeout" -> TimeoutErr
    _ -> Other(e)
  }
}

fn to_hostent(
  erl_hostent: ErlHostent(r),
  addr_list_transform: fn(r) -> s,
) -> Hostent(s) {
  let #(_, name, aliases, addrtype, length, addr_list) = erl_hostent
  let gleam_aliases = list.map(aliases, from_dns_name)

  Hostent(
    name: from_dns_name(name),
    aliases: gleam_aliases,
    addrtype: atom.to_string(addrtype),
    length: length,
    addr_list: list.map(addr_list, addr_list_transform),
  )
}

fn from_dns_name(dyn: Dynamic) -> String {
  let atom_result_lazy = fn() {
    dyn
    |> atom.from_dynamic()
    |> result.map(atom.to_string)
  }

  dyn
  |> charlist_from_dynamic()
  |> result.map(charlist.to_string)
  |> result.lazy_or(atom_result_lazy)
  |> result.unwrap("")
}

fn to_mx(erl_mx: ErlMx) -> MXRecord {
  let #(priority, exchange) = erl_mx
  MXRecord(priority: priority, exchange: from_dns_name(exchange))
}

fn to_soa(erl_soa: ErlSoa) -> SOARecord {
  let #(mname, rname, serial, refresh, retry, expire, minimum) = erl_soa
  SOARecord(
    mname: from_dns_name(mname),
    rname: from_dns_name(rname),
    serial: serial,
    refresh: refresh,
    retry: retry,
    expire: expire,
    minimum: minimum,
  )
}

@external(erlang, "nessie_inet_res_ffi", "charlist_from_dynamic")
fn charlist_from_dynamic(d: Dynamic) -> Result(Charlist, dynamic.DecodeErrors)

@external(erlang, "nessie_inet_res_ffi", "getbyname")
fn do_getbyname_string(
  name: String,
  t: StringRecordType,
  timeout: Timeout,
) -> Result(ErlHostent(Charlist), Atom)

@external(erlang, "nessie_inet_res_ffi", "getbyname")
fn do_getbyname_soa(
  name: String,
  timeout: Timeout,
) -> Result(ErlHostent(ErlSoa), Atom)

@external(erlang, "nessie_inet_res_ffi", "getbyname_ipv4")
fn do_getbyname_ipv4(
  name: String,
  timeout: Timeout,
) -> Result(ErlHostent(IPV4), Atom)

@external(erlang, "nessie_inet_res_ffi", "getbyname_ipv6")
fn do_getbyname_ipv6(
  name: String,
  timeout: Timeout,
) -> Result(ErlHostent(IPV6), Atom)

@external(erlang, "nessie_inet_res_ffi", "getbyname_mx")
fn do_getbyname_mx(
  name: String,
  timeout: Timeout,
) -> Result(ErlHostent(ErlMx), Atom)

@external(erlang, "nessie_inet_res_ffi", "gethostbyaddr")
fn do_gethostbyaddr(
  address: IPAddress,
  timeout: Timeout,
) -> Result(ErlHostent(Charlist), Atom)

@external(erlang, "nessie_inet_res_ffi", "lookup_ipv4")
fn do_lookup_ipv4(
  name: String,
  class: DnsClass,
  options: List(Dynamic),
) -> List(IPV4)

@external(erlang, "nessie_inet_res_ffi", "lookup_ipv6")
fn do_lookup_ipv6(
  name: String,
  class: DnsClass,
  options: List(Dynamic),
) -> List(IPV6)

@external(erlang, "nessie_inet_res_ffi", "lookup")
fn do_lookup_string(
  name: String,
  class: DnsClass,
  t: StringRecordType,
  options: List(Dynamic),
) -> List(Charlist)

@external(erlang, "nessie_inet_res_ffi", "lookup_soa")
fn do_lookup_soa(
  name: String,
  class: DnsClass,
  options: List(Dynamic),
) -> List(ErlSoa)

@external(erlang, "nessie_inet_res_ffi", "lookup_mx")
fn do_lookup_mx(
  name: String,
  class: DnsClass,
  options: List(Dynamic),
) -> List(ErlMx)

@external(erlang, "nessie_inet_ffi", "ntoa")
fn ntoa(ip: IPAddress) -> Result(Charlist, Atom)

@external(erlang, "nessie_inet_ffi", "parse_address")
fn parse_address(ip: String) -> Result(Dynamic, Atom)
