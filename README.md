# nessie

[![Package Version](https://img.shields.io/hexpm/v/nessie)](https://hex.pm/packages/nessie)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/nessie/)

A friendly DNS client for Gleam programs targeting Erlang.

⚠️ This library is new and untested, use at your own risk ⚠️

```sh
gleam add nessie
```
```gleam
import nessie
import gleam/io

pub fn main() {
  let ipv4_addrs = nessie.lookup_ipv4("gleam.run", nessie.In, [])
  io.debug(ipv4_addrs)
}
```

Further documentation can be found at <https://hexdocs.pm/nessie>.

## Supported DNS Records

The following is a list of supported DNS record types. If
nessie doesn't currently support a DNS record type you want, feel free to contribute
to this project!

- [x] A
- [x] AAAA
- [x] TXT
- [x] CNAME
- [x] MX
- [x] SOA
- [x] NS
- [ ] PTR
- [ ] SRV
- [ ] CERT
- [ ] DCHID
- [ ] DNAME
- [ ] DNSKEY
- [ ] DS

## What about JavaScript support?

Due to Erlang and JavaScript's fundamentally different approaches
to asynchronous programming (processes & event loops, respectively),
it's not currently possible in Gleam to write a DNS client supporting
both targets.

For more information, see the following Discord thread: https://discord.com/channels/768594524158427167/1224351163797868575

## Development

```sh
gleam test  # Run the tests
gleam shell # Run an Erlang shell
```
