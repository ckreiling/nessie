import gleeunit
import gleeunit/should
import gleam/set
import nessie

pub fn main() {
  gleeunit.main()
}

pub fn ipv4_address_to_string_succeeds_test() {
  let r = nessie.ip_to_string(nessie.IPV4(#(1, 2, 3, 4)))
  should.equal(r, Ok("1.2.3.4"))
}

pub fn ipv4_address_to_string_fails_for_invalid_ip_test() {
  let r = nessie.ip_to_string(nessie.IPV4(#(1000, 2000, 3000, 4000)))
  should.equal(r, Error("einval"))
}

pub fn ipv6_address_to_string_succeeds_test() {
  let r =
    nessie.ip_to_string(nessie.IPV6(#(100, 200, 300, 0, 500, 600, 700, 800)))
  should.equal(r, Ok("64:c8:12c:0:1f4:258:2bc:320"))
}

pub fn ipv6_address_to_string_fails_for_invalid_ip_test() {
  let r =
    nessie.ip_to_string(
      nessie.IPV6(#(-1, -2, 100, 20_302_302, 2020, -3, 2, 10)),
    )
  should.equal(r, Error("einval"))
}

pub fn ipv4_string_to_address_succeeds_test() {
  let r = nessie.string_to_ip("1.2.3.4")
  should.equal(r, Ok(nessie.IPV4(#(1, 2, 3, 4))))
}

pub fn ipv4_string_to_address_fails_for_invalid_ip_test() {
  let r = nessie.string_to_ip("1.2.3.256")
  should.equal(r, Error("einval"))
}

pub fn ipv6_string_to_address_succeeds_test() {
  let r =
    nessie.string_to_ip("64:c8:12c:0:1f4:258:2bc:320")
  should.equal(r, Ok(nessie.IPV6(#(100, 200, 300, 0, 500, 600, 700, 800))))
}

pub fn cname_test() {
  let assert Ok(hostent) =
    nessie.getbyname("www.growtherapy.com", nessie.CNAME, nessie.Infinity)

  should.equal(hostent.addr_list, ["growtherapy.com"])
}

pub fn txt_test() {
  let assert Ok(hostent) =
    nessie.getbyname("example.com", nessie.TXT, nessie.Infinity)

  should.equal(hostent.addr_list, ["growtherapy.com"])
}

pub fn ipv4_test() {
  let assert Ok(hostent) =
    nessie.getbyname_ipv4("ipv4only.arpa", nessie.Infinity)

  let expected_addrs = set.from_list([#(192, 0, 0, 170), #(192, 0, 0, 171)])
  let addrs = set.from_list(hostent.addr_list)

  should.equal(expected_addrs, addrs)
}

pub fn lookup_ipv4_test() {
  let addrs =
    nessie.lookup_ipv4("google.com", nessie.In, [
      nessie.Nameservers([#(nessie.IPV4(#(1, 1, 1, 1)), 53)]),
    ])

  should.equal(addrs, [#(0, 0, 0, 0)])
}

pub fn mx_test() {
  let assert Ok(hostent) = nessie.getbyname_mx("google.com", nessie.Infinity)

  should.equal(hostent.addr_list, [nessie.MXRecord(10, "smtp.google.com")])
}
