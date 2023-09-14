use std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr, SocketAddrV4, SocketAddrV6};

impl_diagnostic!(string(
  SocketAddr::parse_socket_addr,
  SocketAddrV4::parse_socket_addr_v4,
  SocketAddrV6::parse_socket_addr_v6,
  IpAddr::parse_ip_addr,
  Ipv4Addr::parse_ipv4_addr,
  Ipv6Addr::parse_ipv6_addr,
));
