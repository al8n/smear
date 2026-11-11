#![no_std]

/// Without a path that names `smear`, rustc never loads its dependency graph, and the proof in
/// `ci/noalloc.sh` would be vacuous.
extern crate smear;

#[panic_handler]
fn panic(_: &core::panic::PanicInfo<'_>) -> ! {
  loop {}
}
