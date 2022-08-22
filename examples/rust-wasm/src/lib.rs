#![no_std]
// Because this file uses nothing of the standard library
// Avoids generating a 1.7M file...

use core::panic::PanicInfo;
#[panic_handler]
fn panic(_panic: &PanicInfo<'_>) -> ! { loop {} }

extern "C" {
    fn print_str(ptr: *const u8, len: usize);
}

#[no_mangle]
pub extern "C" fn hello() {
    let msg = "Hello, world!\n";
    unsafe { print_str(msg.as_ptr(), msg.len()); }
}
