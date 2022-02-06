use libc::getgrgid;
use libc::getpwuid;
use libc::getuid;
use std::ffi::CStr;

pub fn uid2name(uid: u32) -> Option<String> {
    let p: *const _ = unsafe { getpwuid(uid) };
    if !p.is_null() {
        let raw_name: *const _ = unsafe { (*p).pw_name };
        if !raw_name.is_null() {
            let cstr = unsafe { CStr::from_ptr(raw_name) };
            return Some(cstr.to_string_lossy().into_owned());
        }
    }
    None
}

pub fn gid2name(gid: u32) -> Option<String> {
    let p: *const _ = unsafe { getgrgid(gid) };
    if !p.is_null() {
        let raw_name: *const _ = unsafe { (*p).gr_name };
        if !raw_name.is_null() {
            let cstr = unsafe { CStr::from_ptr(raw_name) };
            return cstr.to_str().ok().map(|s| s.to_string());
        }
    }
    None
}

pub fn current_uid() -> u32 {
    unsafe { getuid() }
}
