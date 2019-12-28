use std::ffi::OsStr;
use std::process::{Stdio, Command};
use std::io::Write;

pub fn fzf_dmenu<S,T,I,J>(choices: J, args: I) -> std::io::Result<Option<String>>
where
    S: AsRef<str>,
    T: AsRef<OsStr>,
    I: IntoIterator<Item = T>,
    J: IntoIterator<Item = S>
{
    let mut child = Command::new("fzf_dmenu")
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;

    {
        let mut to_send = String::new();
        for c in choices {
            to_send.push_str(c.as_ref());
            to_send.push_str("\n");
        }
        let stdin = child.stdin.as_mut().expect("failed to open stdin");
        stdin.write_all(to_send.as_bytes())?;
    }

    let res = child.wait_with_output()?;
    let output = String::from_utf8_lossy(&res.stdout).trim().to_string();
    if output.is_empty() {
        Ok(None)
    } else {
        Ok(Some(output))
    }
}
