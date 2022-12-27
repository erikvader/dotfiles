#[macro_use]
mod ansistring;
mod table;
mod users;
mod versionsort;

use ansistring::*;
use lazy_static::lazy_static;
// use rand::{thread_rng, Rng};
use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs::{self, DirEntry, Metadata};
use std::os::unix::fs::{MetadataExt, PermissionsExt};
use table::*;
use users::*;
use versionsort::*;

lazy_static! {
    static ref LS_COLORS: String =
        std::env::var("LS_COLORS").unwrap_or_else(|_| "".into());
    static ref TYPE_COLORS: HashMap<&'static str, &'static str> = {
        let mut res = HashMap::new();
        for eq in LS_COLORS.split(':') {
            if let Some((lhs, rhs)) = eq.split_once('=') {
                if !lhs.starts_with("*.") {
                    res.insert(lhs, rhs);
                }
            }
        }
        res
    };
    static ref EXT_COLORS: HashMap<&'static str, &'static str> = {
        let mut res = HashMap::new();
        for eq in LS_COLORS.split(':') {
            if let Some((lhs, rhs)) = eq.split_once('=') {
                if let Some(ext) = lhs.strip_prefix("*.") {
                    res.insert(ext, rhs);
                }
            }
        }
        res
    };
}

const SUFFIXES: &[&str] = &["B ", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei", "Zi", "Yi"];

// trait OptionExt {
//     fn maybe(self) -> Self;
// }

// impl<T> OptionExt for Option<T> {
//     fn maybe(self) -> Self {
//         if thread_rng().gen_bool(0.0) {
//             None
//         } else {
//             self
//         }
//     }
// }

fn is_exec(mode: u32) -> bool {
    (libc::S_IXUSR | libc::S_IXGRP | libc::S_IXOTH) & mode != 0
}

fn file_color(meta: &Metadata, ext: Option<&str>) -> Option<&'static str> {
    let mode = meta.permissions().mode();
    match mode & libc::S_IFMT {
        libc::S_IFDIR => TYPE_COLORS.get("di").copied(),
        libc::S_IFLNK => TYPE_COLORS.get("ln").copied(),
        libc::S_IFIFO => TYPE_COLORS.get("pi").copied(),
        libc::S_IFSOCK => TYPE_COLORS.get("so").copied(),
        libc::S_IFBLK => TYPE_COLORS.get("bd").copied(),
        libc::S_IFCHR => TYPE_COLORS.get("cd").copied(),
        libc::S_IFREG => ext.and_then(|e| EXT_COLORS.get(e).copied()).or_else(|| {
            if is_exec(mode) {
                TYPE_COLORS.get("ex").copied()
            } else {
                None
            }
        }),
        _ => None,
    }
}

fn entry_name(filename: Option<&OsStr>, color: Option<&str>) -> AnsiString {
    if let Some(e) = filename {
        if let Some(n) = e.to_str() {
            let mut res = AnsiString::empty();
            res.push_maybe_code(color);
            res.push_str(n);
            res.push_reset();
            return res;
        }
    }
    "???".into()
}

fn entry_perms(meta: Option<&Metadata>, color: Option<&str>) -> AnsiString {
    if let Some(m) = meta {
        let mode = m.permissions().mode();
        let mut res = AnsiString::empty();

        res.push_maybe_code(color);
        let ftype = mode & libc::S_IFMT;
        res.push_str(match ftype {
            libc::S_IFSOCK => "s",
            libc::S_IFLNK => "l",
            libc::S_IFREG => "-",
            libc::S_IFBLK => "b",
            libc::S_IFDIR => "d",
            libc::S_IFCHR => "c",
            libc::S_IFIFO => "p",
            _ => "?",
        });
        res.push_reset();

        res.push_str(if mode & libc::S_IRUSR > 0 { "r" } else { "-" });
        res.push_str(if mode & libc::S_IWUSR > 0 { "w" } else { "-" });
        res.push_str(if mode & libc::S_IXUSR > 0 { "x" } else { "-" });
        res.push_str(if mode & libc::S_IRGRP > 0 { "r" } else { "-" });
        res.push_str(if mode & libc::S_IWGRP > 0 { "w" } else { "-" });
        res.push_str(if mode & libc::S_IXGRP > 0 { "x" } else { "-" });
        res.push_str(if mode & libc::S_IROTH > 0 { "r" } else { "-" });
        res.push_str(if mode & libc::S_IWOTH > 0 { "w" } else { "-" });
        res.push_str(if mode & libc::S_IXOTH > 0 { "x" } else { "-" });

        return res;
    }
    "??????????".into()
}

fn entry_link(entry: &DirEntry) -> AnsiString {
    // assert entry is symlink
    let path = entry.path();

    if let Ok(point) = fs::read_link(entry.path()) {
        let point_meta = fs::metadata(&point).ok();
        let point_exists = point_meta.is_some();
        let point_color = if !point_exists {
            TYPE_COLORS
                .get("or")
                .copied()
                .or(Some(color_code!(bright fg red)))
        } else {
            let point_ext = path.extension().and_then(|ext| ext.to_str());
            file_color(&point_meta.unwrap(), point_ext)
        };

        let arrow_col = if fs::symlink_metadata(&point)
            .ok()
            .map(|m| m.file_type().is_symlink())
            .unwrap_or(false)
        {
            TYPE_COLORS.get("ln").copied()
        } else {
            None
        };

        let mut res = AnsiString::empty();
        if let Some(col) = arrow_col {
            res.push_code(col);
        } else {
            res.push_fg_bright(Color::Red);
        }
        if !point_exists {
            res.push_blink();
        }
        res.push_str("->");
        res.push_reset();
        res.push_str(" ");
        res.push_ansistring(&entry_name(Some(point.as_os_str()), point_color));
        res
    } else {
        let mut res = AnsiString::empty();
        res.push_fg(Color::Red);
        res.push_blink();
        res.push_str("-x>");
        res.push_reset();
        res
    }
}

fn entry_size(meta: Option<&Metadata>) -> AnsiString {
    let (out, mag) = if let Some(m) = meta {
        let b = m.len() as f64;
        if b == 0.0 {
            ("0".into(), 0)
        } else {
            let mag = b.log(1024.0).floor() as i32;
            if mag == 0 {
                (format!("{}", b), 0)
            } else {
                let d = b / (1024_f64.powi(mag));
                if d >= 10.0 {
                    (format!("{:.0}", d.round()), mag)
                } else {
                    (format!("{:.1}", d), mag)
                }
            }
        }
    } else {
        ("?".into(), 0)
    };

    let mut res = AnsiString::empty();
    res.push_str(&out).push_fg_bright(Color::White);
    if mag >= 0 && (mag as usize) < SUFFIXES.len() {
        res.push_str(SUFFIXES[mag as usize]);
    } else {
        res.push_str("??");
    }
    res.push_reset();
    res
}

fn entry_user(meta: Option<&Metadata>) -> AnsiString {
    if let Some(m) = meta {
        let uid = m.uid();
        let mut res = AnsiString::empty();
        if uid == 0 {
            res.push_fg_bright(Color::Red);
        } else {
            res.push_fg_bright(Color::Magenta);
        }

        if let Some(c) = uid2name(uid).and_then(|name| name.chars().next()) {
            res.push_char(c);
        } else {
            res.push_str(&uid.to_string());
        }

        res.push_reset();
        return res;
    }
    "?".into()
}

fn entry_group(gid: u32) -> AnsiString {
    let mut res = AnsiString::empty();
    res.push_str("(");
    res.push_fg(Color::Yellow);

    if let Some(name) =
        gid2name(gid).and_then(|name| if name.is_empty() { None } else { Some(name) })
    {
        res.push_str(&name);
    } else {
        res.push_str(&gid.to_string());
    }

    res.push_reset();
    res.push_str(")");
    res
}

fn list_dir(path: &str, versionsort: bool) -> bool {
    let mut tab = RaggedTable::<AnsiString>::new();

    let ite = fs::read_dir(path);
    if let Err(e) = ite {
        eprintln!("{}", e);
        return false;
    }

    let mut names = Vec::new();
    let mut users = Vec::new();

    for rde in ite.unwrap() {
        let e = rde.ok();
        let filename = e.as_ref().map(|x| x.file_name());
        let meta = e.as_ref().and_then(|x| x.metadata().ok());
        let path = e.as_ref().map(|x| x.path());
        let ext = path
            .as_ref()
            .and_then(|x| x.extension())
            .and_then(|ext| ext.to_str());
        let color = meta.as_ref().and_then(|m| file_color(m, ext));

        let mut row = vec![
            entry_perms(meta.as_ref(), color),
            entry_user(meta.as_ref()),
            entry_size(meta.as_ref()),
            entry_name(filename.as_deref(), color),
        ];

        if meta.as_ref().map(|m| m.uid() != m.gid()).unwrap_or(false) {
            row.push(entry_group(meta.as_ref().unwrap().gid()));
        }

        if meta
            .as_ref()
            .map(|m| m.file_type().is_symlink())
            .unwrap_or(false)
        {
            row.push(entry_link(&e.unwrap()));
        }

        tab.add_row(row);
        names.push(filename.and_then(|f| f.into_string().ok()));
        users.push(meta.as_ref().map(|m| m.uid()));
    }

    tab.align_column(1, Align::Right);
    tab.align_column(2, Align::Right);

    if versionsort {
        let vs: Vec<_> = names
            .iter()
            .map(|ms| ms.as_ref().map(|s| version_sort(s)))
            .collect();
        tab.sort(&vs);
    } else {
        tab.sort(&names);
    }

    if let Some(Some(the_uid)) =
        users
            .into_iter()
            .reduce(|l, r| if l.is_some() && l == r { l } else { None })
    {
        let my_uid = current_uid();
        if the_uid == my_uid {
            tab.remove_col(1);
        }
    }

    if tab.is_empty() {
        println!("empty...");
    } else {
        println!("{}", tab.to_string(" ", "\n"));
    }
    true
}

fn main() {
    // TODO: parse arguments
    std::process::exit(if list_dir(".", true) { 0 } else { 1 });
}
