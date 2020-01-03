use std::collections::HashMap;
use std::env::args;
use std::ffi::OsString;
use std::fs;
use std::os::linux::fs::MetadataExt;
use std::path::Path;
use std::iter::FromIterator;

struct Walkdir {
   stack: Vec<fs::DirEntry>,
}

impl Walkdir {
   fn new(p: &Path) -> Self {
      let mut s = Walkdir { stack: Vec::new() };
      s.add(p);
      s
   }
   fn add(&mut self, p: &Path) {
      for e in fs::read_dir(p).unwrap() {
         let f = e.unwrap();
         let ft = f.file_type().unwrap();
         if ft.is_dir() || ft.is_file() {
            self.stack.push(f);
         }
      }
   }
   fn pop(&mut self) -> Option<fs::DirEntry> {
      self.stack.pop()
   }
}

impl Iterator for Walkdir {
   type Item = fs::DirEntry;
   fn next(&mut self) -> Option<Self::Item> {
      while !self.stack.is_empty() {
         let n = self.pop().unwrap();
         if n.file_type().unwrap().is_dir() {
            self.add(&n.path());
         } else {
            return Some(n);
         }
      }
      None
   }
}

struct FileLook {
   map: HashMap<OsString, Vec<fs::DirEntry>>,
}

impl FileLook {
   fn new() -> Self {
      FileLook {
         map: HashMap::new(),
      }
   }
   fn add(&mut self, d: fs::DirEntry) {
      let name = d.file_name();
      if !self.map.contains_key(&name) {
         self.map.insert(name.clone(), Vec::new());
      }
      self.map.get_mut(&name).unwrap().push(d);
   }
   fn get(&self, d: &fs::DirEntry) -> Option<&fs::DirEntry> {
      let dm = d.metadata().unwrap();
      if let Some(ref v) = self.map.get(&d.file_name()) {
         for x in v.iter() {
            let xm = x.metadata().unwrap();
            // NOTE: should not compare ctime or btime
            if xm.st_mode() == dm.st_mode()
               && xm.st_size() == dm.st_size()
               && xm.st_mtime() == dm.st_mtime()
               && xm.st_uid() == dm.st_uid()
               && xm.st_gid() == dm.st_gid()
            {
               return Some(x);
            }
         }
      }
      None
   }
}

impl FromIterator<fs::DirEntry> for FileLook {
   fn from_iter<T: IntoIterator<Item = fs::DirEntry>>(iter: T) -> Self {
      let mut res = FileLook::new();
      for i in iter {
         res.add(i);
      }
      res
   }
}

fn print_help(prog_name: &str)
{
   eprintln!("usage: {} working reference [doit]", prog_name);
   eprintln!("");
   eprintln!("This program will try to move files in the directory 'working' to be in the same place relative to 'reference'.");
   eprintln!("It's main purpose is to aid rsync by providing better support for moved files by actually moving them instead of deleting and copying them anew.");
   std::process::exit(1);
}

fn main() {
   let args: Vec<String> = args().collect();
   if args.len() <= 1 || args.len() > 4 {
      eprintln!("wrong number of arguments");
      print_help(&args[0]);
      std::process::exit(1);
   }
   let working = Path::new(&args[1]);
   let reference = Path::new(&args[2]);
   let doit = args.len() == 4 && args[3] == "doit";

   if !working.is_dir() {
      eprintln!("argument 'working' doesn't exist or isn't a directory");
      print_help(&args[0]);
   }

   if !reference.is_dir() {
      eprintln!("argument 'reference' doesn't exist or isn't a directory");
      print_help(&args[0]);
   }

   if args.len() == 4 && args[3] != "doit" {
      eprintln!("the last argument must be the string 'doit' or empty");
      print_help(&args[0]);
   }

   let ref_files: FileLook = Walkdir::new(reference).collect();

   for w in Walkdir::new(working) {
      if let Some(r) = ref_files.get(&w) {
         let r_path = r.path();
         let r_strip = r_path.strip_prefix(reference).unwrap();
         let w_path = w.path();
         let w_strip = w_path.strip_prefix(working).unwrap();

         if r_strip != w_strip {
            let src = &w_path;
            let dst = working.join(r_strip);
            println!("{} -> {}", src.display(), dst.display());
            if dst.exists() {
               println!("dst already exists, ignoring...");
            } else {
               if doit {
                  fs::create_dir_all(dst.parent().unwrap()).expect("couldn't create all dirs");
                  fs::rename(src, dst).expect("couldn't rename");
               }
            }
         }

      }
   }
}
