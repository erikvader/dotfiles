use crate::ansistring::{AsString, Len};

pub struct RaggedTable<T> {
    rows: Vec<Vec<T>>,
}

impl<T> RaggedTable<T> {
    #![allow(dead_code)]
    pub fn new() -> Self {
        RaggedTable { rows: Vec::new() }
    }

    pub fn add_row(&mut self, row: Vec<T>) {
        self.rows.push(row);
    }

    pub fn minimum_length(&self) -> usize {
        self.rows.iter().map(|r| r.len()).min().unwrap_or(0)
    }

    pub fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }

    pub fn col_iter(&self, col: usize) -> impl Iterator<Item = &T> + '_ {
        self.rows.iter().map(move |r| &r[col])
    }

    pub fn col_iter_mut(&mut self, col: usize) -> impl Iterator<Item = &mut T> + '_ {
        self.rows.iter_mut().map(move |r| &mut r[col])
    }

    pub fn remove_col(&mut self, col: usize) {
        for r in self.rows.iter_mut() {
            r.remove(col);
        }
    }

    pub fn sort<K: Ord>(&mut self, keys: &[K]) {
        assert_eq!(self.rows.len(), keys.len());
        let len = keys.len();
        let mut order = {
            // position i should have element at v[i]
            let mut v: Vec<usize> = (0..len).collect();
            v.sort_unstable_by_key(|i| &keys[*i]);

            // element at i should go to v_inv[i]
            let mut v_inv: Vec<usize> = vec![0; len];
            for i in 0..len {
                v_inv[v[i]] = i;
            }
            v_inv
        };

        for i in 0..len {
            while order[i] != i {
                let dst = order[i];
                self.rows.swap(i, dst);
                order.swap(i, dst);
            }
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Align {
    Left,
    Right,
}

impl<T> RaggedTable<T>
where
    T: Len + AsString,
{
    pub fn align_column(&mut self, col: usize, align: Align) {
        if self.is_empty() {
            return;
        }

        let longest_len = self
            .rows
            .iter()
            .map(|r| r[col].char_len())
            .max()
            .expect("non-empty");

        for e in self.col_iter_mut(col) {
            let l = longest_len - e.char_len();
            let spaces = std::iter::repeat(' ').take(l);
            match align {
                Align::Left => e.as_string().extend(spaces),
                Align::Right => e.as_string().insert_str(0, &spaces.collect::<String>()),
            }
        }
    }
}

impl<T> RaggedTable<T>
where
    T: AsRef<str>,
{
    pub fn to_string(&self, col_sep: &str, row_sep: &str) -> String {
        let mut res = String::new();

        let mut first_row = true;
        for r in &self.rows {
            if first_row {
                first_row = false;
            } else {
                res.push_str(row_sep);
            }

            let mut first_col = true;
            for c in r {
                if first_col {
                    first_col = false;
                } else {
                    res.push_str(col_sep);
                }
                res.push_str(c.as_ref());
            }
        }

        res
    }
}
