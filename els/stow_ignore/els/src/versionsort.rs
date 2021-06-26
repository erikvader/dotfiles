use std::str::CharIndices;

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone)]
pub enum VSElement<'a> {
    Str(&'a str),
    Num(i64),
}

pub type VersionSort<'a> = Vec<VSElement<'a>>;

pub fn version_sort(filename: &str) -> VersionSort<'_> {
    group_by(&filename, |c| c.is_digit(10))
        .map(|s| match s.parse() {
            Ok(i) => VSElement::Num(i),
            _ => VSElement::Str(s.into()),
        })
        .collect()
}

struct GroupBy<'a, F> {
    chars: CharIndices<'a>,
    string: &'a str,
    grouper: F,
    state: GrpState,
}

enum GrpState {
    Init,
    Run { cur: bool, start: usize, end: usize },
    End,
}

impl<'a, F> Iterator for GroupBy<'a, F>
where
    F: Fn(char) -> bool,
{
    type Item = &'a str;
    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.state {
            GrpState::End => None,
            GrpState::Init => {
                if let Some((s, c)) = self.chars.next() {
                    self.state = GrpState::Run {
                        cur: (self.grouper)(c),
                        start: s,
                        end: c.len_utf8(),
                    };
                    self.next()
                } else {
                    self.state = GrpState::End;
                    None
                }
            }
            GrpState::Run { cur, start, end } => {
                for (s, c) in &mut self.chars {
                    let group = (self.grouper)(c);
                    if group == *cur {
                        *end = s + c.len_utf8();
                    } else {
                        let res = &self.string[*start..*end];
                        *cur = group;
                        *start = s;
                        *end = s + c.len_utf8();
                        return Some(res);
                    }
                }

                let res = &self.string[*start..*end];
                self.state = GrpState::End;
                Some(res)
            }
        }
    }
}

fn group_by<F: Fn(char) -> bool>(s: &str, f: F) -> GroupBy<'_, F> {
    GroupBy {
        chars: s.char_indices(),
        string: s,
        grouper: f,
        state: GrpState::Init,
    }
}
