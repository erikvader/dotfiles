#[allow(dead_code)]
mod x11_stuff;
mod fzf_dmenu;

use x11_stuff::*;
use tabular::{Table, Row};
use fzf_dmenu::fzf_dmenu;

fn main() {
    // get current state
    let conn = get_conn();
    let desktops = get_desktops(&conn);
    let active_window = get_active_window(&conn);
    let all_windows = get_all_windows(&conn);

    // build table
    let mut table = Table::new("{:<}  {:<}  {:<}{:<}  {:<}");
    for (i, w) in all_windows.iter().enumerate() {
        let (instance, class) = w.get_instance_class();
        table.add_row(Row::new()
                      .with_cell(i)
                      .with_cell(&desktops[w.get_desktop_index() as usize])
                      .with_cell(if active_window.as_ref()
                                 .map_or(false, |aw| *aw == *w) {"*"} else {""})
                      .with_cell(format!("{}({})", class, instance))
                      .with_cell(w.get_name()));
    }

    // start fzf_dmenu
    let table_str = table.to_string();
    let res = fzf_dmenu(table_str.split("\n"), &["--nth=2..", "--with-nth=2.."]).unwrap();
    if let Some(selected) = res {
        // select selected window
        let inds: String = selected.chars().take_while(|c| c.is_digit(10)).collect();
        let ind: usize = inds.parse().unwrap();
        all_windows[ind].focus();
    }
}
