use xcb;
use xcb_util::{icccm, ewmh};

pub struct Connection {
    conn: ewmh::Connection,
    screen: i32
}

pub struct Window<'a> {
    id: xcb::Window,
    conn: &'a Connection
}

impl<'a> Window<'a> {
    fn new(id: xcb::Window, conn: &'a Connection) -> Self {
        Window{id: id, conn: conn}
    }

    pub fn get_name(&self) -> String {
        ewmh::get_wm_name(&self.conn.conn, self.id)
            .get_reply()
            .map(|r| r.string().to_string())
            .or_else(|_| icccm::get_wm_name(&self.conn.conn, self.id)
                     .get_reply()
                     .map(|r| r.name().to_string()))
            .expect("this window doesn't have any name/title")
    }

    pub fn get_instance_class(&self) -> (String, String) {
        let rep = icccm::get_wm_class(&self.conn.conn, self.id)
            .get_reply()
            .expect("this windows doesn't have an class and instance");
        (rep.instance().to_string(), rep.class().to_string())
    }

    pub fn get_class(&self) -> String {
        self.get_instance_class().1
    }

    pub fn get_instance(&self) -> String {
        self.get_instance_class().0
    }

    pub fn focus(&self) {
        let res = ewmh::request_change_active_window(
            &self.conn.conn,
            self.conn.screen,
            self.id,
            xcb_util::ffi::ewmh::XCB_EWMH_CLIENT_SOURCE_TYPE_NORMAL,
            0,
            0
        ).request_check();
        if res.is_err() {
            eprintln!("Could not focus window with id {}", self.id);
        }
    }

    pub fn get_desktop_index(&self) -> u32 {
        ewmh::get_wm_desktop(&self.conn.conn, self.id)
            .get_reply()
            .unwrap()
    }
}

impl PartialEq for Window<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Window<'_> {}

pub fn get_conn() -> Connection {
    let (tmp, screen_num) = xcb::Connection::connect(None).unwrap();
    // can't use unwrap or expect :(
    let conn = if let Ok(asd) = ewmh::Connection::connect(tmp) {
        asd
    } else {
        panic!("Failed to create a ewmh::Connection");
    };
    Connection{ conn: conn, screen: screen_num }
}

pub fn get_active_window(conn: &Connection) -> Option<Window> {
    ewmh::get_active_window(&conn.conn, conn.screen)
        .get_reply()
        .map(|w| Window::new(w, conn))
        .ok()
}

pub fn get_all_windows(conn: &Connection) -> Vec<Window> {
    ewmh::get_client_list(&conn.conn, conn.screen)
        .get_reply()
        .unwrap()
        .windows()
        .iter()
        .map(|w| Window::new(*w, conn))
        .collect()
}

pub fn get_desktops(conn: &Connection) -> Vec<String> {
    ewmh::get_desktop_names(&conn.conn, conn.screen)
        .get_reply()
        .unwrap()
        .strings()
        .into_iter()
        .map(|s| s.to_string())
        .collect()
}
