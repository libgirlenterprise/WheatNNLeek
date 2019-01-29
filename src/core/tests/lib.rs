#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn new_static_synapse() {
        use wheatnnleek::connections::static_connection::Connection;
        use wheatnnleek::connections::Connection as Conn;
        let conn = Connection::default();
        println!("weight: {}, delay: {}", conn.weight(), conn.delay());
    }

    #[test]
    fn new_hh_model() {
        use wheatnnleek::models::hodgkin_huxley::Model;
        let model = Model::default();
        assert_eq!(model.cm, 1.0);
    }

    #[test]
    fn new_iz_model() {
        use wheatnnleek::models::izhikevich::Model;
        let model = Model::default();
        assert_eq!(model.a, 0.02);
    }
}
