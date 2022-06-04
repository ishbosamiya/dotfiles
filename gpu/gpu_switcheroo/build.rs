fn main() {
    if !cfg!(unix) {
        panic!("Only unix systems are supported.");
    }
}
