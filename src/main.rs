use matrix::Matrix;

fn main() {
    println!("Hola mundo que rollo wey");
    let mut matriz1 = Matrix::new(10, 15, 0);
    matriz1.set(5, 5, 40);
    println!("{}", matriz1);
}
