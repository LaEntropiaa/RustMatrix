use num_traits::{Num, NumAssign};
use core::panic;
use std::fmt::{self, Debug};
use std::ops::Add;
use std::ops::Sub;
use std::ops::Mul;


#[derive(PartialEq, Eq, Debug)]
pub struct Matrix<T: Num + NumAssign + fmt::Display + Copy + Eq + PartialEq + Debug> {
    rows: usize,
    columns: usize,
    data: Vec<T>,
}

impl<T: Num + NumAssign + fmt::Display + Copy + Eq + PartialEq + Debug> Matrix<T> {
    pub fn new(rows: usize, columns: usize, default: T) -> Self {
        Self {
            rows,
            columns,
            data: vec![default; rows * columns],
        }
    }

    pub fn get(&self, row: usize, column: usize) -> &T {
        if row >= self.rows || column >= self.columns {
            panic!("Index given is out of range.")
        }
        let mut index = 0;
        index += row * self.columns;
        index += column;
        return &self.data[index];
    }

    pub fn get_row(&self, row: usize) -> Vec<T> {
        if row >= self.rows {
            panic!("Row index is out of bounds.");
        }
        
        let mut index = 0;
        let mut data = Vec::new();

        
        index += self.columns * row;
        for i in 0..self.columns {
            data.push(self.data[index + i]);
        }

        return data;
    }

    pub fn get_column(&self, column: usize) -> Vec<T> {
        if column >= self.columns {
            panic!("Column index is out of bounds.");
        }

        let index = column;
        let mut data = Vec::new();

        for i in 0..self.rows {
            data.push(self.data[(i * self.columns) + index])
        }

        return data;
    }

    pub fn set(&mut self, row: usize, column: usize, data: T) -> () {
        if row >= self.rows || column >= self.columns {
            panic!("Index given is out of range.")
        }
        let mut index = 0;
        index += row * self.columns;
        index += column;
        self.data[index] = data;
        return;
    }
    
    pub fn exchange_rows(&mut self, row1: usize, row2: usize) -> () {
        if row1 >= self.rows || row2 >= self.rows {
            panic!("Row index is out of bounds.");
        }

        //Get copy of row2
        let temp = self.get_row(row2);
        let mut index1 = 0;
        let mut index2 = 0;
        //Move from row1 to row2
        index1 += self.columns * row1;
        index2 += self.columns * row2;
        for i in 0..self.columns {
            self.data[index2 + i] = self.data[index1 + i];
            self.data[index1 + i] = temp[i]; 
        }
    }

    pub fn exchange_columns(&mut self, column1: usize, column2: usize) -> () {
        if column1 >= self.columns || column2 >= self.columns {
            panic!("Column index is out of bounds.")
        }

        //Get copy of column2
        let temp = self.get_column(column2);
        for i in 0..self.rows {
            self.data[column2 + (i * self.columns)] = self.data[column1 + (i * self.columns)];
            self.data[column1 + (i * self.columns)] = temp[i];
        }

    }

    pub fn get_determinant(&self) -> T {
        if self.rows != self.columns {
            panic!("Only nxn matrixes can have a determinant.");
        }

        let mut trig_matrix = Matrix {
            columns: self.columns,
            rows: self.rows,
            data: self.data.clone(),
        }; 
        let mut sign = 1.0;
        
        for i in 0..self.columns {
            let mut pivot = self.get(i, i);

            // Assign x to next column
            let mut x = i + 1;
            while pivot.is_zero() && x < self.columns {

            } 
        }

        return self.data[0];
    }
}

impl<T: Num + NumAssign + fmt::Display + Copy + Eq + PartialEq + Debug> Add for Matrix<T> {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        if self.data.len() != other.data.len() {
            panic!("Matrix size is inadecuate.");
        }

        let mut new_data = Vec::new();
        for i in 0..self.data.len() {
            new_data.push(self.data[i] + other.data[i]);
        }

        Matrix {
            columns: self.columns,
            rows: self.rows,
            data: new_data,
        }
    }
}

impl<T: Num + NumAssign + fmt::Display + Copy + Eq + PartialEq + Debug> Sub for Matrix<T> {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        if self.data.len() != other.data.len() {
            panic!("Matrix size is inadecuate.");
        }

        let mut new_data = Vec::new();
        for i in 0..self.data.len() {
            new_data.push(self.data[i] - other.data[i]);
        }

        Matrix {
            columns: self.columns,
            rows: self.rows,
            data: new_data,
        }
    }
}

impl<T: Num + NumAssign + fmt::Display + Copy + Eq + PartialEq + Debug> Mul for Matrix<T> {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        if self.columns != other.rows {
            panic!("Matrix dimentions are inadecuate.");
        }
        
        let mut new_data: Vec<T> = Vec::new();
        for i in 0..self.rows {
            let current_row = self.get_row(i);

            for k in 0..other.columns {
                let current_column = other.get_column(k);
                let mut new_value = T::zero();
                for (a, b) in current_row.iter().zip(current_column.iter()) {
                    new_value += *a * *b;
                }
                new_data.push(new_value);
            }
        }

        Matrix {
            rows: self.rows,
            columns: other.columns,
            data: new_data,
        }    
    }   
}

impl<T: Num + NumAssign + fmt::Display + Copy + Eq + PartialEq + Debug> fmt::Display for Matrix<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut display = String::new();
        let mut index = 0;
        for _i in 0..self.columns {
            display += "{";
            for _k in 0..self.rows {
                display += &format!(" {},", self.data[index]);
                index += 1;
            }
            display += " }\n";
        }
        write!(f, "{}", display)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sum_two_matrix_1() {
        let mut matrix1 = Matrix::new(4, 4, 2);
        let matrix2 = Matrix::new(4, 4, 3);
        let result_matrix = Matrix::new(4, 4, 5);

        matrix1 = matrix1 + matrix2;

        assert_eq!(matrix1, result_matrix);
    }

    #[test]
    fn sum_two_matrix_2() {
        let mut matrix1 = Matrix::new(4, 4, 2);
        let mut matrix2 = Matrix::new(4, 4, 0);
        let mut result_matrix = Matrix::new(4, 4, 2);
    
        result_matrix.set(0, 0, 9);
        matrix2.set(0, 0, 7);
        matrix1 = matrix1 + matrix2;

        assert_eq!(matrix1, result_matrix);
    }

    #[test]
    #[should_panic]
    fn sum_two_matrix_3() {
        let mut matrix1 = Matrix::new(4, 4, 2);
        let matrix2 = Matrix::new(4, 5, 0);

        matrix1 = matrix1 + matrix2;
    }

    #[test]
    fn substract_two_matrix_1() {
        let mut matrix1 = Matrix::new(4, 4, 2);
        let matrix2 = Matrix::new(4, 4, 3);
        let result_matrix = Matrix::new(4, 4, -1);

        matrix1 = matrix1 - matrix2;

        assert_eq!(matrix1, result_matrix);
    }

    #[test]
    fn substract_two_matrix_2() {
        let mut matrix1 = Matrix::new(4, 4, 2);
        let mut matrix2 = Matrix::new(4, 4, 0);
        let mut result_matrix = Matrix::new(4, 4, 2);

        result_matrix.set(0, 0, -5);
        matrix2.set(0, 0, 7);
        matrix1 = matrix1 - matrix2;

        assert_eq!(matrix1, result_matrix);
    }

    #[test]
    #[should_panic]
    fn substract_two_matrix_3() {
        let mut matrix1 = Matrix::new(4, 4, 2);
        let matrix2 = Matrix::new(4, 5, 0);

        matrix1 = matrix1 - matrix2;
    }

    #[test]
    fn get_row_1() {
        let mut matrix1 = Matrix::new(3, 4, 0);
        matrix1.set(0, 0, 8);
        matrix1.set(0, 1, 7);
        matrix1.set(0, 2, 9);
        matrix1.set(0, 3, 6);

        let data = vec![8, 7 , 9, 6];

        assert_eq!(matrix1.get_row(0), data);
    }

    #[test]
    fn get_row_2() {
        let mut matrix1 = Matrix::new(4, 4, 0);
        
        matrix1.set(3, 0, 8);
        matrix1.set(3, 1, 7);
        matrix1.set(3, 2, 9);
        matrix1.set(3, 3, 6);

        let data = vec![8, 7, 9, 6];

        assert_eq!(matrix1.get_row(3), data);
    }

    #[test]
    #[should_panic]
    fn get_row_3() {
        let matrix1 = Matrix::new(4, 4, 0);

        let _data = matrix1.get_row(4);
    }

     #[test]
    fn get_column_1() {
        let mut matrix1 = Matrix::new(4, 3, 0);
        matrix1.set(0, 0, 8);
        matrix1.set(1, 0, 7);
        matrix1.set(2, 0, 9);
        matrix1.set(3, 0, 6);

        let data = vec![8, 7 , 9, 6];

        assert_eq!(matrix1.get_column(0), data);
    }

    #[test]
    fn get_column_2() {
        let mut matrix1 = Matrix::new(4, 4, 0);
        
        matrix1.set(0, 3, 8);
        matrix1.set(1, 3, 7);
        matrix1.set(2, 3, 9);
        matrix1.set(3, 3, 6);

        let data = vec![8, 7, 9, 6];

        assert_eq!(matrix1.get_column(3), data);
    }

    #[test]
    #[should_panic]
    fn get_column_3() {
        let matrix1 = Matrix::new(4, 4, 0);

        let _data = matrix1.get_column(4);
    }

    #[test]
    fn mult_matrix_1() {
        let mut matrix1 = Matrix::new(4, 4, 2);
        let matrix2 = Matrix::new(4, 4, 2);
        let result_matrix = Matrix::new(4, 4, 16);

        matrix1 = matrix1 * matrix2;

        assert_eq!(matrix1, result_matrix);
    }

    #[test]
    fn mult_matrix_2() {
        let mut matrix1 = Matrix::new(4, 4, 2);
        let mut matrix2 = Matrix::new(4, 3, 4);
        let mut result_matrix = Matrix::new(4, 3, 0);

        matrix1.set(0, 0, 6);
        matrix1.set(0, 1, 8);
        matrix1.set(0, 2, 9);
        matrix1.set(0, 3, 5);
        matrix1.set(1, 0, 3);
        matrix1.set(1, 1, 8);
        matrix1.set(1, 2, 4);
        matrix1.set(1, 3, 7);
        matrix1.set(2, 0, 4);
        matrix1.set(2, 1, 5);
        matrix1.set(2, 2, 6);
        matrix1.set(2, 3, 4);
        matrix1.set(3, 0, 6);
        matrix1.set(3, 1, 2);
        matrix1.set(3, 2, 2);
        matrix1.set(3, 3, 9);
        
        matrix2.set(0, 0, 7);
        matrix2.set(0, 1, 6);
        matrix2.set(0, 2, 1);
        matrix2.set(1, 0, 6);
        matrix2.set(1, 1, 4);
        matrix2.set(1, 2, 8);
        matrix2.set(2, 0, 3);
        matrix2.set(2, 1, 0);
        matrix2.set(2, 2, 6);
        matrix2.set(3, 0, 1);
        matrix2.set(3, 1, 1);
        matrix2.set(3, 2, 1);

        result_matrix.set(0, 0, 122);
        result_matrix.set(0, 1, 73);
        result_matrix.set(0, 2, 129);
        result_matrix.set(1, 0, 88);
        result_matrix.set(1, 1, 57);
        result_matrix.set(1, 2, 98);
        result_matrix.set(2, 0, 80);
        result_matrix.set(2, 1, 48);
        result_matrix.set(2, 2, 84);
        result_matrix.set(3, 0, 69);
        result_matrix.set(3, 1, 53);
        result_matrix.set(3, 2, 43);

        matrix1 = matrix1 * matrix2;

        assert_eq!(result_matrix, matrix1);
    }

    #[test]
    fn exchange_rows_1() {
        let mut matrix = Matrix::new(3, 4, 5);
        matrix.set(0, 0, 1);
        matrix.set(0, 1, 2);
        matrix.set(0, 2, 3);
        matrix.set(0, 3, 4);
        matrix.set(1, 0, 5);
        matrix.set(1, 1, 6);
        matrix.set(1, 2, 7);
        matrix.set(1, 3, 8);

        matrix.exchange_rows(0, 1);

        let row1 = vec![1, 2, 3, 4];
        let row2 = vec![5, 6, 7, 8];

        assert_eq!(row1, matrix.get_row(1));
        assert_eq!(row2, matrix.get_row(0));
    }
    
    #[test]
    fn exchange_rows_2() {
        let mut matrix = Matrix::new(3, 4, 5);
        matrix.set(0, 0, 1);
        matrix.set(0, 1, 2);
        matrix.set(0, 2, 3);
        matrix.set(0, 3, 4);
        matrix.set(2, 0, 5);
        matrix.set(2, 1, 6);
        matrix.set(2, 2, 7);
        matrix.set(2, 3, 8);

        matrix.exchange_rows(0, 2);

        let row1 = vec![1, 2, 3, 4];
        let row2 = vec![5, 6, 7, 8];

        assert_eq!(row1, matrix.get_row(2));
        assert_eq!(row2, matrix.get_row(0));
    }
    
    #[test]
    fn exchange_columns_1() {
        let mut matrix = Matrix::new(3, 4, 5);
        matrix.set(0, 0, 1);
        matrix.set(1, 0, 2);
        matrix.set(2, 0, 3);
        matrix.set(0, 1, 4);
        matrix.set(1, 1, 5);
        matrix.set(2, 1, 6);

        matrix.exchange_columns(0, 1);

        let column1 = vec![1, 2, 3];
        let column2 = vec![4, 5, 6];

        assert_eq!(column1, matrix.get_column(1));
        assert_eq!(column2, matrix.get_column(0));
    }

    #[test]
    fn exchange_columns_2() {
        let mut matrix = Matrix::new(3, 4, 5);
        matrix.set(0, 0, 1);
        matrix.set(1, 0, 2);
        matrix.set(2, 0, 3);
        matrix.set(0, 3, 4);
        matrix.set(1, 3, 5);
        matrix.set(2, 3, 6);

        matrix.exchange_columns(0, 3);

        let column1 = vec![1, 2, 3];
        let column2 = vec![4, 5, 6];

        assert_eq!(column1, matrix.get_column(3));
        assert_eq!(column2, matrix.get_column(0));
    }
}
