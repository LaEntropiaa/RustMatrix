use core::panic;
use num_traits::{Float, Num, NumAssign, Signed};
use std::fmt::{self, Debug};
use std::ops::Add;
use std::ops::Mul;
use std::ops::Sub;

#[derive(PartialEq, Eq, Debug)]
pub struct Matrix<
    T: Num
        + NumAssign
        + Signed
        + Float
        + fmt::Display
        + Copy
        + PartialEq
        + Debug
        + std::iter::Product<T>,
> {
    rows: usize,
    columns: usize,
    data: Vec<T>,
}

impl<
        T: Num
            + NumAssign
            + Signed
            + Float
            + fmt::Display
            + Copy
            + PartialEq
            + Debug
            + std::iter::Product<T>,
    > Matrix<T>
{
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

    pub fn get_diagonal(&self) -> Vec<T> {
        if self.columns != self.rows {
            panic!("The matrix needs to be squared for getting diagonal.")
        }

        let mut data = Vec::new();

        let mut index = 0;
        for i in 0..self.columns {
            index = i + (i * self.columns);
            data.push(self.data[index]);
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

    pub fn set_row(&mut self, row: usize, data: Vec<T>) -> () {
        if row >= self.rows {
            panic!("Row index given is out of bounds.")
        }
        if data.len() != self.columns {
            panic!("Data is not the required size")
        }

        for i in 0..data.len() {
            self.set(row, i, data[i]);
        }
    }

    pub fn set_column(&mut self, column: usize, data: Vec<T>) -> () {
        if column >= self.columns {
            panic!("Column index given is out of bouds.")
        }
        if data.len() != self.rows {
            panic!("Data is not the required size")
        }

        for i in 0..data.len() {
            self.set(i, column, data[i]);
        }
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
        let mut sign = T::one();

        for i in 0..self.columns {
            let mut pivot = *trig_matrix.get(i, i);

            // Assign x to next row
            let mut x = i + 1;
            while pivot.is_zero() && x < self.rows {
                if !trig_matrix.get(x, i).is_zero() {
                    trig_matrix.exchange_rows(x, i);
                    sign = -sign;
                    pivot = *trig_matrix.get(i, i);
                    break;
                }
                x += 1;
            }
            // Restart x for exchanging columns if necessary
            x = i + 1;
            while pivot.is_zero() && x < self.columns {
                if !trig_matrix.get(i, x).is_zero() {
                    trig_matrix.exchange_columns(i, x);
                    sign = -sign;
                    pivot = *trig_matrix.get(i, i);
                    break;
                }
                x += 1;
            }
            // If even exchanging in all ways posible pivot is still 0
            // then determinant is 0
            if pivot.is_zero() {
                return T::zero();
            }

            //So, we got the pivot, now we evaluate to 0 to create the
            //triangular matrix
            x = i + 1;
            while x < trig_matrix.rows {
                let m = *trig_matrix.get(x, i) / pivot;
                let new_row = trig_matrix
                    .get_row(x)
                    .iter()
                    .zip(trig_matrix.get_row(i).iter())
                    .map(|(a, b)| *a - m * *b)
                    .collect::<Vec<T>>();
                trig_matrix.set_row(x, new_row);
                x += 1;
            }
        }

        // YES, now we got ourselves a triangular matrix, now we just
        // take the product of the diagonal and multiply by sign, that's
        // the determinant :)
        let determinant = sign * trig_matrix.get_diagonal().iter().copied().product::<T>();

        return determinant;
    }
}

impl<
        T: Num
            + NumAssign
            + Signed
            + Float
            + fmt::Display
            + Copy
            + PartialEq
            + Debug
            + std::iter::Product<T>,
    > Add for Matrix<T>
{
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

impl<
        T: Num
            + NumAssign
            + Signed
            + Float
            + fmt::Display
            + Copy
            + PartialEq
            + Debug
            + std::iter::Product<T>,
    > Sub for Matrix<T>
{
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

impl<
        T: Num
            + NumAssign
            + Signed
            + Float
            + fmt::Display
            + Copy
            + PartialEq
            + Debug
            + std::iter::Product<T>,
    > Mul for Matrix<T>
{
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

impl<
        T: Num
            + NumAssign
            + Signed
            + Float
            + fmt::Display
            + Copy
            + PartialEq
            + Debug
            + std::iter::Product<T>,
    > fmt::Display for Matrix<T>
{
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
        let mut matrix1 = Matrix::new(4, 4, 2.0);
        let matrix2 = Matrix::new(4, 4, 3.0);
        let result_matrix = Matrix::new(4, 4, 5.0);

        matrix1 = matrix1 + matrix2;

        assert_eq!(matrix1, result_matrix);
    }

    #[test]
    fn sum_two_matrix_2() {
        let mut matrix1 = Matrix::new(4, 4, 2.0);
        let mut matrix2 = Matrix::new(4, 4, 0.0);
        let mut result_matrix = Matrix::new(4, 4, 2.0);

        result_matrix.set(0, 0, 9.0);
        matrix2.set(0, 0, 7.0);
        matrix1 = matrix1 + matrix2;

        assert_eq!(matrix1, result_matrix);
    }

    #[test]
    #[should_panic]
    fn sum_two_matrix_3() {
        let mut matrix1 = Matrix::new(4, 4, 2.0);
        let matrix2 = Matrix::new(4, 5, 0.0);

        matrix1 = matrix1 + matrix2;
    }

    #[test]
    fn substract_two_matrix_1() {
        let mut matrix1 = Matrix::new(4, 4, 2.0);
        let matrix2 = Matrix::new(4, 4, 3.0);
        let result_matrix = Matrix::new(4, 4, -1.0);

        matrix1 = matrix1 - matrix2;

        assert_eq!(matrix1, result_matrix);
    }

    #[test]
    fn substract_two_matrix_2() {
        let mut matrix1 = Matrix::new(4, 4, 2.0);
        let mut matrix2 = Matrix::new(4, 4, 0.0);
        let mut result_matrix = Matrix::new(4, 4, 2.0);

        result_matrix.set(0, 0, -5.0);
        matrix2.set(0, 0, 7.0);
        matrix1 = matrix1 - matrix2;

        assert_eq!(matrix1, result_matrix);
    }

    #[test]
    #[should_panic]
    fn substract_two_matrix_3() {
        let mut matrix1 = Matrix::new(4, 4, 2.0);
        let matrix2 = Matrix::new(4, 5, 0.0);

        matrix1 = matrix1 - matrix2;
    }

    #[test]
    fn get_row_1() {
        let mut matrix1 = Matrix::new(3, 4, 0.0);
        matrix1.set(0, 0, 8.0);
        matrix1.set(0, 1, 7.0);
        matrix1.set(0, 2, 9.0);
        matrix1.set(0, 3, 6.0);

        let data = vec![8.0, 7.0, 9.0, 6.0];

        assert_eq!(matrix1.get_row(0), data);
    }

    #[test]
    fn get_row_2() {
        let mut matrix1 = Matrix::new(3, 4, 0.0);

        matrix1.set(2, 0, 8.0);
        matrix1.set(2, 1, 7.0);
        matrix1.set(2, 2, 9.0);
        matrix1.set(2, 3, 6.0);

        let data = vec![8.0, 7.0, 9.0, 6.0];

        assert_eq!(matrix1.get_row(2), data);
    }

    #[test]
    #[should_panic]
    fn get_row_3() {
        let matrix1 = Matrix::new(3, 4, 0.0);

        let _data = matrix1.get_row(4);
    }

    #[test]
    fn get_column_1() {
        let mut matrix1 = Matrix::new(4, 3, 0.0);
        matrix1.set(0, 0, 8.0);
        matrix1.set(1, 0, 7.0);
        matrix1.set(2, 0, 9.0);
        matrix1.set(3, 0, 6.0);

        let data = vec![8.0, 7.0, 9.0, 6.0];

        assert_eq!(matrix1.get_column(0), data);
    }

    #[test]
    fn get_column_2() {
        let mut matrix1 = Matrix::new(4, 3, 0.0);

        matrix1.set(0, 2, 8.0);
        matrix1.set(1, 2, 7.0);
        matrix1.set(2, 2, 9.0);
        matrix1.set(3, 2, 6.0);

        let data = vec![8.0, 7.0, 9.0, 6.0];

        assert_eq!(matrix1.get_column(2), data);
    }

    #[test]
    #[should_panic]
    fn get_column_3() {
        let matrix1 = Matrix::new(4, 4, 0.0);

        let _data = matrix1.get_column(4);
    }

    #[test]
    fn mult_matrix_1() {
        let mut matrix1 = Matrix::new(4, 4, 2.0);
        let matrix2 = Matrix::new(4, 4, 2.0);
        let result_matrix = Matrix::new(4, 4, 16.0);

        matrix1 = matrix1 * matrix2;

        assert_eq!(matrix1, result_matrix);
    }

    #[test]
    fn mult_matrix_2() {
        let mut matrix1 = Matrix::new(4, 4, 2.0);
        let mut matrix2 = Matrix::new(4, 3, 4.0);
        let mut result_matrix = Matrix::new(4, 3, 0.0);

        matrix1.set(0, 0, 6.0);
        matrix1.set(0, 1, 8.0);
        matrix1.set(0, 2, 9.0);
        matrix1.set(0, 3, 5.0);
        matrix1.set(1, 0, 3.0);
        matrix1.set(1, 1, 8.0);
        matrix1.set(1, 2, 4.0);
        matrix1.set(1, 3, 7.0);
        matrix1.set(2, 0, 4.0);
        matrix1.set(2, 1, 5.0);
        matrix1.set(2, 2, 6.0);
        matrix1.set(2, 3, 4.0);
        matrix1.set(3, 0, 6.0);
        matrix1.set(3, 1, 2.0);
        matrix1.set(3, 2, 2.0);
        matrix1.set(3, 3, 9.0);

        matrix2.set(0, 0, 7.0);
        matrix2.set(0, 1, 6.0);
        matrix2.set(0, 2, 1.0);
        matrix2.set(1, 0, 6.0);
        matrix2.set(1, 1, 4.0);
        matrix2.set(1, 2, 8.0);
        matrix2.set(2, 0, 3.0);
        matrix2.set(2, 1, 0.0);
        matrix2.set(2, 2, 6.0);
        matrix2.set(3, 0, 1.0);
        matrix2.set(3, 1, 1.0);
        matrix2.set(3, 2, 1.0);

        result_matrix.set(0, 0, 122.0);
        result_matrix.set(0, 1, 73.0);
        result_matrix.set(0, 2, 129.0);
        result_matrix.set(1, 0, 88.0);
        result_matrix.set(1, 1, 57.0);
        result_matrix.set(1, 2, 98.0);
        result_matrix.set(2, 0, 80.0);
        result_matrix.set(2, 1, 48.0);
        result_matrix.set(2, 2, 84.0);
        result_matrix.set(3, 0, 69.0);
        result_matrix.set(3, 1, 53.0);
        result_matrix.set(3, 2, 43.0);

        matrix1 = matrix1 * matrix2;

        assert_eq!(result_matrix, matrix1);
    }

    #[test]
    fn exchange_rows_1() {
        let mut matrix = Matrix::new(3, 4, 5.0);
        matrix.set(0, 0, 1.0);
        matrix.set(0, 1, 2.0);
        matrix.set(0, 2, 3.0);
        matrix.set(0, 3, 4.0);
        matrix.set(1, 0, 5.0);
        matrix.set(1, 1, 6.0);
        matrix.set(1, 2, 7.0);
        matrix.set(1, 3, 8.0);

        matrix.exchange_rows(0, 1);

        let row1 = vec![1.0, 2.0, 3.0, 4.0];
        let row2 = vec![5.0, 6.0, 7.0, 8.0];

        assert_eq!(row1, matrix.get_row(1));
        assert_eq!(row2, matrix.get_row(0));
    }

    #[test]
    fn exchange_rows_2() {
        let mut matrix = Matrix::new(3, 4, 5.0);
        matrix.set(0, 0, 1.0);
        matrix.set(0, 1, 2.0);
        matrix.set(0, 2, 3.0);
        matrix.set(0, 3, 4.0);
        matrix.set(2, 0, 5.0);
        matrix.set(2, 1, 6.0);
        matrix.set(2, 2, 7.0);
        matrix.set(2, 3, 8.0);

        matrix.exchange_rows(0, 2);

        let row1 = vec![1.0, 2.0, 3.0, 4.0];
        let row2 = vec![5.0, 6.0, 7.0, 8.0];

        assert_eq!(row1, matrix.get_row(2));
        assert_eq!(row2, matrix.get_row(0));
    }

    #[test]
    fn exchange_columns_1() {
        let mut matrix = Matrix::new(3, 4, 5.0);
        matrix.set(0, 0, 1.0);
        matrix.set(1, 0, 2.0);
        matrix.set(2, 0, 3.0);
        matrix.set(0, 1, 4.0);
        matrix.set(1, 1, 5.0);
        matrix.set(2, 1, 6.0);

        matrix.exchange_columns(0, 1);

        let column1 = vec![1.0, 2.0, 3.0];
        let column2 = vec![4.0, 5.0, 6.0];

        assert_eq!(column1, matrix.get_column(1));
        assert_eq!(column2, matrix.get_column(0));
    }

    #[test]
    fn exchange_columns_2() {
        let mut matrix = Matrix::new(3, 4, 5.0);
        matrix.set(0, 0, 1.0);
        matrix.set(1, 0, 2.0);
        matrix.set(2, 0, 3.0);
        matrix.set(0, 3, 4.0);
        matrix.set(1, 3, 5.0);
        matrix.set(2, 3, 6.0);

        matrix.exchange_columns(0, 3);

        let column1 = vec![1.0, 2.0, 3.0];
        let column2 = vec![4.0, 5.0, 6.0];

        assert_eq!(column1, matrix.get_column(3));
        assert_eq!(column2, matrix.get_column(0));
    }

    #[test]
    fn set_row_1() {
        let mut matrix = Matrix::new(3, 4, 5.0);
        let vec1 = vec![1.0, 2.0, 3.0, 4.0];
        matrix.set_row(0, vec1);

        let vec2 = vec![1.0, 2.0, 3.0, 4.0];
        let row = matrix.get_row(0);
        assert_eq!(row, vec2);
    }

    #[test]
    fn set_row_2() {
        let mut matrix = Matrix::new(3, 4, 5.0);
        let vec1 = vec![1.0, 2.0, 3.0, 4.0];
        matrix.set_row(2, vec1);

        let vec2 = vec![1.0, 2.0, 3.0, 4.0];
        let row = matrix.get_row(2);
        assert_eq!(row, vec2);
    }

    #[test]
    fn set_column_1() {
        let mut matrix = Matrix::new(3, 4, 5.0);
        let vec1 = vec![1.0, 2.0, 3.0];
        matrix.set_column(0, vec1);

        let vec2 = vec![1.0, 2.0, 3.0];
        let column = matrix.get_column(0);
        assert_eq!(column, vec2);
    }

    #[test]
    fn set_column_2() {
        let mut matrix = Matrix::new(3, 4, 5.0);
        let vec1 = vec![1.0, 2.0, 3.0];
        matrix.set_column(3, vec1);

        let vec2 = vec![1.0, 2.0, 3.0];
        let column = matrix.get_column(3);
        assert_eq!(column, vec2);
    }

    #[test]
    fn get_diagonal_1() {
        let mut matrix = Matrix::new(3, 3, 8.0);
        matrix.set(0, 0, 1.0);
        matrix.set(1, 1, 2.0);
        matrix.set(2, 2, 3.0);

        let vec1 = vec![1.0, 2.0, 3.0];
        assert_eq!(matrix.get_diagonal(), vec1);
    }

    #[test]
    fn get_diagonal_2() {
        let mut matrix = Matrix::new(4, 4, 8.0);
        matrix.set(0, 0, 1.0);
        matrix.set(1, 1, 2.0);
        matrix.set(2, 2, 3.0);
        matrix.set(3, 3, 4.0);

        let vec1 = vec![1.0, 2.0, 3.0, 4.0];
        assert_eq!(matrix.get_diagonal(), vec1);
    }

    #[test]
    fn get_determinant_1() {
        let mut matrix = Matrix::new(2, 2, 0.0);
        matrix.set(0, 0, 2.0);
        matrix.set(1, 0, 4.0);
        matrix.set(0, 1, 3.0);
        matrix.set(1, 1, 5.0);

        assert_eq!(matrix.get_determinant(), -2.0);
    }

    #[test]
    fn get_determinant_2() {
        let mut matrix = Matrix::new(3, 3, 0.0);
        matrix.set(0, 0, 3.0);
        matrix.set(1, 0, 4.0);
        matrix.set(2, 0, 7.0);
        matrix.set(0, 1, 2.0);
        matrix.set(1, 1, 5.0);
        matrix.set(2, 1, 8.0);
        matrix.set(0, 2, 1.0);
        matrix.set(1, 2, 6.0);
        matrix.set(2, 2, 9.0);

        assert_eq!(matrix.get_determinant(), 0.0);
    }
}
