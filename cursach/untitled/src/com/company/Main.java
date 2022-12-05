package com.company;

import static com.company.Type.sin;

public class Main {

    public static void main(String[] args) {

        Variable x = new Variable("x");
        Variable y = new Variable("y");

        F f1 = new F()
                .add(new F().multiply(x, 1.0))
                .add(new F().multiply(y, 1.0).multiply(-1.0))
                .add(new F().multiply(-300.0));

        F f2 = new F()
                .add(new F().multiply(x,1.0).type(sin))
                .add(new F().multiply(new F().multiply(y,1.0).type(sin), 1.0).multiply(-2.0));

        Matrix matrix = new Matrix(0.0).add(f1).add(f2);


        System.out.println(matrix);
        matrix.solveMatrix();


    }
}