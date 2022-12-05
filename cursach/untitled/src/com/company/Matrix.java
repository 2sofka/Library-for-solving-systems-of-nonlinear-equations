package com.company;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Matrix {

    private static int iter = 0;
    private final List<F> vector = new ArrayList<>();
    private Double[][] JacobyMatrix;

    private final Map<Variable, Double> values = new HashMap<>();

    //добавление функции в вектор-функций
    public Matrix add(F f) {
        vector.add(f);
        return this;
    }

    public Matrix(Double firstValue) {
        for (Variable variable : F.allVariables)
            this.values.put(variable, firstValue);
    }

    //Якобиан
    private void JacobyMatrix() {
        JacobyMatrix = new Double[vector.size()][vector.size()];
        for (int i = 0; i < vector.size(); i++) {
            int j = 0;
            for (Variable variable : values.keySet()) {
                F f = new F(vector.get(i));
                JacobyMatrix[j][i] = getValue(f.derivation(variable));
                j++;
            }
        }
    }

    //метод нахождения ответа
    public void solveMatrix() {
        while (!solved()) {
            iter++;
            reset();
        }
        System.out.println(values);
        System.out.println(iter + " iterations");
    }

    //метод нахождения более точного значения решения системы уравнений
    private void reset() {
        JacobyMatrix();
        inversion(JacobyMatrix);
        Double[] list = new Double[vector.size()];
        int i = 0;
        for (Variable variable : values.keySet()) {
            Double answer = values.get(variable);
            double minus = 0.0;
            for (int j = 0; j < vector.size(); j++) {

                minus += 0.1 * getValue(vector.get(i)) * JacobyMatrix[i][j];

            }

            if (getValue(vector.get(i)) > 0)
                answer -= Math.abs(minus);
            else
                answer += Math.abs(minus);
            list[i] = answer;
            i++;
        }
        i = 0;
        for (Variable variable : values.keySet()) {
            values.put(variable, list[i]);
            i++;
        }

    }

    //проверка выполнения условия
    private Boolean solved() {

        for (F f : vector) {
            double eps = 0.00001;
            if (Math.abs(getValue(f)) > eps) {
                return false;
            }
        }
        return true;
    }

    @Override
    public String toString() {
        StringBuilder string = new StringBuilder();

        for (F f : vector) {
            string.append(f).append(" = 0\n");
        }

        return string.toString();
    }

    //нахождение значения функции в точке
    private Double getValue(F f) {

        Double value = 1.0;
        if (f.getFunctionsList().isEmpty() & f.getFunctions().isEmpty() & f.getVariables().isEmpty()) {
            return f.getMultiplication();
        }

        if (f.getFunctionsList().isEmpty()) {

            if (f.getType().equals(Type.none)) {

                value *= f.getMultiplication();
                for (Variable variable : f.getVariables().keySet()) {
                    if (values.containsKey(variable)) {
                        value *= Math.pow(values.get(variable), f.getVariables().get(variable));
                    }
                }
                for (F function : f.getFunctions().keySet()) {
                    value *= Math.pow(getValue(function), f.getFunctions().get(function));
                }

            } else if (f.getType().equals(Type.sin)) {
                value *= Math.sin(getValue(f.type(Type.none)));
                f.type(Type.sin);
            } else if (f.getType().equals(Type.cos)) {
                value *= Math.cos(getValue(f.type(Type.none)));
                f.type(Type.cos);
            } else if (f.getType().equals(Type.tg)) {
                value *= Math.tan(getValue(f.type(Type.none)));
                f.type(Type.tg);
            } else if (f.getType().equals(Type.ctg)) {
                value *= 1 / Math.tan(getValue(f.type(Type.none)));
                f.type(Type.ctg);
            } else if (f.getType().equals(Type.ln)) {
                value *= Math.log(getValue(f.type(Type.none)));
                f.type(Type.ln);
            }
        } else {

            value = 0.0;
            for (F func : f.getFunctionsList()) {
                value += getValue(func);
            }

            value *= f.getMultiplication();

        }

        return value;
    }

    //нахождение обратной матрицы
    private void inversion(Double[][] A) {
        Double temp;

        Double[][] E = new Double[A.length][A.length];

        for (int i = 0; i < A.length; i++)
            for (int j = 0; j < A.length; j++) {
                E[i][j] = 0.0;

                if (i == j)
                    E[i][j] = 1.0;
            }

        for (int k = 0; k < A.length; k++) {
            temp = A[k][k];

            for (int j = 0; j < A.length; j++) {
                A[k][j] /= temp;
                E[k][j] /= temp;
            }

            for (int i = k + 1; i < A.length; i++) {
                temp = A[i][k];

                for (int j = 0; j < A.length; j++) {
                    A[i][j] -= A[k][j] * temp;
                    E[i][j] -= E[k][j] * temp;
                }
            }
        }

        for (int k = A.length - 1; k > 0; k--) {
            for (int i = k - 1; i >= 0; i--) {
                temp = A[i][k];

                for (int j = 0; j < A.length; j++) {
                    A[i][j] -= A[k][j] * temp;
                    E[i][j] -= E[k][j] * temp;
                }
            }
        }

        for (int i = 0; i < A.length; i++) {
            System.arraycopy(E[i], 0, A[i], 0, A.length);
        }

    }

}
