package com.company;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class F {

    public static List<Variable> allVariables = new ArrayList<>();
    private final List<F> functionsList = new ArrayList<>();
    private final Map<F, Double> functions = new HashMap<>();
    private final Map<Variable, Double> variables = new HashMap<>();
    private Double multiplication;

    private Type type;

    public F() {
        this.multiplication = 1.0;
        this.type = Type.none;
    }

    public F(F f) {
        this.functionsList.addAll(f.functionsList);
        this.functions.putAll(f.functions);
        this.variables.putAll(f.variables);
        this.multiplication = f.multiplication;
        this.type = Type.valueOf(f.type.toString());
    }
    //метод нахождения производной
    public F derivation(Variable variable) {
        F derivation = new F();
        if(isConstant() ) return derivation.multiply(0.0);
        if (!contains(variable)) {
            return derivation.multiply(0.0);
        }

        if (!functionsList.isEmpty()) {
            List<F> fList = new ArrayList<>();
            F func = new F().multiply(multiplication);
            for (Variable var : variables.keySet()) {
                func.multiply(var, variables.get(var));
            }
            for (F f : functionsList) {
                if(! f.contains(variable)) {
                    fList.add(new F().multiply(0.0));
                    continue;
                }
                if (!functions.isEmpty()) {
                    for (F function : functions.keySet()) {
                        fList.add(new F()
                                .multiply(func, 1.0)
                                .multiply(f, 1.0)
                                .multiply(function, functions.get(function)));
                    }
                } else {
                    fList.add(new F()
                            .multiply(func, 1.0)
                            .multiply(f, 1.0));

                }
            }

            for (F f : fList) {
                derivation.add(f.derivation(variable));
            }
                if (type.equals(Type.tg)) {
                    F term = new F();
                    for(F f : fList)
                        term.add(f);
                    derivation.multiply(term.type(Type.cos), -2.0);
            }
            else if (type.equals(Type.ctg)) {
                    F term = new F();
                    for(F f : fList)
                        term.add(f);
                    derivation.multiply(term.type(Type.sin), -2.0);
            }
            else if (type.equals(Type.cos)) {
                    F term = new F();
                    for(F f : fList)
                        term.add(f);
                    derivation.multiply(term.type(Type.sin), 1.0).multiply(-1.0);
            }
            else if (type.equals(Type.sin)) {
                    F term = new F();
                    for(F f : fList)
                        term.add(f);
                    derivation.multiply(term.type(Type.cos), 1.0);
            }
            else if (type.equals(Type.ln)) {
                F term = new F();
                for(F f : fList)
                    term.add(f);

                derivation.multiply(term, -1.0);

            }
        }
        else if (functions.isEmpty()) {
            if (this.type.equals(Type.none)) {
                if (variables.containsKey(variable)) {
                    derivation.multiply(variable, variables.get(variable) - 1)
                            .multiply(variables.get(variable))
                            .multiply(multiplication);
                    for (Variable var : variables.keySet())
                        if (!var.equals(variable)) {
                            derivation.multiply(var, variables.get(var));
                        }
                }
            }
            else if (type.equals(Type.tg)) {
                if (variables.containsKey(variable)) {
                    F cos = new F()
                            .multiply(variable, variables.get(variable))
                            .multiply(multiplication)
                            .type(Type.cos);
                    for (Map.Entry<Variable, Double> otherVariable : variables.entrySet()) {
                        if (!otherVariable.getKey().equals(variable)) {
                            derivation.multiply(otherVariable.getKey(), otherVariable.getValue());
                            cos.multiply(otherVariable.getKey(), otherVariable.getValue());
                        }
                    }
                    derivation.multiply(cos, -2.0)
                            .multiply(variables.get(variable))
                            .multiply(multiplication)
                            .multiply(variable, variables.get(variable) - 1);
                }
            }
            else if (type.equals(Type.ctg)) {
                if (variables.containsKey(variable)) {
                    F sin = new F()
                            .multiply(variable, variables.get(variable))
                            .multiply(multiplication)
                            .type(Type.sin);
                    for (Map.Entry<Variable, Double> otherVariable : variables.entrySet()) {
                        if (!otherVariable.getKey().equals(variable)) {
                            derivation.multiply(otherVariable.getKey(), otherVariable.getValue());
                            sin.multiply(otherVariable.getKey(), otherVariable.getValue());
                        }
                    }
                    derivation.multiply(sin, -2.0)
                            .multiply(variables.get(variable))
                            .multiply(multiplication)
                            .multiply(variable, variables.get(variable) - 1);
                }
            }
            else if (type.equals(Type.cos)) {
                if (variables.containsKey(variable)) {
                    F sin = new F()
                            .multiply(variable, variables.get(variable))
                            .multiply(multiplication)
                            .type(Type.sin);
                    for (Map.Entry<Variable, Double> otherVariable : variables.entrySet()) {
                        if (!otherVariable.getKey().equals(variable)) {
                            derivation.multiply(otherVariable.getKey(), otherVariable.getValue());
                            sin.multiply(otherVariable.getKey(), otherVariable.getValue());
                        }
                    }
                    derivation.multiply(sin, 1.0)
                            .multiply(variables.get(variable))
                            .multiply(-multiplication)
                            .multiply(variable, variables.get(variable) - 1);
                }
            }
            else if (type.equals(Type.sin)) {
                if (variables.containsKey(variable)) {
                    F cos = new F()
                            .multiply(variable, variables.get(variable))
                            .multiply(multiplication)
                            .type(Type.cos);
                    for (Map.Entry<Variable, Double> otherVariable : variables.entrySet()) {
                        if (!otherVariable.getKey().equals(variable)) {
                            derivation.multiply(otherVariable.getKey(), otherVariable.getValue());
                            cos.multiply(otherVariable.getKey(), otherVariable.getValue());
                        }
                    }
                    derivation.multiply(cos, 1.0)
                            .multiply(variables.get(variable))
                            .multiply(multiplication)
                            .multiply(variable, variables.get(variable) - 1);

                }
            }
            else if (type.equals(Type.ln)) {
                if (variables.containsKey(variable)) {
                    derivation
                            .multiply(variables.get(variable))
                            .multiply(multiplication)
                            .multiply(variable, -1.0);
                }
            }

        }
        else {
            derivation.multiply(multiplication);
            for(Variable var : variables.keySet()) {
                if(!var.equals(variable)){
                    derivation.multiply(var, variables.get(var));
                }
            }

            if(variables.containsKey(variable)){
                for (F func : functions.keySet()){
                    F term = new F().multiply(variable, variables.get(variable));
                    if (!func.isConstant()) {
                        for (F otherFunc : functions.keySet()) {
                            if (otherFunc.contains(variable)) {
                                if (otherFunc.equals(func)) {
                                    term.multiply(otherFunc.derivation(variable), 1.0);
                                }
                            } else {
                                term.multiply(otherFunc, 1.0);
                            }
                        }
                        derivation.add(term);
                    }
                }
                F term = new F().multiply(variable, variables.get(variable) - 1).multiply(variables.get(variable));
                for (F func : functions.keySet()){
                    term.multiply(func, functions.get(func));
                }

                derivation.add(term);
            } else {
                for (F func : functions.keySet()){
                    F term = new F();
                    if (!func.isConstant()) {
                        for (F otherFunc : functions.keySet()) {
                            if (otherFunc.contains(variable)) {
                                if (otherFunc.equals(func)) {
                                    term.multiply(otherFunc.derivation(variable), 1.0);
                                }
                            } else {
                                term.multiply(otherFunc, 1.0);
                            }
                        }

                        derivation.add(term);
                    }
                }

            }

        }
        return derivation;
    }

    /**
     умножение функции на переменную
     */

    public F multiply(Variable x, Double power) {

        if (variables.containsKey(x)) {
            if(variables.get(x) + power != 0)
                variables.put(x, variables.get(x) + power);
            else
                variables.remove(x);
        }
        else {
            if(power != 0)
                variables.put(x, power);
        }

        if (!allVariables.contains(x))
            allVariables.add(x);
        return this;
    }
    //умножение функции на другую функцию
    public F multiply(F function, Double power) {

        functions.put(function, power);

        return this;
    }
    //добавление функции через сложение
    public F add(F function) {
        functionsList.add(function);
        return this;
    }
    //умножение функции на константу
    public F multiply(Double multiplication) {
        this.multiplication *= multiplication;
        return this;
    }

    @Override
    public String toString() {

        if (multiplication == 0) return "0";
        StringBuilder string = new StringBuilder();


        if (!type.equals(Type.none)) {
            string.append(type).append("(");

            if (variables.isEmpty() & functions.isEmpty() & functionsList.isEmpty()) {
                    return multiplication.toString();

            } else if (multiplication != 1.0)
                string.append(multiplication).append("*");

            if (functionsList.isEmpty()) {
                string.append(showVariables());
                if (!showFunctions().isEmpty()) {
                    string.append(showFunctions());
                    string.append("(").append(showFunctionsList()).append(")");
                } else string.append(showFunctionsList());
            } else {
                if (!variables.isEmpty())
                    string.append(showVariables()).append("*");
                if (!showFunctions().isEmpty()) {
                    string.append(showFunctions()).append(")");
                    string.append("(").append(showFunctionsList()).append(")");
                } else string.append(showFunctionsList());
            }
            string.append(")");

        } else {
            if (variables.isEmpty() & functions.isEmpty() & functionsList.isEmpty()) {
                return multiplication.toString();
            } else if (multiplication != 1.0)
                string.append(multiplication).append("*");

            if (functionsList.isEmpty()) {
                string.append(showVariables());
                if (!showFunctions().isEmpty()) {
                    string.append(showFunctions());
                } else string.append(showFunctionsList());
            } else {
                string.append(showVariables());
                if (!showFunctions().isEmpty()) {
                    string.append("(").append(showFunctions()).append(")");
                    if (!showFunctionsList().isEmpty())
                        string.append("(").append(showFunctionsList()).append(")");
                } else string.append(showFunctionsList());

            }
        }

        return string.toString();
    }
    // методы вывода функции в консоль
    private String showFunctions() {
        StringBuilder string = new StringBuilder();

        if (functions.size() > 1) {
            for (Map.Entry<F, Double> function : functions.entrySet()) {
                if (function.getValue() != 1.0)
                    string.append("(").append(function.getKey()).append("^").append(function.getValue()).append(")").append("*");
                else if (!function.getKey().isConstant())
                    string.append(function.getKey());
                else if(function.getKey().multiplication != 1.0)
                    string.append(function.getKey()).append("*");
                else {
                    string.append(" ");
                }
            }
            string.deleteCharAt(string.length() - 1);
        } else if (functions.size() == 1) {
            for (Map.Entry<F, Double> function : functions.entrySet()) {
                if (function.getValue() != 1.0)
                    string.append("(" + "(").append(function.getKey()).append(")").append("^").append(function.getValue()).append(")").append("*");
                else
                    string.append(function.getKey()).append("*");
            }
            string.deleteCharAt(string.length() - 1);
        }

        return string.toString();
    }
    private String showFunctionsList() {
        StringBuilder string = new StringBuilder();
        if (functionsList.size() > 1) {
            for (int i = 0; i < functionsList.size() - 1; i++) {
                if(!functionsList.get(i).toString().equals(" "))
                string.append(functionsList.get(i)).append(" + ");
            }
            string.append(functionsList.get(functionsList.size() - 1));
        } else if (functionsList.size() == 1) {
            string.append(functionsList.get(0).toString());
        }
        return string.toString();
    }
    private String showVariables() {
        StringBuilder string = new StringBuilder();

        if (variables.size() > 1) {
            for (Map.Entry<Variable, Double> variable : variables.entrySet()) {
                if (variable.getValue() != 1.0 & variable.getValue() != 0.0)
                    string.append("(").append(variable.getKey()).append("^").append(variable.getValue()).append(")").append("*");
                else if (variable.getValue() != 0.0)
                    string.append(variable.getKey()).append("*");
            }
            if (!string.isEmpty())
                string.deleteCharAt(string.length() - 1);
        } else if (variables.size() == 1) {
            for (Map.Entry<Variable, Double> variable : variables.entrySet()) {
                if (variable.getValue() != 1.0 & variable.getValue() != 0.0)
                    string.append("(").append(variable.getKey()).append("^").append(variable.getValue()).append(")").append("*");
                else if (variable.getValue() != 0.0)
                    string.append(variable.getKey()).append("*");
            }
            if (!string.isEmpty())
                string.deleteCharAt(string.length() - 1);
        }

        return string.toString();
    }
    //
    public F type(Type type) {
        this.type = type;
        return this;
    }
    //проверка является ли функция константой
    private Boolean isConstant(){

        return variables.isEmpty() & functions.isEmpty() & functionsList.isEmpty();

    }

    public List<F> getFunctionsList() {
        return functionsList;
    }

    public Map<F, Double> getFunctions() {
        return functions;
    }

    public Map<Variable, Double> getVariables() {
        return variables;
    }

    public Double getMultiplication() {
        return multiplication;
    }

    public Type getType() {
        return type;
    }
    //метод определяющий есть ли в данной функции необходимая для взятия производной переменная
    private Boolean contains(Variable variable) {
        if (variables.containsKey(variable))
            return true;
        for (F func : functionsList) {
            if (func.contains(variable))
                return true;
        }
        for (Map.Entry<F, Double> function : functions.entrySet()) {
            if (function.getKey().contains(variable)) {
                return true;
            }
        }

        return false;
    }

}
