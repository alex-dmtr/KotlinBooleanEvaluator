package main

import java.lang.Math.pow

fun isVar(token: String): Boolean {
    if (token.length != 1)
        return false

    return token[0] in 'A'..'Z'
}

fun findVariables(expr: String): ArrayList<String> {
    val vars = arrayListOf<String>()

    for (c in expr) {
        val token = c.toString()

        if (token !in vars && isVar(token))
            vars.add(token)
    }

    return vars
}

open class Operator(val token: String, val precedence: Int)

class BinaryOperator(token: String, precedence: Int, val apply: (Boolean, Boolean) -> Boolean) : Operator(token, precedence)

class UnaryOperator(token: String, precedence: Int, val apply: (Boolean) -> Boolean) : Operator(token, precedence)

class VariabileOperator(val variableName: String): Operator("var", 1000)

fun implication(a : Boolean, b : Boolean): Boolean {
    return !a || b
}

fun negation(a: Boolean): Boolean {
    return !a
}

fun boolAnd(a: Boolean, b: Boolean): Boolean {
    return a && b
}
fun boolOr(a: Boolean, b: Boolean): Boolean {
    return a || b
}

fun findToken(expr: String, token: String): Int? {
    var index = expr.length - 1
    var paranLevel = 0

    while (index >= 0) {
        if (expr[index] == ')')
            paranLevel++
        else if (expr[index] == '(')
            paranLevel--

        val substr = expr.substring(index, index+token.length)
        if (substr == token && paranLevel == 0)
            return index
        index--
    }

    return null
}

data class FoundOperator(val op: Operator, val index: Int?)

fun constructSyntaxTree(expr: String): SyntaxTreeNode {
    val ops = arrayOf(BinaryOperator(">", 0, ::implication),
            UnaryOperator ("~", 5, ::negation),
            BinaryOperator("+", 1, ::boolOr),
            BinaryOperator("*", 2, ::boolAnd))

    val vars = findVariables(expr)

    val foundOp = ops.sortedWith(compareBy(Operator::precedence))
            .map { op -> FoundOperator(op, findToken(expr, op.token)) }
            .filter { op -> op.index != null }
            .firstOrNull()

    if (foundOp != null && foundOp.index != null) {
        val indx = foundOp.index
        val op = foundOp.op

        if (op is BinaryOperator) {
            val left = constructSyntaxTree(expr.substring(0, indx))
            val right = constructSyntaxTree(expr.substring(indx+1))

            return SyntaxTreeNode(op, arrayOf(left, right))
        }
        else { //if (op is UnaryOperator) {
            val substr = expr.substring(indx+1, expr.length)
            val right = constructSyntaxTree(substr)

            return SyntaxTreeNode(op, arrayOf(right))
        }
    }
    else if (expr[0] == '(' && expr[expr.length-1] == ')')
        return constructSyntaxTree(expr.substring(1, expr.length-1))
    else
        return SyntaxTreeNode(VariabileOperator(expr), arrayOf())

}

fun evaluateSyntaxTree(variableMapping: Map<String, Boolean>, root: SyntaxTreeNode): Boolean {
    val op = root.operator
    val children = root.children

    if (op is BinaryOperator) {
        val leftResult = evaluateSyntaxTree(variableMapping, children[0])
        val rightResult = evaluateSyntaxTree(variableMapping, children[1])

        return op.apply(leftResult, rightResult)
    }
    else if (op is UnaryOperator) {
        val rightResult = evaluateSyntaxTree(variableMapping, children[0])

        return op.apply(rightResult)
    }
    else if (op is VariabileOperator) {
        val result = variableMapping[op.variableName]!!

        return result
    }
    else
        return false
}

data class SyntaxTreeNode(val operator: Operator, val children: Array<SyntaxTreeNode>)

fun eval(expr: String) {
    val tree = constructSyntaxTree(expr)
    val variables = findVariables(expr)

    println("$variables -> $expr")
    val upper = (1 shl variables.size) - 1

    val results = arrayListOf<Boolean>()
    for (i in 0..upper) {

        val mapping = mutableMapOf<String, Boolean> ()
        for (j in 0..variables.size-1) {
            val varName = variables[j]

            mapping[varName] = (i and (1 shl (variables.size-1-j))) != 0
        }

        val varValues = variables. map { varName -> if (mapping[varName]!!) 1 else 0 }
        val result = evaluateSyntaxTree(mapping, tree)

        results.add(result)
        val varResult = if (result) 1 else 0
        println("$varValues -> $varResult")
    }

    val allTrue = results.all { result -> result}
    val allFalse = results.all { result -> result}

    if (allTrue) {
        println("Tautology")
    }
    else if (allFalse){
        println("Contradiction")
    }
    else
        println("Satisfiable")
}

fun main(args: Array<String>) {
    eval("(A>B)+(B>A)")
}

