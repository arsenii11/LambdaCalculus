package org.example

import java.util.Scanner

/**
 * 1. Abstract Syntax Tree (AST) for Lambda Expressions
 */
sealed class Expr {
    // We override toString() or provide a separate prettyPrint for clarity.
    abstract override fun toString(): String
}

/** A variable, e.g., x, y, z */
data class VarExpr(val name: String) : Expr() {
    override fun toString() = name
}

/** A lambda abstraction, e.g., λx. M or \x. M */
data class LambdaExpr(val param: String, val body: Expr) : Expr() {
    override fun toString() = "λ$param. $body"
}

/** A function application, e.g., (M N) */
data class AppExpr(val func: Expr, val arg: Expr) : Expr() {
    override fun toString() = "($func $arg)"
}

/**
 * 2. Parser to read a string and produce an Expr
 *
 *    Grammar (simplified):
 *    Expr         ::= Lambda | Application
 *    Lambda       ::= ('λ' | '\\') Var '.' Expr
 *    Application  ::= Factor { Factor }
 *    Factor       ::= '(' Expr ')' | Var
 *
 *    We rely on whitespace and parentheses to separate expressions.
 *    Multiple factors in a row are interpreted as application (left-associative).
 */
class Parser(private val tokens: List<String>) {
    private var position = 0

    private fun currentToken(): String? =
        if (position < tokens.size) tokens[position] else null

    private fun advance() { position++ }

    // Entry point
    fun parseExpr(): Expr {
        return parseLambda()
    }

    // Parse a possible lambda or else parse an application
    private fun parseLambda(): Expr {
        val token = currentToken()
        return if (token == "\\" || token == "λ") {
            // Lambda abstraction
            advance() // consume '\' or 'λ'
            val param = currentToken() ?: error("Expected parameter after λ or \\")
            advance() // consume the variable
            if (currentToken() != ".") {
                error("Expected '.' after parameter in lambda abstraction")
            }
            advance() // consume '.'
            val body = parseLambda() // parse the body (which could itself be a lambda or application)
            LambdaExpr(param, body)
        } else {
            // Otherwise parse an application
            parseApplication()
        }
    }

    // Parse applications (left-associative)
    private fun parseApplication(): Expr {
        var expr = parseFactor()
        while (true) {
            val token = currentToken()
            // If the next token looks like the start of a factor, parse it and build an application
            if (token != null && token != ")" && token != "." && token != "λ" && token != "\\") {
                val arg = parseFactor()
                expr = AppExpr(expr, arg)
            } else {
                break
            }
        }
        return expr
    }

    // Parse a single factor: either a parenthesized expression or a variable
    private fun parseFactor(): Expr {
        val token = currentToken() ?: error("Unexpected end of expression")
        return when {
            token == "(" -> {
                advance() // consume '('
                val exprInside = parseLambda()
                if (currentToken() != ")") {
                    error("Expected ')' after sub-expression")
                }
                advance() // consume ')'
                exprInside
            }
            token == "\\" || token == "λ" -> {
                // This can happen if there's a missing dot or something similar.
                // We'll just forward to parseLambda, though we typically handle it earlier.
                parseLambda()
            }
            else -> {
                // Must be a variable
                // Ensure it's not a punctuation symbol
                if (token in listOf(".", "(", ")", "λ", "\\")) {
                    error("Unexpected token '$token' when parsing factor")
                }
                advance()
                VarExpr(token)
            }
        }
    }
}

/**
 * Helper function to tokenize the input.
 * Splits on whitespace and single-character tokens like ( ) . λ \
 */
fun tokenize(input: String): List<String> {
    val symbols = setOf('(', ')', '.', 'λ', '\\')
    val tokens = mutableListOf<String>()
    var i = 0
    while (i < input.length) {
        val c = input[i]
        when {
            c.isWhitespace() -> {
                i++
                continue
            }
            c in symbols -> {
                tokens.add(c.toString())
                i++
            }
            else -> {
                // Accumulate a variable name
                val sb = StringBuilder()
                while (i < input.length && !input[i].isWhitespace() && input[i] !in symbols) {
                    sb.append(input[i])
                    i++
                }
                tokens.add(sb.toString())
            }
        }
    }
    return tokens
}

/**
 * 3. Utility functions for alpha-conversion, substitution, beta-reduction
 */

/** Collect free variables in an expression */
fun freeVars(expr: Expr): Set<String> = when (expr) {
    is VarExpr -> setOf(expr.name)
    is LambdaExpr -> freeVars(expr.body) - expr.param
    is AppExpr -> freeVars(expr.func) + freeVars(expr.arg)
}

/** Generate a fresh variable name based on a given hint (x -> x') */
fun freshName(hint: String, used: Set<String>): String {
    var candidate = "${hint}'"
    while (candidate in used) {
        candidate += "'"
    }
    return candidate
}

/** Alpha-conversion: rename all occurrences of oldName to newName in expr */
fun alphaConvert(expr: Expr, oldName: String, newName: String): Expr = when (expr) {
    is VarExpr ->
        if (expr.name == oldName) VarExpr(newName) else expr

    is LambdaExpr ->
        if (expr.param == oldName) {
            // overshadowed, no conversion in body
            expr
        } else {
            LambdaExpr(
                expr.param,
                alphaConvert(expr.body, oldName, newName)
            )
        }

    is AppExpr ->
        AppExpr(
            alphaConvert(expr.func, oldName, newName),
            alphaConvert(expr.arg, oldName, newName)
        )
}

/** Substitution: replace all free occurrences of varName with newExpr in body */
fun substitute(body: Expr, varName: String, newExpr: Expr): Expr = when (body) {
    is VarExpr ->
        if (body.name == varName) newExpr else body

    is LambdaExpr -> {
        if (body.param == varName) {
            // varName is bound here, do not substitute in body
            body
        } else {
            // if param is in freeVars(newExpr), alpha-convert to avoid capture
            if (body.param in freeVars(newExpr)) {
                val usedVars = freeVars(body.body) + freeVars(newExpr) + setOf(body.param, varName)
                val fresh = freshName(body.param, usedVars)
                val renamedBody = alphaConvert(body.body, body.param, fresh)
                LambdaExpr(
                    fresh,
                    substitute(renamedBody, varName, newExpr)
                )
            } else {
                LambdaExpr(
                    body.param,
                    substitute(body.body, varName, newExpr)
                )
            }
        }
    }

    is AppExpr ->
        AppExpr(
            substitute(body.func, varName, newExpr),
            substitute(body.arg, varName, newExpr)
        )
}

/** Perform one step of normal-order beta-reduction */
fun betaReduce(expr: Expr): Expr {
    // Normal-order means: reduce the leftmost-outermost redex first
    return when (expr) {
        is AppExpr -> {
            // If expr.func is a lambda, substitute immediately
            if (expr.func is LambdaExpr) {
                substitute(expr.func.body, expr.func.param, expr.arg)
            } else {
                // Otherwise, reduce expr.func first
                val reducedFunc = betaReduce(expr.func)
                if (reducedFunc != expr.func) {
                    AppExpr(reducedFunc, expr.arg)
                } else {
                    // Then reduce expr.arg
                    val reducedArg = betaReduce(expr.arg)
                    AppExpr(expr.func, reducedArg)
                }
            }
        }
        is LambdaExpr ->
            // Reduce the body of the lambda
            LambdaExpr(expr.param, betaReduce(expr.body))

        is VarExpr ->
            // A variable alone cannot be reduced
            expr
    }
}

/** Repeatedly apply betaReduce until no changes occur (normal form) */
fun normalize(expr: Expr): Expr {
    while (true) {
        val next = betaReduce(expr)
        if (next == expr) {
            return expr
        }
        return normalize(next)
    }
}

/**
 * 4. Main: Simple REPL to read and evaluate expressions
 *
 * Usage:
 *   kotlinc LambdaInterpreter.kt -include-runtime -d lambda.jar
 *   java -jar lambda.jar
 *
 * Then type expressions like:
 *   (\x. x) y
 *   (λx. x) z
 *   (\x. λy. x) a b
 *   (λs. λz. z)  <-- Church numeral 0
 */
fun main() {
    val scanner = Scanner(System.`in`)
    println("Simple Lambda Calculus REPL in Kotlin")
    println("Enter an expression (or type :quit to exit):")
    while (true) {
        print("> ")
        val line = scanner.nextLine().trim()
        if (line.lowercase() == ":quit") {
            println("Goodbye.")
            break
        }
        if (line.isBlank()) continue

        try {
            val tokens = tokenize(line)
            val parser = Parser(tokens)
            val expr = parser.parseExpr()
            val result = normalize(expr)
            println("AST: $expr")
            println("Normal form: $result")
        } catch (e: Exception) {
            println("Error: ${e.message}")
        }
    }
}
