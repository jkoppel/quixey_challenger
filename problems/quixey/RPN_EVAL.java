package quixey;
import java.util.*;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author derricklin
 */
public class RPN_EVAL {

    public static Value rpn_eval(List<Token> tokens) {
        Stack<Value> stack = new Stack<Value>();

        for (Token token : tokens) {
            if (Value.class.isInstance(token)) {
                stack.push((Value) token);
            } else if (Op.class.isInstance(token)) {
                Op operator = (Op) token;
                char op = operator.op;
                Double a = stack.pop().value;
                Double b = stack.pop().value;
                if (op == '+') {
                    stack.push(new Value(a+b));
                } else if (op == '-') {
                    stack.push(new Value(a-b));
                } else if (op == '*') {
                    stack.push(new Value(a*b));
                } else if (op == '/') {
                    stack.push(new Value(a/b));
                }
            }
        }

        return stack.pop();
    }
    public static class Token {}
    public static class Op extends Token {
        char op;
    }
    public static class Value extends Token {
        double value;
        public Value(double val) {
            value = val;
        }
    }
}
