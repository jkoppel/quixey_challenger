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
public class SHUNTING_YARD {
    public static List<Token> shunting_yard(List<Token> tokens) {
        Map<Character, Integer> precedence = new HashMap<Character, Integer>();
        precedence.put('+',1);
        precedence.put('-',1);
        precedence.put('*',2);
        precedence.put('/',2);

        ArrayList<Token> rpntokens = new ArrayList<Token>();
        Deque<Op> opstack = new ArrayDeque<Op>();

        for (Token token: tokens) {
            if (Value.class.isInstance(token)) {
                rpntokens.add(token);
            } else {
                Op operator = (Op) token;
                char op = operator.op;
                while (!opstack.isEmpty() && precedence.get(op) <= precedence.get(opstack.getLast().getOp())) {
                    rpntokens.add(opstack.pop());
                }
            }
        }

        while (!opstack.isEmpty()) {
            rpntokens.add(opstack.pop());
        }

        return rpntokens;
    }




    public static class Token {}
    public static class Op extends Token {
        char op;
        public char getOp() {
            return op;
        }
    }
    public static class Value extends Token {
        int value;
        public Value(int val) {
            value = val;
        }
    }
}
