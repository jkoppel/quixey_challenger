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
    public static List<? extends Token> shunting_yard(List<? extends Token> tokens) {
        Map<Char, int> precedence = new HashMap<Char, int>();
        precedence.put('+',1);
        precedence.put('-',1);
        precedence.put('*',2);
        precedence.put('/',2);
        
        ArrayList<? extends Token> rpntokens = new ArrayList<? extends Token>();
        ArrayList<? extends Token> opstack = new ArrayList<? extends Token>();
        
        for (Token token: tokens) {
            if (token instanceof int) {
                rpntokens.add(token)
            } else {
                while (!opstack.isEmpty() && precedence.get(token) <= precedence.get(opstack.getLast())) {
                    rpntokens.add(opstack.getLast());
                    opstack.removeLast();
                }
            }
        }
        
        while (!opstack.isEmpty()) {
            rpntokens.add(opstack.getLast());
            opstack.removeLast();
        }
        
        return rpntokens;
    }
}
