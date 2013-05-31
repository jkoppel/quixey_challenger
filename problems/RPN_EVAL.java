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
    public static Number op(Char symbol, Number a, Number b) {
        switch (symbol) {
            case '+': return a + b;
                      break;
            case '-': return a - b;
                      break;
            case '*': return a * b;
                      break;
            case '/': return a / b;
                      break;
            default: return 0;
                      break;
        }
    }
    
    public static Number rpn_eval()
}
