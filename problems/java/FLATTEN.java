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
public class FLATTEN {
    public static List<Object> flatten(List<?> arr) {
        List<Object> result = new ArrayList<Object>();
        for (Object x : arr) {
            if (x instanceof List<?>) {
                for (Object y : flatten((List<?>) x))
                    result.add(y);
            } else {
                result.add(flatten((List<?>) x));
            }
        }
        return result;
    }
}
