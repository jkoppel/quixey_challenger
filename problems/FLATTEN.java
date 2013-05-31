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
    public static ArrayList flatten(ArrayList arr) {
        for (Object x : arr) {
            if (x instanceof List<?>) {
                for (ArrayList y : flatten(x))
                    yield y;
            } else {
                yield flatten(x);
            }
        }
    }
}
