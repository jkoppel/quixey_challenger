import java.util.*;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author derricklin
 */
public class POWERSET {
    public static ArrayList<ArrayList<Char>> powerset(ArrayList<Char> arr) {
        if (!arr.isEmpty()) {
            first, *rest = arr;
            rest_subsets = powerset(rest);
            return [[first] + subset for subset in rest_subsets];
        } else {
            return [[]];
        }
    }
}
