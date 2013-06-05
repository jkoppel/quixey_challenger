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
    public static ArrayList<ArrayList<Character>> powerset(ArrayList<Character> arr) {
        if (!arr.isEmpty()) {
            char first = arr.get(0);
            arr.remove(0);
            ArrayList<Character> rest = arr;
            ArrayList<ArrayList<Character>> rest_subsets = powerset(rest);
            
            ArrayList<ArrayList<Character>> output = new ArrayList<ArrayList<Character>>();
            for (ArrayList<Character> subset : rest_subsets) {
                ArrayList<Character> to_add = new ArrayList<Character>();
                to_add.add(first);
                to_add.addAll(subset);
                output.add(to_add);
            }
            
            return output;
        } else {
            return new ArrayList<ArrayList<Character>>();
        }
    }
}
