import java.util.*;
/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author derricklin
 */
public class LCS_LENGTH {
    public static Integer lcs_length(String s, String t) {
        // make a Counter
        
        Map<Pair<Integer, Integer>, Integer> counter = new HashMap<Pair<Integer,Integer>, Integer>();
        
        for (int i=0; i < s.length(); i++) {
            for (int j=0; j < t.length(); j++) {
                if (s.charAt(i) == t.charAt(j)) {
                    try {
                        counter.put(Pair(i,j), counter.get(Pair(i-1,j)) + 1);
                    }
                    catch {
                        count.put(Pair(i,j), 1);
                    }
                }
            }
        }
        
        return !counter.isEmpty() ? Collections.max(counter.values()) : 0;
    }
    
}
