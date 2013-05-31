import java.util.*;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author derricklin
 */
public class SUBSEQUENCES {
    public static ArrayList<T> subsequences (int a, int b, int k) {
        if (k == 0) {
            return new ArrayList<ArrayList<Integer>>();
        }
        
        ArrayList<T> ret = new ArrayList<ArrayList<Integer>>();
        for (int i=a; i<b+1-k; i++) {
            ArrayList<Integer> base = new ArrayList<Integer>();
            base.add(i);
            for (T rest : subsequences(i+1, b, k-1)) {
                base.add(rest);
            }
            ret.addAll(base);
                    
        }
        
        return ret;
    }
}
