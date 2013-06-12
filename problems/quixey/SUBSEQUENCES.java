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
public class SUBSEQUENCES {
    public static ArrayList<Integer> subsequences (int a, int b, int k) {
        if (k == 0) {
            return new ArrayList<Integer>();
        }

        ArrayList<Integer> ret = new ArrayList<Integer>();
        for (int i=a; i<b+1-k; i++) {
            ArrayList<Integer> base = new ArrayList<Integer>();
            base.add(i);
            for (Integer rest : subsequences(i+1, b, k-1)) {
                base.add(rest);
            }
            ret.addAll(base);

        }

        return ret;
    }
}
