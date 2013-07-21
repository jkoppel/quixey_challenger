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
    public static ArrayList subsequences(int a, int b, int k) {
        if (k == 0) {
            return new ArrayList();
        }

        ArrayList ret = new ArrayList(50);
        for (int i=a; i<b+1-k; i++) {
            ArrayList base = new ArrayList(50);
            ArrayList initial = new ArrayList();
            initial.add(i);
            base.add(initial);
            for (Object rest : subsequences(i+1, b, k-1)) {
                base.add(rest);
            }
            ret.addAll(base);

        }

        return ret;
    }
}
