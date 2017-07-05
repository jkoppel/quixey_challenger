package java_programs;
import java.util.*;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author derricklin
 */
public class SHORTEST_PATH_LENGTHS {
    public static Map<Integer,Map<Integer,Integer>> shortest_path_lengths(Integer n, Map<Integer,Map<Integer,Integer>> length_by_edge) {

        Map<Integer,Map<Integer,Integer>> length_by_path = new HashMap<Integer,Map<Integer,Integer>>();
        for (int i=0; i<n; i++) {
            Map<Integer,Integer> init = new HashMap<Integer,Integer>();
            init.put(i, 0);
            length_by_path.put(i, init);
        }

        for (int a : length_by_edge.keySet()) {
            for (int b : length_by_edge.get(a).keySet()) {
                int update = length_by_edge.get(a).get(b);
                Map<Integer,Integer> path_update = length_by_path.get(a);
                path_update.put(b, update);
                length_by_path.put(a, path_update);
            }
        }

        for (int k=0; k<n; k++) {
            for (int i=0; i<n; i++) {
                for (int j=0; j<n; j++) {
                    if (length_by_path.containsKey(i)) {
                        int fst_option = length_by_path.get(i).containsKey(j) ? length_by_path.get(i).get(j) : Integer.MAX_VALUE;
                        int snd_option = Integer.MAX_VALUE;
                        if (length_by_path.get(i).containsKey(k)) {
                            if (length_by_path.containsKey(j) && length_by_path.get(j).containsKey(k)) {
                                snd_option = length_by_path.get(i).get(k) + length_by_path.get(j).get(k);
                            }
                        }

                        Map<Integer,Integer> to_update = length_by_path.get(i);
                        to_update.put(j, Math.min(fst_option,snd_option));
                        length_by_path.put(i, to_update);

                    } else {
                        Map<Integer,Integer> init = new HashMap<Integer,Integer>();
                        init.put(j,Integer.MAX_VALUE);
                        length_by_path.put(i, init);
                    }
                }
            }
        }

        return length_by_path;
    }
}
