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
    public static Integer shortest_path_lengths(Integer n, Map<Pair<Node,Node>, Integer> length_by edge) {
        double inf = Double.POSITIVE_INFINITY;
        
        Map<Pair<Integer,Integer>, Double> length_by_path = new Map<Pair<Node,Node>, Double>();
        for (int i=0; i<n; i++) {
            for (int j=0; j<n; j++) {
                length_by_path.put(Pair(i,j), 0);
            }
        }
        length_by_path.putAll(length_by_edge);
        
        for (int k=0; k<n; k++) {
            for (int i=0; i<n; i++) {
                for (int j=0; j<n; j++) {
                    length_by_path.put(Pair(i,j),
                            Math.min(
                                length_by_path(Pair(i,j).isEmpty? inf : length_by_path(Pair(i,j))),
                                length_by_path(Pair(i,k) + length_by_path(Pair(j,k)))
                            )
                    )
                }
            }
        }
        
        return length_by_path;
        
        
    }
}
