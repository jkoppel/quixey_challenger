import.java.util.*;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author derricklin
 */
public class SHORTEST_PATHS {
    public static Map<Char, Integer> shortest_paths(Char source, Map<Pair<Char,Char>, Integer> weight_by_edge) {
        double inf = Double.POSITIVE_INFINITY;
        Map<Pair<Char,Char>, Double> weight_by_edge = new HashMap<Pair<Char,Char>, Double>();
        for (Pair (u,v) : weight_by_edge.keySet()) {
            weight_by_node.put(Pair(u,v), inf);
        }
        weight_by_node.put(source, 0);
        
        for (int i=0, i<n-1; i++) {
            for (Pair (u,v) : weight_by_edge.keySet()) {
                weigh_by_edge.put(Pair(u,v),
                        Math.min(
                            weight_by_node.get(u) + weigh_by_edge.get(Pair(u,v)),
                            weight_by_node.get(v)
                        )
                )
            }
        }
        
        return weight_by_node;
    }
}
