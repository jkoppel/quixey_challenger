import java.util.*;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author derricklin
 */
public class SHORTEST_PATHS {
    public static Map<Character, Integer> shortest_paths(char source, Map<Character, Map<Character,Integer>> weight_by_edge) {
        Map<Character,Integer> weight_by_node = new HashMap<Character,Integer>();
        for (char u : weight_by_edge.keySet()) {
            for (char v : weight_by_edge.get(u).keySet()) {
                weight_by_node.put(v, Integer.MAX_VALUE);
            }
        }
        weight_by_node.put(source,0);
        
        for (int i=0; i<weight_by_node.size()-1; i++) {
            for (char u : weight_by_edge.keySet()) {
                for (char v : weight_by_edge.get(u).keySet()) {
                    Integer weight = weight_by_edge.get(u).get(v);
                    Integer ins_value = Math.min(
                            weight_by_node.get(u)+weight, 
                            weight_by_node.get(v));
                    
                    Map<Character,Integer> to_update = weight_by_edge.get(u);
                    to_update.put(v, ins_value);
                    weight_by_edge.put(u, to_update);
                }
            }
        }
        
        return weight_by_node;
    }
}
