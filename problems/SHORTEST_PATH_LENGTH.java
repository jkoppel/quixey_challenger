import java.util.*;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author derricklin
 */
public class SHORTEST_PATH_LENGTH {
    public static Double shortest_path_length(Map<Pair<Node, Node>, Integer> length_by_edge, Node startnode, Node goalnode ) {
        double inf = Double.POSITIVE_INFINITY;
        
        FibHeap unvisited_nodes = MinHeap();
        unvisited_nodes.insert(startnode, 0);
        Set<Node> visited_nodes = new Set<Node>();
        
        while (unvisited_nodes.size() > 0) {
            node, distance = unvisited_nodes.pop();
            if (node == goalnode) {
                return (double) distance;
            }
            
            visited_nodes.add(node);
            
            for (Node nextnode : node.outgoing_nodes) {
                if (visited_nodes.contains(nextnode)) {
                    continue
                }
                
                unvisited_nodes.insert_or_update(
                        nextnode,
                        Math.min(
                            unvisited_nodes.get(nextnode) or inf,
                            unvisited_nodes.get(nextnode) + length_by_edge.lookup(Pair(node, nextnode))
                        )
                )
            }
        }
        
        return inf;
    }
}
