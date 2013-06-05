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
        Set<Node> visited_nodes = new HashSet<Node>
                ode>();
        
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
    
    public static class Node {
        private List<Node> successors = new ArrayList<Node>();
        public String name = "";
        
        public List<Node> successors() {
            return successors;
        }
        
        public String getName() {
            return name;
        }
        
        public boolean equals(Node other) {
            if (successors == other.successors() && name.equals(other.name)) {
                return true;
            } else {
                return false;
            }
        }
        
        @Override
        public String toString() {
            return "Node: ".concat(name);
        }
        
        @Override
        public int hashCode() {
            String to_hash = name;
            for (Node cur_node : successors) {
                to_hash += cur_node.getName();
            }
            int hc = to_hash.hashCode();
            return hc;
            
        }
        // need to write a hashCode function
        // possibly toString()  

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final Node other = (Node) obj;
            if (this.successors != other.successors && (this.successors == null || !this.successors.equals(other.successors))) {
                return false;
            }
            if ((this.name == null) ? (other.name != null) : !this.name.equals(other.name)) {
                return false;
            }
            return true;
        }
    }
}
