import java.util.*;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author derricklin
 */
public class MINIMUM_SPANNING_TREE {
    public static Set<Pair<Node,Node>> minimum_spanning_tree(Map<Pair<Node,Node>,Integer> weight_by_edge) {
        HashMap<> group_by_node = new HashMap<>();
        Set<Pair<Node,Node>> mst_edges = new HashSet<Pair<Node,Node>>();
        
        // setdefault returns val if key in dict, otherwise insert key with default and return default val
    }
    
    
    
    
    public static class Node {
        private List<SHORTEST_PATH_LENGTH.Node> successors = new ArrayList<SHORTEST_PATH_LENGTH.Node>();
        public String name = "";
        
        public List<SHORTEST_PATH_LENGTH.Node> successors() {
            return successors;
        }
        
        public String getName() {
            return name;
        }
        
        public boolean equals(SHORTEST_PATH_LENGTH.Node other) {
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
            for (SHORTEST_PATH_LENGTH.Node cur_node : successors) {
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
            final SHORTEST_PATH_LENGTH.Node other = (SHORTEST_PATH_LENGTH.Node) obj;
            if (this.successors != other.successors && (this.successors == null || !this.successors.equals(other.successors))) {
                return false;
            }
            if ((this.name == null) ? (other.name != null) : !this.name.equals(other.name)) {
                return false;
            }
            return true;
        }
    }
    
    
    public static class Pair<F, S> {
        private F first; //first member of pair
        private S second; //second member of pair

        public Pair(F first, S second) {
            this.first = first;
            this.second = second;
        }

        public void setFirst(F first) {
            this.first = first;
        }

        public void setSecond(S second) {
            this.second = second;
        }

        public F getFirst() {
            return first;
        }

        public S getSecond() {
            return second;
        }
    }    
}
