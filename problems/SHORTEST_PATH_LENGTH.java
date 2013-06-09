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
    public static Integer shortest_path_length(Map<Pair<Node,Node>,Integer> length_by_edge, Node startnode, Node goalnode ) {
        
        PriorityQueue<Pair<Node,Integer>> unvisited_nodes = new PriorityQueue<Pair<Node,Integer>>(30, new Comparator<Pair<Node,Integer>>() {
 
            @Override
            public int compare(Pair<Node,Integer> pair1, Pair<Node,Integer> pair2) {
                return Integer.compare(pair1.second, pair2.second);
            }
        });        
                
        unvisited_nodes.add(new Pair<Node,Integer>(startnode, 0));
        Set<Node> visited_nodes = new HashSet<Node>();
        
        while (!unvisited_nodes.isEmpty()) {
            Pair<Node,Integer> popped = unvisited_nodes.poll();
            Node node = popped.getFirst();
            Integer distance = popped.getSecond();
            if (node == goalnode) {
                return distance;
            }
            
            visited_nodes.add(node);
            
            for (Node nextnode : node.successors()) { // outgoing nodes should just be successors
                if (visited_nodes.contains(nextnode)) {
                    continue;
                }
                
                Integer new_value;
                Integer alt_value = distance + length_by_edge.get(new Pair<Node,Node>(node,nextnode));
                new_value = alt_value; // if nextnode isn't in unvisited, this is default
                Iterator<Pair<Node,Integer>> hacky = unvisited_nodes.iterator();
                
                do {
                    Pair<Node,Integer> current = hacky.next();
                    if (current.getFirst().equals(nextnode)) {
                        new_value = Math.min(current.getSecond(), alt_value);
                    }
                } while (hacky.hasNext());
                                
                Pair<Node,Integer> to_insert = new Pair<Node,Integer>(nextnode,new_value);
                
                hacky = unvisited_nodes.iterator();
                boolean update = false;
                do {
                    Pair<Node,Integer> current = hacky.next();
                    if (current.getFirst().equals(nextnode)) {
                        // update
                        unvisited_nodes.remove(current);
                        unvisited_nodes.add(to_insert);
                        update = true;
                        break;
                        
                    }
                    
                } while (hacky.hasNext());
                
                if (!update) {
                    // insert
                    unvisited_nodes.add(to_insert);
                }
            }
        }
        
        return Integer.MAX_VALUE;
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
