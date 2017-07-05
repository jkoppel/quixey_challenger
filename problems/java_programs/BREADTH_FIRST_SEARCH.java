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
public class BREADTH_FIRST_SEARCH {
    public static boolean breadthfirstsearch(Node startnode, Node goalnode) {
        ArrayDeque<Node> queue = new ArrayDeque<Node>();
        queue.addLast(startnode);

        Set<Node> nodesseen = new HashSet<Node>();
        nodesseen.add(startnode);

        while (true) {
            Node node = queue.getFirst();
            queue.removeFirst();

            if (node == goalnode) {
                return true;
            } else {
                for (Node cur_node : node.successors()) {
                    if (!nodesseen.contains(cur_node)) {
                        queue.addFirst(cur_node);
                    }
                }
            }
        return false;
        }
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
