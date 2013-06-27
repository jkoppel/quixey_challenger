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
public class DEPTH_FIRST_SEARCH {

    public static Set<Node> nodesvisited = new HashSet<Node>();

    public static boolean depth_first_search(Node startnode, Node goalnode) {

        if (nodesvisited.contains(startnode)) {
            return false;
        } else if (startnode.equals(goalnode)) {
            return true;
        } else {
            for (Node nextnode : startnode.successors) {
                boolean check = depth_first_search(nextnode, goalnode);
                if (check == true) {
                    return true;
                }
            }
            return false;
        }
    }

    public class Node {
        List<Node> successors = new ArrayList<Node>();
        public List<Node> successors() {
            return successors;
        }
    }
}
