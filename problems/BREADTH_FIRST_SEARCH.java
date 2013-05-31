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
                for (Object cur_node : node.successors()) {
                    if (!nodesseen.contains(cur_node)) {
                        queue.addFirst(cur_node);
                    }
                }
            }
        return false;            
        }
    }
}
