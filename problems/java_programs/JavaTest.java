package java_programs;
import java.util.*;

public class JavaTest {
    public static void main(String[] args) throws Exception {
        Node station1 = new Node("Westminster", new ArrayList<Node>());
        Node station2 = new Node("Waterloo", new ArrayList<Node>(Arrays.asList(station1)));
        Node station3 = new Node("Trafalgar Square",  new ArrayList<Node>(Arrays.asList(station1, station2)));
        Node station4 = new Node("Canary Wharf",  new ArrayList<Node>(Arrays.asList(station2, station3)));
        Node station5 = new Node("London Bridge",  new ArrayList<Node>(Arrays.asList(station4, station3)));
        Node station6 = new Node("Tottenham Court Road",  new ArrayList<Node>(Arrays.asList(station5, station4)));

        BREADTH_FIRST_SEARCH bfs = new BREADTH_FIRST_SEARCH();

        if (bfs.breadth_first_search(station6, station1))
            System.out.println("Path Found!");
        else
            System.out.println("Path Not Found!");

        DEPTH_FIRST_SEARCH dfs = new DEPTH_FIRST_SEARCH();

        if (dfs.depth_first_search(station6, station1))
            System.out.println("Path Found!");
        else
            System.out.println("Path Not Found!");

        Node node1 = new Node("1");
        Node node2 = new Node("2", node1);
        Node node3 = new Node("3", node2);
        Node node4 = new Node("4", node3);
        Node node5 = new Node("5", node4);

        REVERSE_LINKED_LIST rll = new REVERSE_LINKED_LIST();

        Node result = rll.reverse_linked_list(node5);

        if (result.getValue() == node1.getValue()) {
            System.out.println("Reversed!");
        }

        while (result != null) {
            System.out.printf("%s ", result.getValue());
            result = result.getSuccessor();
        }
        System.out.println();

        node1.setSuccessor(node2);

        DETECT_CYCLE dc = new DETECT_CYCLE();

        if (dc.detect_cycle(node5)) {
            System.out.println("Cycle detected!");
        } else {
            System.out.println("Cycle not detected!");
        }

        WeightedEdge edge1 = new WeightedEdge(node1, node2, 10);
        WeightedEdge edge2 = new WeightedEdge(node2, node3, 15);
        WeightedEdge edge3 = new WeightedEdge(node3, node4, 10);
        WeightedEdge edge4 = new WeightedEdge(node1, node4, 10);

        List<WeightedEdge> graph = new ArrayList<>(Arrays.asList(edge1, edge2, edge3, edge4));
        MINIMUM_SPANNING_TREE mst = new MINIMUM_SPANNING_TREE();

        Set<WeightedEdge> minspantree = new HashSet<>();
        minspantree.addAll(mst.minimum_spanning_tree(graph));

        for (WeightedEdge edge : minspantree) {
            System.out.printf("u: %s, v: %s, weight: %d\n", edge.node1.getValue(), edge.node2.getValue(), edge.weight);
        }

    }
}