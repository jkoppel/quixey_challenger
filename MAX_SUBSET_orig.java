import java.util.*;
import java.io.*;
import java.awt.Point;
import static java.lang.Math.*;

public class MAX_SUBSET {
    public static void main(String[] args) throws Exception {
        Scanner in = new Scanner(System.in);
        while(in.hasNextLine()) {
            String[] str = in.nextLine().split(" ");
            int bound = Integer.parseInt(str[str.length-1]);
            int[] weights = new int[str.length-1];
            for(int i=0; i<weights.length; i++)
                weights[i] = Integer.parseInt(str[i]);

            int[][] maxWeight = new int[weights.length][bound];
            for(int w=0; w<=bound; w++) {
                /*if(weights[0] <= w)
                    maxWeight[0][w] = weights[0];
                else maxWreight[0][w] = 0;
                */
            }
            for(int i=1; i<weights.length; i++) {
                for(int w=0; w<=bound; w++) {
                    if(weights[i] > w) {
                        (maxWeight[i])[w] = maxWeight[i-1][w];
                    } else {
                        int include = weights[i] + maxWeight[i-1][w-weights[i]];
                        int exclude = maxWeight[i-1][w];
                        maxWeight[i][w] = Math.max(include, exclude);
                    }
                }
            }
            System.out.println(maxWeight[weights.length-1][bound]);
        }
    }
}
