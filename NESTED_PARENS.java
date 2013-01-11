import java.util.*;

public class NESTED_PARENS {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        String S = in.next();
        int[] num = new int[S.length()];
        for(int i=0; i<S.length(); i++)
            num[i] = S.charAt(i)=='(' ? 1 : -1;

        System.out.println(is_ok(num)==1 ? "GOOD" : "BAD");
    }

    public static int is_ok(int[] A) {
        int depth;
        int i;
        depth = 0;
        i = 0;
        while(i < A.length) {
            depth = depth + A[i];
            if(depth < 0) { return 0; }
            i = i + 1;
        }
        return 1;
    }
}
