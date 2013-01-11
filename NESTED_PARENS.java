public class NESTED_PARENS {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        char[] S = in.next();
        int[] SA = new int[
        
    }

    public static int is_ok(int[] A) {
        int depth = 0;
        for(int i=0; i<A.length; i++) {
            if(A[i] == '(') {
                depth++;
            } else {
                depth--;
                if(depth < 0) return 0;
            }
        }
        return 1;
    }
}
