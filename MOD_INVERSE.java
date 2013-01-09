import java.util.*;
import java.io.*;
import java.awt.Point;
import static java.lang.Math.*;

public class MOD_INVERSE {
    public static void main(String[] args) throws Exception {
        Scanner in = new Scanner(System.in);
        int base = in.nextInt();
        int mod = in.nextInt();
        System.out.println(inverse(base, mod));
    }
    static int inverse(int base, int mod) {
        if(base == 1) {
            return base;
        } else {
            int coeff = base - inverse(mod%base, base);
            return (coeff*mod+1)/base;
        }
    }
}
