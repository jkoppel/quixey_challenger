import java.util.*;
import java.io.*;
import java.awt.Point;
import static java.lang.Math.*;

public class GCD {
    public static boolean pre(int x, int y) {
        return x>=0 && y>=0;
    }
    static int real_gcd(int x, int y) {
        return y==0 ? x : gcd(y, x%y);
    }
    static int post(int x, int y, int d) {
        return d == real_gcd(x,y);
    }

    static int gcd(int a, int b) {
        if(b==0) {
            return a;
        }
        return gcd(a%b, b);
    }
}
