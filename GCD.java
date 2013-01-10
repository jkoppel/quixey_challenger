import java.util.*;
import java.io.*;
import java.awt.Point;
import static java.lang.Math.*;
public class GCD
{
  public static void main (String[] args)
  {
    Scanner in = new Scanner(System.in);
    while (in.hasNextInt())
    {
      int x = in.nextInt();
      int y = in.nextInt();
      System.out.println(gcd(x, y));
    }
  }
  static int gcd (int a, int b)
  {
    if (b == 0)
    {
      return a;
    }
    return gcd(b, a % b);
  }
}
