import java.util.*;
import java.io.*;
import java.awt.Point;
import static java.lang.Math.*;
public class ADD
{
  public static void main (String[] args)
  {
    Scanner in = new Scanner(System.in);
    while (in.hasNextInt())
    {
      int x = in.nextInt();
      int y = in.nextInt();
      System.out.println(x+y-1);
    }
  }
}
