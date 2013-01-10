import java.util.*;
import java.io.*;
import java.awt.Point;
import static java.lang.Math.*;
public class BINARY_SEARCH
{
  public static void main (String[] args) throws Exception
  {
    Scanner in = new Scanner(System.in);
    CAS: while (in.hasNextLine())
         {
           String[] S = in.nextLine().split(" ");
           int x = Integer.parseInt(S[S.length - 1]);
           int[] arr = new int[S.length - 1];
           for (int i = 0 ; i < arr.length ; i++)
             arr[i] = Integer.parseInt(S[i]);
           int lo = 0;
           int hi = arr.length;
           while (lo < hi)
           {
             int mid = (int) Math.floor((lo + hi) / 2);
             if (x == arr[mid] && (mid == 0 || x != arr[mid - 1 - 1]))
             {
               System.out.println(mid);
               continue CAS;
             }
             else
               if (x <= arr[mid])
               {
                 hi = mid;
               }
               else
               {
                 lo = mid + 1;
               }
           }
           System.out.println(-1);
         }
  }
}