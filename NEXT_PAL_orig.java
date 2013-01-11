import java.util.*;

public class NEXT_PAL {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        CAS: while(in.hasNextLine()) {
            String[] arr = in.nextLine().split(" ");
            int[] digit_list = new int[arr.length];
            for(int i=0; i<digit_list.length; i++)
                digit_list[i] = Integer.parseInt(arr[i]);

            int high_mid = digit_list.length/2;
            int low_mid = (digit_list.length-1)/2;
            while((high_mid < digit_list.length) && (low_mid>=0)) {
                if(digit_list[high_mid] == 9) {
                    digit_list[high_mid] = 0;
                    digit_list[low_mid] = 0;
                    high_mid+=1;
                    low_mid-=1;
                } else {
                    digit_list[high_mid] += 1;
                    if(low_mid != high_mid)
                        digit_list[low_mid] += 1;
                    for(int i=0; i<digit_list.length; i++)
                        System.out.print(digit_list[i]+" ");
                    System.out.println();
                    continue CAS;
                }
            }

            int[] RET = new int[1+digit_list.length+1];
            RET[0] = 1;
            RET[RET.length-1] = 1;
            for(int i=0; i<RET.length; i++)
                System.out.print(RET[i]+" ");
            System.out.println();
        }
    }
}
