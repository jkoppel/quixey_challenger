import java.util.*;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author derricklin
 */
public class WRAP {
    public static ArrayList<String> wrap(String text, int cols) {
        ArrayList<String> lines = new ArrayList<String>();
        
        String line;
        while (text.length() > cols) {
            int end = text.lastIndexOf(" ");
            if (end == -1) {
                end = cols;
            }
            line = text.substring(0,end);
            text = text.substring(end);
            lines.add(line);
        }
        
        return lines;
    }
}
