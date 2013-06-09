import java.util.*;
import java.lang.reflect.*;
/*
 */

/**
 *
 */
public class Main {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {        
        // .class?
        // getMethod, getDeclaredMethod, getDeclaredMethods, getMethods
        String sMethodName = args[0];
        String[] input_args = Arrays.copyOfRange(args, 1, args.length);
        String sClassName = sMethodName.toUpperCase();
        try {
            Class target_class = Class.forName(sClassName);
            Method[] classMethods = target_class.getDeclaredMethods();
            for (Method m : classMethods) {
                if (m.getName().equals(sMethodName)) {
                    Type[] types = m.getGenericParameterTypes();
                    
                    
                    Object.class.isAssignableFrom(types[0].getClass());
                    // how to dynamically cast and then invoke as params??
                            
                            
                    String returnValue = (String) target_class.invoke(m, new Object[] { methodParameter });
                    System.out.println(returnValue);
                }
            }
            
        } catch (ClassNotFoundException e) {
        } catch (NoSuchMethodException e) {
        }
       
    }
}
