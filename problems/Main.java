import java.util.*;
import java.lang.reflect.*;
import quixey.*;
/*
 */

/**
 *
 */
public class Main {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) throws Exception {
        // .class?
        // getMethod, getDeclaredMethod, getDeclaredMethods, getMethods
        String sMethodName = args[0];
        String[] input_args = Arrays.copyOfRange(args, 1, args.length);
        String sClassName = sMethodName.toUpperCase();

        try {
            Class target_class = Class.forName("quixey."+sClassName);
            Method[] classMethods = target_class.getDeclaredMethods();
            for (Method m : classMethods) {
                if (m.getName().equals(sMethodName)) {
                    Type[] types = m.getGenericParameterTypes();
                    int length = input_args.length;
                    Object[] obj_args = new Object[length];

                    for (int i=0; i<length; i++) {
                        Type type = types[i];
                        String arg = input_args[i];


                        if (type.equals(int.class) || type.equals(Integer.class)) {
                            obj_args[i] = new Integer(Integer.parseInt(arg));
                        } else if (type.equals(String.class)) {
                            obj_args[i] = arg;
                        } else {
                            obj_args[i] = arg; // do some transformation?
                        }
                    }



                    String returnValue = (String) m.invoke(null, obj_args);
                    // If the underlying method is static, then the specified obj argument is ignored. It may be null.
                    // If the number of formal parameters required by the underlying method is 0, the supplied args array may be of length 0 or null.
                    // If the underlying method is static, the class that declared the method is initialized if it has not already been initialized.
                    // If the method completes normally, the value it returns is returned to the caller of invoke; if the value has a primitive type,
                    // it is first appropriately wrapped in an object. However, if the value has the type of an array of a primitive type, the elements
                    // of the array are not wrapped in objects; in other words, an array of primitive type is returned. If the underlying method return
                    // type is void, the invocation returns null.

                    System.out.println(returnValue);
                }
            }

        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        } catch (Exception e) {
            System.out.println(e.getCause());
        }

    }
}
