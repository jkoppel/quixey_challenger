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

                        if (type instanceof ParameterizedType) {
                            type = ((ParameterizedType) type).getRawType();
                        };
                        obj_args[i] = parser((Class) type, arg);
                    }



                    String returnValue = String.valueOf(m.invoke(null, obj_args));
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
            System.out.println("aww");
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("nuu"+e.getCause());
        }

    }

    public static Object parser(Class type, String arg) {
        System.out.println(String.valueOf(type));
        if (type == Object.class) {
            // try to figure it out by looking at the first char
            // ' String, python
            // [ List
            // integer, etc
            char firstchar = arg.charAt(0);
            if (firstchar == '\'') {
                return arg.substring(1,arg.length()-1);
            } else if ("0123456789".indexOf(firstchar) != -1) {
                return new Integer(Integer.parseInt(arg));
            } else if (arg.charAt(0)=='[') {
                // list
                String[] args = arg.substring(1,arg.length()-1).split("\\s*,\\s*");
                int length = args.length;
                List to_return = new ArrayList();

                for (String current_arg : args) {
                    to_return.add(parser(Object.class,current_arg));
                }

                return to_return;
            }
        }
        if (type.equals(int.class) || type.equals(Integer.class)) {
            return new Integer(Integer.parseInt(arg));
        } else if (type.equals(String.class)) {
            return arg;
        } else if (type.isAssignableFrom(List.class)) {
            System.out.println("hit list condition");
            String[] args = arg.substring(1,arg.length()-1).split("\\s*,\\s*");
            int length = args.length;
            List to_return = new ArrayList();

            if ((Type) type instanceof ParameterizedType) {
                Type[] generic_types = ((ParameterizedType) (Type) type).getActualTypeArguments();

                for (int i=0; i<length; i++) {
                    String current_arg = args[i];
                    Type current_type = generic_types[i];
                    to_return.add(parser((Class) current_type,current_arg));
                }
            } else {
                for (String current_arg : args) {
                    to_return.add(parser(Object.class,current_arg));
                }
            }

            return to_return;
        //} else if (type.equals(Array.class)) {
        //    Type generic_type = ((ParameterizedType) (Type) type).getActualTypeArguments()[0];
        //    String[] args = arg.substring(1,arg.length()-1).split("\\s*,\\s*");
        //    int length = args.length;
        //    Object[] to_return = new Object[length];

        //    for (int i =0; i < length; i++) {
        //        String current_arg = args[i];
        //        to_return[i] = parser((Class) generic_type,current_arg);
        //    }

        //    return to_return;
        } else {
            return arg; // do some transformation?
        }
    }
}
