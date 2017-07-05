
/**
 *  This program takes in an algorithm and runs the corresponding java program
 *  with the json test cases as input and gets the return value from the program.
 */

import java.util.*;
import java.lang.reflect.*;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java_programs.*;

import com.google.gson.Gson;

public class JavaDeserialization {
    public static void main(String[] args) throws Exception {

        // Check if passing in proper arguments.
        if(args.length < 2)
        {
            System.out.println("Not enough arguments.");
            System.exit(0);
        }

        String methodName = args[0];
        String className = methodName.toUpperCase();

        // Get parameter type for class method.
        try {
            Class target_class = Class.forName("java_programs." + className);
            Method[] method = target_class.getDeclaredMethods();

            for (Method m : method) {
                if (!m.getName().equals(methodName)) {
                    continue;
                }

                Type[] types = m.getGenericParameterTypes();
                Object[] parameters = getParameters(types, args);

                try {
                    String returnValue = String.valueOf(m.invoke(null, parameters));
                    System.out.println(returnValue);
                } catch (IllegalArgumentException e) {
                    System.out.println("Invalid parameters.: Mismatch types or wrong number of arguments.");
                }
            }
        } catch (ClassNotFoundException e) {
            System.out.println(className + " Class is not found.");
        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("nuu :( "+e.getCause());
        }
    }

    // Create list of objects corresponding to input arguments through deserialization
    public static Object[] getParameters(Type[] types,  String[] args){
        int numOfParameters= types.length;
        Object[] parameters = new Object[numOfParameters];
        Gson gsonArguments = new Gson();
        try {
            if (numOfParameters == args.length - 1) {
                for (int i = 0; i < numOfParameters; i++) {
                    Type type = types[i];
                    System.out.println(type);
                    System.out.println(args[i+1]);
                    parameters[i] = gsonArguments.fromJson(args[i + 1], (Class)type);
                }
            }

        } catch (NumberFormatException e){
            System.out.println("Incompatible types: Object cannot be converted.");
        }
        return parameters;
    }

}
