package dreme.macros.interop;

import dreme.*;
import dreme.Number;
import org.apache.commons.lang.reflect.ConstructorUtils;
import org.apache.commons.lang.reflect.MethodUtils;

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;

public final class ConversionUtils {
    private ConversionUtils() {
        // prevent instantiation
    }

    public static SchemeObject invokeConstructor(String className, List arguments) {
        Class<?> clazz = toClass(className);
        try {
            if (arguments.isEmpty())
                return toSchemeObject(clazz.newInstance());
            Object[] javaArgs = toJavaObjects(arguments);
            return toSchemeObject(ConstructorUtils.invokeConstructor(clazz, javaArgs));
        }
        catch (InstantiationException e) {
            throw new RuntimeException("Failed to construct " + clazz.getName() + " with arguments: " + arguments, e);
        }
        catch (InvocationTargetException e) {
            throw new RuntimeException("Failed to construct " + clazz.getName() + " with arguments: " + arguments, e);
        }
        catch (IllegalAccessException e) {
            throw new RuntimeException("Could not access constructor on " + clazz.getName() + " for arguments: " + arguments, e);
        }
        catch (NoSuchMethodException e) {
            throw new RuntimeException("Constructor not found on " + clazz.getName() + " for arguments: " + arguments, e);
        }
    }

    private static Class<?> toClass(String className) {
        try {
            return ConversionUtils.class.getClassLoader().loadClass(className);
        }
        catch (ClassNotFoundException e) {
            throw new IllegalArgumentException("Could not load class: " + className, e);
        }
    }

    public static SchemeObject toSchemeObject(Object javaObject) {
        if (javaObject == null) {
            return Unspecified.INSTANCE;
        }
        if (javaObject instanceof Double) {
            return new Number((Double) javaObject);
        }
        if (javaObject instanceof Integer) {
            return new Number((Integer) javaObject);
        }
        if (javaObject instanceof String) {
            return new SchemeString((String) javaObject);
        }
        if (javaObject instanceof Character) {
            return new SchemeString(new String(new char[] { (Character) javaObject }));
        }
        return new JavaSchemeObject(javaObject);
    }

    public static Object toJavaObject(SchemeObject schemeObject) {
        ToJavaObjectVisitor visitor = new ToJavaObjectVisitor();
        schemeObject.acceptVisitor(visitor);
        return visitor.getJavaObject();
    }

    public static Object[] toJavaObjects(List schemeObjects) {
        java.util.List<Object> result = new java.util.ArrayList<Object>();
        for (SchemeObject schemeObject : schemeObjects) {
            result.add(toJavaObject(schemeObject));
        }
        return result.toArray(new Object[result.size()]);
    }

    public static SchemeObject invokeMethod(SchemeObject object, String methodName, List arguments) {
        Object instance = toJavaObject(object);
        Class<?> clazz = instance.getClass();
        Object[] javaArgs = toJavaObjects(arguments);
        String invocation = clazz.getName() + "." + methodName + "(" + Arrays.toString(javaArgs) + ")";
        try {
            return toSchemeObject(MethodUtils.invokeMethod(instance, methodName, javaArgs));
        }
        catch (NoSuchMethodException e) {
            throw new RuntimeException("Method not found: " + invocation, e);
        }
        catch (InvocationTargetException e) {
            throw new RuntimeException("Method invocation failed: " + invocation, e.getCause());
        }
        catch (IllegalAccessException e) {
            throw new RuntimeException("Could not access method: " + invocation, e);
        }
    }

    private static class ToJavaObjectVisitor extends AbstractSchemeObjectVisitor {
        private Object javaObject = null;

        private Object getJavaObject() {
            return javaObject;
        }

        public void string(SchemeString string) {
            javaObject = string.getValue();
        }

        public void number(Number number) {
            // hack: this only works with methods that take an 'int', not a 'long'
            if (number.isInteger()) {
                javaObject = number.intValue();
            }
            else {
                javaObject = number.getValue();
            }
        }

        public void object(SchemeObject schemeObject) {
            if (!(schemeObject instanceof JavaSchemeObject)) {
                throw new IllegalArgumentException("Can't unwrap non-Java object: " + schemeObject);
            }
            javaObject = ((JavaSchemeObject) schemeObject).getJavaObject();
        }

        public void unspecified(Unspecified unspecified) {
            javaObject = null;
        }
    }

    private static class JavaSchemeObject implements SchemeObject {
        private final Object javaObject;

        private JavaSchemeObject(Object javaObject) {
            this.javaObject = javaObject;
        }

        public Object getJavaObject() {
            return javaObject;
        }

        public void acceptVisitor(SchemeObjectVisitor visitor) {
            visitor.object(this);
        }

        public String toString() {
            return javaObject.getClass().getName() + "(" + javaObject.toString() + ")";
        }
    }
}
