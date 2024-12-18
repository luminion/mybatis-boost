package io.github.bootystar.mybatisplus.generate.info;

import lombok.*;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

/**
 * @author bootystar
 */
@Getter
@NoArgsConstructor
public class MethodInfo extends ClassInfo {
    protected String methodName;
    protected int methodGenericTypeCount;
    protected boolean isStatic;
    protected boolean isConstructor;
    protected boolean isGenericMethod;

    public MethodInfo(Method method) {
        super(method.getDeclaringClass());
        this.methodName = method.getName();
        this.methodGenericTypeCount = method.getTypeParameters().length;
        this.isStatic = Modifier.isStatic(method.getModifiers());
        this.isConstructor = method.getName().startsWith("<init>");
    }

    public MethodInfo(Constructor<?> method) {
        super(method.getDeclaringClass());
        this.methodName = method.getName();
        this.methodGenericTypeCount = method.getTypeParameters().length;
        this.isStatic = Modifier.isStatic(method.getModifiers());
        this.isConstructor = true;
    }

    public String method(String parametersStr) {
        if (methodName == null) {
            return parametersStr;
        }
        if (isStatic) {
            return String.format("%s.%s(%s)" , classSimpleName, methodName, parametersStr);
        }
        if (isConstructor) {
            return String.format("new %s%s(%s)" , classSimpleName, classGenericTypeCount == 1 ? "<>" : "" , parametersStr);
        }
        if (isGenericMethod) {
            return String.format("new %s<>().%s(%s)" , classSimpleName, methodName, parametersStr);
        }
//        if (classGenericTypeCount == 1) {
//            return String.format("new %s<%s>().%s(%s)" , classSimpleName, returnGenericTypeStr, methodName, parametersStr);
//        }
        return String.format("new %s().%s(%s)" , classSimpleName, methodName, parametersStr);
    }

}
