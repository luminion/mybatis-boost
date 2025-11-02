package io.github.luminion.mybatis.util;


import io.github.luminion.mybatis.core.Booster;
import io.github.luminion.mybatis.core.SFunction;
import lombok.SneakyThrows;
import org.springframework.beans.BeanUtils;
import org.springframework.core.GenericTypeResolver;

import java.beans.PropertyDescriptor;
import java.lang.invoke.*;
import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * MyBatis反射工具类
 * <p>
 * 提供针对MyBatis-Plus的反射工具方法，包括泛型解析、字段映射等
 *
 * @author luminion
 */
public abstract class BoostUtils {

    /**
     * 实体类字段到数据库列的映射缓存
     */
    private static final Map<Class<?>, Map<String, String>> JAVA_FIELD_TO_JDBC_COLUMN_CACHE_MAP = new ConcurrentHashMap<>();

    /**
     * 获取指定类对应的表名
     *
     * @param clazz 实体类
     * @return {@link String} 表名称
     */
    public static String getEntityTableName(Class<?> clazz) {
        return clazz.getSimpleName();
    }

    /**
     * 获取ID字段属性名
     *
     * @param clazz 实体类
     * @return {@link String} ID字段属性名
     */
    public static String getIdPropertyName(Class<?> clazz) {
        if (MybatisPlusUtils.isMybatisPlusEnv()) {
            return MybatisPlusUtils.getIdPropertyName(clazz);
        }
        PropertyDescriptor[] descriptors = BeanUtils.getPropertyDescriptors(clazz);
        // 获取名为id的属性, 若有, 返回该属性名, 若没有, 获取第一个属性, 并判断是否以id结尾, 若是, 返回该属性名
        String potentialId = null;
        for (PropertyDescriptor descriptor : descriptors) {
            String propertyName = descriptor.getName();
            if ("class".equals(propertyName)) {
                continue;
            }
            // 优先精确匹配名为 "id" 的属性（忽略大小写）
            if ("id".equalsIgnoreCase(propertyName)) {
                return propertyName;
            }
            // 如果还没找到过备选ID，则将第一个以 "id" 结尾的属性作为备选
            if (potentialId == null && propertyName.toLowerCase().endsWith("id")) {
                potentialId = propertyName;
            }
        }
        if (potentialId != null) {
            return potentialId;
        }
        throw new IllegalStateException("No ID field found in " + clazz);
    }

    /**
     * 获取ID字段getter
     *
     * @param clazz 实体类
     * @return {@link SFunction} ID字段getter
     */
    @SneakyThrows
    public static <T, R> SFunction<T, R> getIdPropertyGetter(Class<T> clazz) {
        // This is the non-MyBatis-Plus implementation.
        // It dynamically creates a serializable lambda for the ID getter.

        // 1. Find the ID property name and its getter method.
        String idPropertyName = getIdPropertyName(clazz);
        PropertyDescriptor descriptor = BeanUtils.getPropertyDescriptor(clazz, idPropertyName);
        if (descriptor == null) {
            throw new IllegalStateException("Could not find property descriptor for ID: " + idPropertyName);
        }
        Method getter = descriptor.getReadMethod();
        if (getter == null) {
            throw new IllegalStateException("Could not find getter for ID property: " + idPropertyName);
        }

        // 2. Use LambdaMetafactory to create a serializable lambda function.
        MethodHandles.Lookup lookup = MethodHandles.lookup();

        // The MethodHandle for the getter.
        MethodHandle getterHandle = lookup.unreflect(getter);

        // The type of the SAM (Single Abstract Method) in SFunction.
        // It's `R apply(T t)`, so its type is `(T) -> R`.
        // We use the getter's return type for R.
        MethodType samMethodType = MethodType.methodType(getter.getReturnType(), clazz);

        // The type of the function object we want to create.
        MethodType factoryType = MethodType.methodType(SFunction.class);

        // The signature of the SAM as it will be implemented.
        MethodType implType = getterHandle.type();

        CallSite site = LambdaMetafactory.metafactory(
                lookup,
                "apply", // The name of the abstract method in SFunction (from Function interface)
                factoryType,
                samMethodType,
                getterHandle,
                implType
        );

        // Create an instance of the lambda and cast it.
        @SuppressWarnings("unchecked")
        SFunction<T, R> getterLambda = (SFunction<T, R>) site.getTarget().invokeExact();
        return getterLambda;
    }

    /**
     * 获取getter方法对应的java类属性名
     *
     * @param getter getter方法
     * @return {@link String} java类属性名
     */
    @SneakyThrows
    public static String getGetterPropertyName(SFunction<?, ?> getter) {
        if (MybatisPlusUtils.isMybatisPlusEnv()) {
            return MybatisPlusUtils.getGetterPropertyName(getter);
        }
        Method lambdaMethod = getter.getClass().getDeclaredMethod("writeReplace");
        lambdaMethod.setAccessible(Boolean.TRUE);
        SerializedLambda serializedLambda = (SerializedLambda) lambdaMethod.invoke(getter);
        return serializedLambda.getImplMethodName();
    }

    
    
    @SneakyThrows
    @SuppressWarnings({"unchecked", "ConstantConditions"})
    public static <T, V> Class<T> getEntityClass(Booster<T, V> booster) {
        return (Class<T>) GenericTypeResolver
                .resolveTypeArguments(booster.getClass(), Booster.class)[0];
    }

    @SneakyThrows
    @SuppressWarnings({"unchecked", "ConstantConditions"})
    public static <T, V> Class<V> getViewObjectClass(Booster<T, V> booster) {
        return (Class<V>) GenericTypeResolver
                .resolveTypeArguments(booster.getClass(), Booster.class)[1];
    }

    
    /**
     * 获取实体类属性与数据库字段的映射关系
     * 包含:
     * 1.mybatis-plus实体类属性与字段映射信息
     * 2.mybatis-plus注解指定的映射信息
     * 3.实现了EnhanceEntity接口的映射信息
     *
     * @param entityClass 实体类
     * @return {@link Map} 字段到列的映射关系
     */
    public static Map<String, String> javaFieldToJdbcColumnMap(Class<?> entityClass) {
        Map<String, String> map = JAVA_FIELD_TO_JDBC_COLUMN_CACHE_MAP.get(entityClass);
        if (map != null) {
            return map;
        }
        String format = "a.%s";
        LinkedHashMap<String, String> result = new LinkedHashMap<>();
        if (MybatisPlusUtils.isMybatisPlusEnv()) {
            Map<String, String> fieldToJdbcColumnMap = MybatisPlusUtils.javaFieldToJdbcColumnMap(entityClass);
            fieldToJdbcColumnMap.forEach((key, value) -> {
                result.putIfAbsent(key, String.format(format, value));
            });
        }
        JAVA_FIELD_TO_JDBC_COLUMN_CACHE_MAP.put(entityClass, result);
        return result;
    }
}
