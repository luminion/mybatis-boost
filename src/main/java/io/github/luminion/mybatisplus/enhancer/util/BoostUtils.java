package io.github.luminion.mybatisplus.enhancer.util;


import io.github.luminion.mybatisplus.enhancer.core.BoostCore;
import io.github.luminion.mybatisplus.enhancer.core.SFunction;
import lombok.SneakyThrows;
import org.springframework.core.GenericTypeResolver;

import java.lang.invoke.SerializedLambda;
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
        String idFieldName = "id";
        return idFieldName;
    }

    /**
     * 获取ID字段getter
     *
     * @param clazz 实体类
     * @return {@link String} ID字段getter
     */
    public static <T, R> SFunction<T, R> getIdPropertyGetter(Class<T> clazz) {
        // 完善该方法
        return null;
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
    public static <T, V> Class<T> getEntityClass(BoostCore<T, V> boostCore) {
        return (Class<T>) GenericTypeResolver
                .resolveTypeArguments(boostCore.getClass(), BoostCore.class)[0];
    }

    @SneakyThrows
    @SuppressWarnings({"unchecked", "ConstantConditions"})
    public static <T, V> Class<V> getViewObjectClass(BoostCore<T, V> boostCore) {
        return (Class<V>) GenericTypeResolver
                .resolveTypeArguments(boostCore.getClass(), BoostCore.class)[1];
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