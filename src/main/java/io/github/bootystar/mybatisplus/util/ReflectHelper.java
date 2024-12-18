package io.github.bootystar.mybatisplus.util;

import lombok.SneakyThrows;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author bootystar
 */
public abstract class ReflectHelper {

    private static final Map<Class<?>, Map<String, Field>> FIELD_MAP_CACHE = new ConcurrentHashMap<>();


    /**
     * 是否为java自带的核心类
     *
     * @param clazz 克拉兹
     * @return boolean
     * @author bootystar
     */
    public static boolean isJavaCoreClass(Class<?> clazz) {
        if (clazz == null) {
            return false;
        }
        return clazz.getClassLoader() == null;
    }

    /**
     * 新建实例
     *
     * @param clazz 克拉兹
     * @return {@link T }
     * @author bootystar
     */
    @SneakyThrows
    public static <T> T newInstance(Class<T> clazz) {
        return clazz.getConstructor().newInstance();
    }

    /**
     * 指定类属性map
     *
     * @param clazz 类
     * @return {@link Map }<{@link String }, {@link Field }>
     * @author bootystar
     */
    public static Map<String, Field> fieldMap(Class<?> clazz) {
        if (isJavaCoreClass(clazz)) {
            throw new IllegalArgumentException("clazz must not be java class");
        }
        Map<String, Field> stringFieldMap = FIELD_MAP_CACHE.get(clazz);
        if (stringFieldMap != null) {
            return stringFieldMap;
        }
        Map<String, Field> map = new HashMap<>();
        while (clazz != null && Object.class != clazz && !clazz.isInterface()) {
            Field[] fields = clazz.getDeclaredFields();
            for (Field field : fields) {
                field.setAccessible(true);
                if (isSpecialModifier(field.getModifiers())) {
                    continue;
                }
                map.putIfAbsent(field.getName(), field);
            }
            clazz = clazz.getSuperclass();
        }
        FIELD_MAP_CACHE.put(clazz, map);
        return map;
    }

    public static boolean isSpecialModifier(int modifiers) {
        return Modifier.isStatic(modifiers)
                || Modifier.isFinal(modifiers)
                || Modifier.isNative(modifiers)
                || Modifier.isVolatile(modifiers)
                || Modifier.isTransient(modifiers)
                ;
    }


    /**
     * 复制属性
     *
     * @param source 来源
     * @param target 目标
     * @return {@link T }
     * @author bootystar
     */
    @SneakyThrows
    public static <T> T copyFieldProperties(Object source, T target) {
        if (source == null || target == null || source.equals(target)) return target;
        Map<String, Field> sourceMap = fieldMap(source.getClass());
        Map<String, Field> targetMap = fieldMap(target.getClass());
        for (Field field : sourceMap.values()) {
            Object o = field.get(source);
            if (o == null) continue;
            Field targetFiled = targetMap.get(field.getName());
            if (targetFiled != null && targetFiled.getType().isAssignableFrom(field.getType())) {
                targetFiled.set(target, o);
            }
        }
        return target;
    }


    /**
     * 对象转map
     *
     * @param source 来源
     * @return {@link Map }<{@link ? }, {@link ? }>
     * @author bootystar
     */
    @SneakyThrows
    public static Map<?, ?> objectToMap(Object source) {
        if (source == null) return null;
        if (source instanceof Map) return (Map<?, ?>) source;
        HashMap<String, Object> map = new HashMap<>();
        Collection<Field> fields = fieldMap(source.getClass()).values();
        for (Field field : fields) {
            Object o = field.get(source);
            if (o == null) continue;
            map.put(field.getName(), o);
        }
        return map;
    }

    /**
     * 对象转对象
     *
     * @param source 来源
     * @param clazz  目标类
     * @return {@link U }
     * @author bootystar
     */
    public static <U> U toTarget(Object source, Class<U> clazz) {
        return copyFieldProperties(source, newInstance(clazz));
    }

}
