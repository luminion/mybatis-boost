package io.github.luminion.mybatis.util;

import io.github.luminion.mybatis.core.MethodReference;
import lombok.SneakyThrows;
import org.springframework.beans.BeanUtils;
import org.springframework.core.GenericTypeResolver;
import org.springframework.util.ReflectionUtils;

import java.beans.PropertyDescriptor;
import java.lang.invoke.SerializedLambda;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 反射工具类
 * <p>
 * 提供通用的反射工具方法，包括实例创建、字段映射、属性复制等
 *
 * @author luminion
 */
public abstract class ReflectUtils {

    /**
     * 类字段映射缓存
     */
    private static final Map<Class<?>, Map<String, Field>> FIELD_MAP_CACHE = new ConcurrentHashMap<>();


    /**
     * 判断是否为Java核心类
     *
     * @param clazz 类
     * @return boolean 是否为Java核心类
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
     * @param clazz 类
     * @param <T>   实例类型
     * @return {@link T} 新实例
     */
    public static <T> T newInstance(Class<T> clazz) {
        if (clazz == null) {
            throw new IllegalArgumentException("clazz must not be null");
        }
        return BeanUtils.instantiateClass(clazz);
    }

    /**
     * 获取指定类的字段映射
     *
     * @param clazz 类
     * @return {@link Map} 字段名到字段的映射
     */
    public static Map<String, Field> fieldMap(Class<?> clazz) {
        if (clazz == null) {
            throw new IllegalArgumentException("clazz must not be null");
        }
        if (isJavaCoreClass(clazz)) {
            throw new IllegalArgumentException("clazz must not be java class");
        }
        return FIELD_MAP_CACHE.computeIfAbsent(clazz, k -> {
            Map<String, Field> map = new HashMap<>();
            ReflectionUtils.doWithFields(k, 
                    field -> map.putIfAbsent(field.getName(), field),
                    ReflectionUtils.COPYABLE_FIELDS);
            return map;
        });
    }


    /**
     * 复制属性
     *
     * @param source 来源对象
     * @param target 目标对象
     * @param <T>    目标对象类型
     * @return {@link T} 目标对象
     */
    public static <T> T copyFieldProperties(Object source, T target) {
        if (source == null || target == null) return target;
        BeanUtils.copyProperties(source, target);
        return target;
    }


    /**
     * 对象转map
     *
     * @param source 来源对象
     * @return {@link Map} 映射关系
     */
    @SneakyThrows
    @SuppressWarnings("unchecked")
    public static Map<String, Object> objectToMap(Object source) {
        if (source == null) return null;
        if (source instanceof Map) return (Map<String, Object>) source;
        HashMap<String, Object> map = new HashMap<>();
        Collection<Field> fields = fieldMap(source.getClass()).values();
        for (Field field : fields) {
            ReflectionUtils.makeAccessible(field);
            Object o = field.get(source);
            if (o == null) continue;
            map.put(field.getName(), o);
        }
        return map;
    }

    /**
     * 对象转对象
     *
     * @param source 来源对象
     * @param clazz  目标类
     * @param <T>    目标类型
     * @return {@link T} 目标对象
     */
    public static <T> T toTarget(Object source, Class<T> clazz) {
        if (source == null) {
            return null;
        }
        if (clazz == null) {
            throw new IllegalArgumentException("clazz must not be null");
        }
        return copyFieldProperties(source, newInstance(clazz));
    }

    /**
     * 解析超类泛型参数
     *
     * @param clazz      指定类
     * @param superClass 超类
     * @return {@link Class} 泛型参数数组
     */
    public static Class<?>[] resolveTypeArguments(Class<?> clazz, Class<?> superClass) {
        return GenericTypeResolver.resolveTypeArguments(clazz, superClass);
    }


    /**
     * 获取方法引用序列化后的Lambda信息
     *
     * @param getter 方法引用
     * @return {@link SerializedLambda} 序列化的Lambda信息
     */
    @SneakyThrows
    private static <T, R> SerializedLambda getSerializedLambda(MethodReference<T, R> getter) {
        Method writeReplaceMethod = getter.getClass().getDeclaredMethod("writeReplace");
        writeReplaceMethod.setAccessible(true);
        return (SerializedLambda) writeReplaceMethod.invoke(getter);
    }

    /**
     * 获取getter对应类的名称
     *
     * @param getter 方法引用
     * @return {@link String} Lambda实现类名
     */
    @SneakyThrows
    @SuppressWarnings("unchecked")
    public static <T, R> Class<T> getGetterClass(MethodReference<T, R> getter) {
        SerializedLambda serializedLambda = getSerializedLambda(getter);
        String className = serializedLambda.getImplClass().replace("/", ".");
        return (Class<T>) Class.forName(className);
    }

    /**
     * 获取getter对应的方法
     *
     * @param getter 方法引用
     * @return {@link Method}
     */
    @SneakyThrows
    public static <T, R> Method getGetterMethod(MethodReference<T, R> getter) {
        SerializedLambda serializedLambda = getSerializedLambda(getter);
        String implMethodName = serializedLambda.getImplMethodName();
        Class<?> getterClass = getGetterClass(getter);
        Method method = ReflectionUtils.findMethod(getterClass, implMethodName);
        if (method == null) {
            throw new IllegalStateException("Could not find method " + implMethodName);
        }
        return method;
    }

    /**
     * 获取getter对应的字段
     *
     * @param getter 方法引用
     * @return {@link Field}
     */
    @SneakyThrows
    public static <T, R> Field getGetterField(MethodReference<T, R> getter) {
        Class<T> getterClass = getGetterClass(getter);
        Method getterMethod = getGetterMethod(getter);
        PropertyDescriptor propertyDescriptor = BeanUtils.findPropertyForMethod(getterMethod, getterClass);
        if (propertyDescriptor == null) {
            throw new IllegalStateException("Could not find property for method " + getterMethod.getName());
        }
        String propertyName = propertyDescriptor.getName();
        Field field = ReflectionUtils.findField(getterClass, propertyName);
        if (field == null) {
            throw new IllegalStateException("Could not find field " + propertyName);
        }
        return field;
    }


}
