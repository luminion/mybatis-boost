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
 * 反射工具类.
 * <p>
 * 提供通用的反射操作, 包括实例创建、字段缓存、属性复制、方法引用解析等.
 *
 * @author luminion
 * @since 1.0.0
 */
public abstract class ReflectUtils {

    /**
     * 类字段映射缓存, 用于提升性能.
     */
    private static final Map<Class<?>, Map<String, Field>> FIELD_MAP_CACHE = new ConcurrentHashMap<>();


    /**
     * 判断一个类是否为 Java 核心库中的类.
     *
     * @param clazz 待检查的类
     * @return 如果是核心类则返回 true, 否则返回 false
     * @since 1.0.0
     */
    public static boolean isJavaCoreClass(Class<?> clazz) {
        if (clazz == null) {
            return false;
        }
        return clazz.getClassLoader() == null;
    }

    /**
     * 使用 Spring 的 {@link BeanUtils} 创建一个新的实例.
     *
     * @param clazz 待实例化的类
     * @param <T>   实例类型
     * @return 新创建的实例
     * @since 1.0.0
     */
    public static <T> T newInstance(Class<T> clazz) {
        if (clazz == null) {
            throw new IllegalArgumentException("clazz must not be null");
        }
        return BeanUtils.instantiateClass(clazz);
    }

    /**
     * 获取并缓存指定类的所有字段.
     *
     * @param clazz 待分析的类
     * @return 字段名到 {@link Field} 对象的映射
     * @since 1.0.0
     */
    public static Map<String, Field> fieldMap(Class<?> clazz) {
        if (clazz == null) {
            throw new IllegalArgumentException("clazz must not be null");
        }
        if (isJavaCoreClass(clazz)) {
            throw new IllegalArgumentException("clazz must not be java class");
        }
        return FIELD_MAP_CACHE.computeIfAbsent(clazz, c -> {
            Map<String, Field> map = new HashMap<>();
            ReflectionUtils.doWithFields(c, 
                    field -> map.putIfAbsent(field.getName(), field),
                    ReflectionUtils.COPYABLE_FIELDS);
            return map;
        });
    }


    /**
     * 使用 {@link BeanUtils#copyProperties(Object, Object)} 复制对象的属性.
     *
     * @param source 源对象
     * @param target 目标对象
     * @param <T>    目标对象类型
     * @return 复制完属性的目标对象
     * @since 1.0.0
     */
    public static <T> T copyFieldProperties(Object source, T target) {
        if (source == null || target == null) return target;
        BeanUtils.copyProperties(source, target);
        return target;
    }


    /**
     * 将一个对象转换为 {@link Map}.
     *
     * @param source 源对象
     * @return 转换后的 Map
     * @since 1.0.0
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
     * 将一个对象转换为指定类型的新对象.
     *
     * @param source 源对象
     * @param clazz  目标类
     * @param <T>    目标类型
     * @return 转换后的新对象
     * @since 1.0.0
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
     * 解析一个类实现的泛型接口或继承的泛型父类的实际类型参数.
     *
     * @param clazz      待解析的类
     * @param superClass 泛型接口或父类
     * @return 实际类型参数的 {@link Class} 数组
     * @since 1.0.0
     */
    public static Class<?>[] resolveTypeArguments(Class<?> clazz, Class<?> superClass) {
        return GenericTypeResolver.resolveTypeArguments(clazz, superClass);
    }


    /**
     * 从可序列化的方法引用中提取 {@link SerializedLambda} 信息.
     *
     * @param getter 方法引用
     * @return {@link SerializedLambda} 实例
     * @since 1.0.0
     */
    @SneakyThrows
    private static <T, R> SerializedLambda getSerializedLambda(MethodReference<T, R> getter) {
        Method writeReplaceMethod = getter.getClass().getDeclaredMethod("writeReplace");
        writeReplaceMethod.setAccessible(true);
        return (SerializedLambda) writeReplaceMethod.invoke(getter);
    }

    /**
     * 从方法引用中获取其声明所在的类的 {@link Class} 对象.
     *
     * @param getter 方法引用
     * @return 声明该方法的类的 {@link Class} 对象
     * @since 1.0.0
     */
    @SneakyThrows
    @SuppressWarnings("unchecked")
    public static <T, R> Class<T> getGetterClass(MethodReference<T, R> getter) {
        SerializedLambda serializedLambda = getSerializedLambda(getter);
        String className = serializedLambda.getImplClass().replace("/", ".");
        return (Class<T>) Class.forName(className);
    }

    /**
     * 从方法引用中获取其对应的 {@link Method} 对象.
     *
     * @param getter 方法引用
     * @return {@link Method} 对象
     * @since 1.0.0
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
     * 从 getter 方法引用中获取其对应的属性 {@link Field} 对象.
     *
     * @param getter 方法引用
     * @return {@link Field} 对象
     * @since 1.0.0
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
