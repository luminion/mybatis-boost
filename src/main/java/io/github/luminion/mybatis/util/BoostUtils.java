package io.github.luminion.mybatis.util;

import io.github.luminion.mybatis.core.Booster;
import io.github.luminion.mybatis.core.SFunction;
import lombok.SneakyThrows;
import org.springframework.beans.BeanUtils;
import org.springframework.core.GenericTypeResolver;

import java.beans.PropertyDescriptor;
import java.lang.invoke.*;
import java.lang.reflect.Method;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * MyBatis Boost 反射工具类
 * <p>
 * 提供可扩展的、针对实体和VO的反射功能。通过注册Provider，可以插入自定义逻辑。
 *
 * @author luminion
 */
public abstract class BoostUtils {

    // ==================== Extension Point Functional Interfaces ====================

    @FunctionalInterface
    public interface TableNameProvider {
        Optional<String> getTableName(Class<?> entityClass);
    }

    @FunctionalInterface
    public interface IdPropertyNameProvider {
        Optional<String> getIdPropertyName(Class<?> entityClass);
    }

    @FunctionalInterface
    public interface IdPropertyGetterProvider {
        <T> Optional<SFunction<T, ?>> get(Class<T> entityClass);
    }

    @FunctionalInterface
    public interface GetterPropertyNameProvider {
        Optional<String> get(SFunction<?, ?> getter);
    }

    @FunctionalInterface
    public interface EntityClassProvider {
        <T, V> Optional<Class<T>> get(Booster<T, V> booster);
    }

    @FunctionalInterface
    public interface ViewObjectClassProvider {
        <T, V> Optional<Class<V>> get(Booster<T, V> booster);
    }

    @FunctionalInterface
    public interface FieldToColumnMapProvider {
        Map<String, String> getMap(Class<?> entityClass);
    }

    // ==================== Extension Point Lists ====================

    public static final List<TableNameProvider> TABLE_NAME_PROVIDERS = new CopyOnWriteArrayList<>();
    public static final List<IdPropertyNameProvider> ID_PROPERTY_NAME_PROVIDERS = new CopyOnWriteArrayList<>();
    public static final List<IdPropertyGetterProvider> ID_PROPERTY_GETTER_PROVIDERS = new CopyOnWriteArrayList<>();
    public static final List<GetterPropertyNameProvider> GETTER_PROPERTY_NAME_PROVIDERS = new CopyOnWriteArrayList<>();
    public static final List<EntityClassProvider> ENTITY_CLASS_PROVIDERS = new CopyOnWriteArrayList<>();
    public static final List<ViewObjectClassProvider> VIEW_OBJECT_CLASS_PROVIDERS = new CopyOnWriteArrayList<>();
    public static final List<FieldToColumnMapProvider> FIELD_TO_COLUMN_MAP_PROVIDERS = new CopyOnWriteArrayList<>();

    /**
     * 实体类字段到数据库列的映射缓存
     */
    private static final Map<Class<?>, Map<String, String>> JAVA_FIELD_TO_JDBC_COLUMN_CACHE_MAP = new ConcurrentHashMap<>();

    // ==================== Default Provider Registration ====================

    static {
        // 注册MyBatis-Plus作为默认的扩展实现 (如果存在于环境中)
        ID_PROPERTY_NAME_PROVIDERS.add(entityClass -> {
            if (MybatisPlusUtils.isMybatisPlusEnv()) {
                return Optional.of(MybatisPlusUtils.getIdPropertyName(entityClass));
            }
            return Optional.empty();
        });

        GETTER_PROPERTY_NAME_PROVIDERS.add(getter -> {
            if (MybatisPlusUtils.isMybatisPlusEnv()) {
                return Optional.of(MybatisPlusUtils.getGetterPropertyName(getter));
            }
            return Optional.empty();
        });

        FIELD_TO_COLUMN_MAP_PROVIDERS.add(entityClass -> {
            if (MybatisPlusUtils.isMybatisPlusEnv()) {
                return MybatisPlusUtils.javaFieldToJdbcColumnMap(entityClass);
            }
            return Collections.emptyMap();
        });
    }

    // ==================== Public Static Methods ====================

    public static String getEntityTableName(Class<?> clazz) {
        for (TableNameProvider provider : TABLE_NAME_PROVIDERS) {
            Optional<String> tableName = provider.getTableName(clazz);
            if (tableName.isPresent()) {
                return tableName.get();
            }
        }
        return clazz.getSimpleName(); // Default logic
    }

    public static String getIdPropertyName(Class<?> clazz) {
        for (IdPropertyNameProvider provider : ID_PROPERTY_NAME_PROVIDERS) {
            Optional<String> idName = provider.getIdPropertyName(clazz);
            if (idName.isPresent()) {
                return idName.get();
            }
        }
        // Default logic
        PropertyDescriptor[] descriptors = BeanUtils.getPropertyDescriptors(clazz);
        String potentialId = null;
        for (PropertyDescriptor descriptor : descriptors) {
            String propertyName = descriptor.getName();
            if ("class".equals(propertyName)) {
                continue;
            }
            if ("id".equalsIgnoreCase(propertyName)) {
                return propertyName;
            }
            if (potentialId == null && propertyName.toLowerCase().endsWith("id")) {
                potentialId = propertyName;
            }
        }
        if (potentialId != null) {
            return potentialId;
        }
        throw new IllegalStateException("No ID field found in " + clazz);
    }

    @SneakyThrows
    @SuppressWarnings("unchecked")
    public static <T, R> SFunction<T, R> getIdPropertyGetter(Class<T> clazz) {
        for (IdPropertyGetterProvider provider : ID_PROPERTY_GETTER_PROVIDERS) {
            Optional<SFunction<T, ?>> getter = provider.get(clazz);
            if (getter.isPresent()) {
                return (SFunction<T, R>) getter.get();
            }
        }
        // Default logic
        String idPropertyName = getIdPropertyName(clazz);
        PropertyDescriptor descriptor = BeanUtils.getPropertyDescriptor(clazz, idPropertyName);
        if (descriptor == null) {
            throw new IllegalStateException("Could not find property descriptor for ID: " + idPropertyName);
        }
        Method getter = descriptor.getReadMethod();
        if (getter == null) {
            throw new IllegalStateException("Could not find getter for ID property: " + idPropertyName);
        }
        MethodHandles.Lookup lookup = MethodHandles.lookup();
        MethodHandle getterHandle = lookup.unreflect(getter);
        MethodType samMethodType = MethodType.methodType(getter.getReturnType(), clazz);
        MethodType factoryType = MethodType.methodType(SFunction.class);
        MethodType implType = getterHandle.type();
        CallSite site = LambdaMetafactory.metafactory(lookup, "apply", factoryType, samMethodType, getterHandle, implType);
        return (SFunction<T, R>) site.getTarget().invokeExact();
    }

    @SneakyThrows
    public static String getGetterPropertyName(SFunction<?, ?> getter) {
        for (GetterPropertyNameProvider provider : GETTER_PROPERTY_NAME_PROVIDERS) {
            Optional<String> propertyName = provider.get(getter);
            if (propertyName.isPresent()) {
                return propertyName.get();
            }
        }
        // Default logic
        Method lambdaMethod = getter.getClass().getDeclaredMethod("writeReplace");
        lambdaMethod.setAccessible(Boolean.TRUE);
        SerializedLambda serializedLambda = (SerializedLambda) lambdaMethod.invoke(getter);
        return serializedLambda.getImplMethodName();
    }

    @SneakyThrows
    @SuppressWarnings({"unchecked", "ConstantConditions"})
    public static <T, V> Class<T> getEntityClass(Booster<T, V> booster) {
        for (EntityClassProvider provider : ENTITY_CLASS_PROVIDERS) {
            Optional<Class<T>> entityClass = provider.get(booster);
            if (entityClass.isPresent()) {
                return entityClass.get();
            }
        }
        return (Class<T>) GenericTypeResolver.resolveTypeArguments(booster.getClass(), Booster.class)[0];
    }

    @SneakyThrows
    @SuppressWarnings({"unchecked", "ConstantConditions"})
    public static <T, V> Class<V> getViewObjectClass(Booster<T, V> booster) {
        for (ViewObjectClassProvider provider : VIEW_OBJECT_CLASS_PROVIDERS) {
            Optional<Class<V>> voClass = provider.get(booster);
            if (voClass.isPresent()) {
                return voClass.get();
            }
        }
        return (Class<V>) GenericTypeResolver.resolveTypeArguments(booster.getClass(), Booster.class)[1];
    }

    public static Map<String, String> javaFieldToJdbcColumnMap(Class<?> entityClass) {
        Map<String, String> map = JAVA_FIELD_TO_JDBC_COLUMN_CACHE_MAP.get(entityClass);
        if (map != null) {
            return map;
        }
        String format = "a.%s";
        LinkedHashMap<String, String> result = new LinkedHashMap<>();
        for (FieldToColumnMapProvider provider : FIELD_TO_COLUMN_MAP_PROVIDERS) {
            Map<String, String> contributedMap = provider.getMap(entityClass);
            if (contributedMap != null) {
                contributedMap.forEach((key, value) -> {
                    result.putIfAbsent(key, String.format(format, value));
                });
            }
        }
        JAVA_FIELD_TO_JDBC_COLUMN_CACHE_MAP.put(entityClass, result);
        return result;
    }
}
