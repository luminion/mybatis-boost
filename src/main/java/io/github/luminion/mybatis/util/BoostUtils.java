package io.github.luminion.mybatis.util;

import io.github.luminion.mybatis.core.Booster;
import io.github.luminion.mybatis.core.MethodRefence;
import io.github.luminion.mybatis.provider.BoostProvider;
import lombok.SneakyThrows;
import org.springframework.core.GenericTypeResolver;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * MyBatis Boost 反射工具类
 * <p>
 * 提供可扩展的、针对实体和VO的反射功能。通过注册Provider，可以插入自定义逻辑。
 *
 * @author luminion
 */
public abstract class BoostUtils {
    /**
     * 实体类字段到数据库列的映射缓存
     */
    private static final Map<Class<?>, Map<String, String>> ENTITY_PROPERTY_TO_COLUMN_MAP = new ConcurrentHashMap<>();
    private static final List<BoostProvider> PROVIDERS = new ArrayList<>();

    public static boolean registerProvider(BoostProvider provider) {
        return PROVIDERS.add(provider);
    }

    public static boolean removeProvider(BoostProvider provider) {
        return PROVIDERS.remove(provider);
    }

    public static void sortProvider(Comparator<BoostProvider> comparator) {
        PROVIDERS.sort(comparator);
    }

    @SneakyThrows
    @SuppressWarnings({"unchecked", "ConstantConditions"})
    public static <T, V> Class<T> getEntityClass(Booster<T, V> booster) {
        return (Class<T>) GenericTypeResolver.resolveTypeArguments(booster.getClass(), Booster.class)[0];
    }

    @SneakyThrows
    @SuppressWarnings({"unchecked", "ConstantConditions"})
    public static <T, V> Class<V> getViewObjectClass(Booster<T, V> booster) {
        return (Class<V>) GenericTypeResolver.resolveTypeArguments(booster.getClass(), Booster.class)[1];
    }

    public static String getEntityTableName(Class<?> clazz) {
        for (BoostProvider provider : PROVIDERS) {
            String tableName = provider.getTableName(clazz);
            if (tableName != null) {
                return tableName;
            }
        }
        throw new IllegalStateException("No table name found in " + clazz.getName());
    }

    public static String getEntityIdPropertyName(Class<?> clazz) {
        for (BoostProvider provider : PROVIDERS) {
            String idPropertyName = provider.getIdPropertyName(clazz);
            if (idPropertyName != null) {
                return idPropertyName;
            }
        }
        throw new IllegalStateException("No IdProperty found in " + clazz.getName());
    }

    @SneakyThrows
    public static <T, R> MethodRefence<T, R> getEntityIdPropertyGetter(Class<T> clazz) {
        for (BoostProvider provider : PROVIDERS) {
            MethodRefence<T, R> idPropertyGetter = provider.getIdPropertyGetter(clazz);
            if (idPropertyGetter != null) {
                return idPropertyGetter;
            }
        }
        throw new IllegalStateException("No IdPropertyGetter found in " + clazz.getName());
    }

    @SneakyThrows
    public static <T, R> String getEntityGetterPropertyName(MethodRefence<T, R> getter) {
        for (BoostProvider provider : PROVIDERS) {
            String propertyName = provider.getGetterPropertyName(getter);
            if (propertyName != null) {
                return propertyName;
            }
        }
        throw new IllegalStateException("No property name found in " + getter);
    }

    public static Map<String, String> getEntityPropertyToColumnMapForSqlEntity(Class<?> entityClass) {
        Map<String, String> map = ENTITY_PROPERTY_TO_COLUMN_MAP.get(entityClass);
        if (map != null) {
            return map;
        }
        String format = "a.%s";
        LinkedHashMap<String, String> result = new LinkedHashMap<>();
        for (BoostProvider provider : PROVIDERS) {
            Map<String, String> contributedMap = provider.getPropertyToColumnMap(entityClass);
            if (contributedMap != null) {
                contributedMap.forEach((key, value) -> {
                    result.putIfAbsent(key, String.format(format, value));
                });
            }
        }
        ENTITY_PROPERTY_TO_COLUMN_MAP.put(entityClass, result);
        return result;
    }
}
