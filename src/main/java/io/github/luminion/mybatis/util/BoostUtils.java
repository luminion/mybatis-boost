package io.github.luminion.mybatis.util;

import io.github.luminion.mybatis.core.Booster;
import io.github.luminion.mybatis.core.MethodReference;
import io.github.luminion.mybatis.provider.BoostProvider;
import lombok.extern.slf4j.Slf4j;
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
@Slf4j
public abstract class BoostUtils {
    /**
     * 实体类字段到数据库列的映射缓存
     */
    private static final Map<Class<?>, Map<String, String>> ENTITY_PROPERTY_TO_COLUMN_MAP = new ConcurrentHashMap<>();
    private static final TreeSet<BoostProvider> PROVIDERS = new TreeSet<>();

    public static List<BoostProvider> checkoutProviders() {
        return new ArrayList<>(PROVIDERS);
    }

    public static boolean registerProvider(BoostProvider provider) {
        return PROVIDERS.add(provider);
    }

    public static boolean removeProvider(BoostProvider provider) {
        return PROVIDERS.remove(provider);
    }


    /**
     * 下划线转驼峰
     *
     * @param str str
     * @return 驼峰字符串
     */
    public static String underscoreToCamelCase(String str) {
        StringBuilder sb = new StringBuilder();
        boolean upperCase = false;
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            if (c == '_') {
                upperCase = true;
            } else if (upperCase) {
                sb.append(Character.toUpperCase(c));
                upperCase = false;
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    /**
     * 驼峰转下滑线
     *
     * @param str str
     * @return 下划线字符串
     */
    public static String camelCaseToUnderscore(String str) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            if (Character.isUpperCase(c)) {
                sb.append('_').append(Character.toLowerCase(c));
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }
    

    @SuppressWarnings({"unchecked", "ConstantConditions"})
    public static <T, V> Class<T> getEntityClass(Booster<T, V> booster) {
        return (Class<T>) GenericTypeResolver.resolveTypeArguments(booster.getClass(), Booster.class)[0];
    }

    @SuppressWarnings({"unchecked", "ConstantConditions"})
    public static <T, V> Class<V> getViewObjectClass(Booster<T, V> booster) {
        return (Class<V>) GenericTypeResolver.resolveTypeArguments(booster.getClass(), Booster.class)[1];
    }

    public static String getTableName(Class<?> entityClass) {
        for (BoostProvider provider : PROVIDERS) {
            String tableName = provider.getTableName(entityClass);
            if (tableName != null) {
                return tableName;
            }
        }
        throw new IllegalStateException("No table name found in " + PROVIDERS.size() + " providers, class: " + entityClass.getName());
    }

    public static String getIdPropertyName(Class<?> entityClass) {
        for (BoostProvider provider : PROVIDERS) {
            String idPropertyName = provider.getIdPropertyName(entityClass);
            if (idPropertyName != null) {
                return idPropertyName;
            }
        }
        throw new IllegalStateException("No IdProperty found in " + PROVIDERS.size() + " providers, class: " + entityClass.getName());
    }

    public static <T, R> String getGetterPropertyName(MethodReference<T, R> getter) {
        for (BoostProvider provider : PROVIDERS) {
            String propertyName = provider.getGetterPropertyName(getter);
            if (propertyName != null) {
                return propertyName;
            }
        }
        throw new IllegalStateException("No property name found in " + PROVIDERS.size() + " providers, getter: " + getter);
    }

    public static Map<String, String> getPropertyToColumnAliasMap(Class<?> entityClass) {
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
        log.warn("No property to column alias map found in {} providers, class: {}", PROVIDERS.size(), entityClass.getName());
        ENTITY_PROPERTY_TO_COLUMN_MAP.put(entityClass, result);
        return result;
    }
}
