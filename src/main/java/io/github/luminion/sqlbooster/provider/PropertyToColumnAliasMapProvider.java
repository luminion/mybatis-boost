package io.github.luminion.sqlbooster.provider;

import java.util.Map;

/**
 * 属性到列映射提供者接口.
 * <p>
 * 用于获取实体类的属性名到数据库列名的映射关系.
 *
 * @author luminion
 * @since 1.0.0
 */
@FunctionalInterface
public interface PropertyToColumnAliasMapProvider {

    /**
     * 获取实体类的属性到数据库列名(别名)的映射.
     *
     * @param clazz 实体类
     * @param <T>   实体类型
     * @return 属性名到列名(别名)的映射 Map
     * @since 1.0.0
     */
    <T> Map<String, String> getPropertyToColumnAliasMap(Class<T> clazz);
}
