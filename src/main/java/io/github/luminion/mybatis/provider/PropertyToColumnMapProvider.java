package io.github.luminion.mybatis.provider;

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
public interface PropertyToColumnMapProvider {

    /**
     * 获取实体类的属性到列的映射.
     *
     * @param clazz 实体类
     * @param <T>   实体类型
     * @return 属性名到列名的映射 Map
     * @since 1.0.0
     */
    <T> Map<String, String> getPropertyToColumnMap(Class<T> clazz);
}
