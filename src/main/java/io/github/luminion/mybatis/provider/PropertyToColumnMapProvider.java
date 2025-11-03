package io.github.luminion.mybatis.provider;

import java.util.Map;

/**
 * 获取实体类字段到数据库列的映射
 * @author luminion
 */
@FunctionalInterface
public interface PropertyToColumnMapProvider {
    
    <T> Map<String, String> getPropertyToColumnMap(Class<T> clazz);
}
