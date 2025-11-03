package io.github.luminion.mybatis.provider;

/**
 * 用于获取指定类 的ID字段数据库列名
 * @author luminion
 */
@FunctionalInterface
public interface IdColumProvider {
    
    <T> String getIdColumnName(Class<T> clazz);
}
