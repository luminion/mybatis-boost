package io.github.luminion.mybatis.provider;

/**
 * 用于获取指定类的表名
 * @author luminion
 */
@FunctionalInterface
public interface TableNameProvider {
    
    <T> String getTableName(Class<T> clazz);
}
