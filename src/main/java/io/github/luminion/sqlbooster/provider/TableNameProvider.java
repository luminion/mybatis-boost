package io.github.luminion.sqlbooster.provider;

/**
 * 表名提供者接口.
 * <p>
 * 用于根据实体类获取其对应的数据库表名.
 *
 * @author luminion
 * @since 1.0.0
 */
@FunctionalInterface
public interface TableNameProvider {

    /**
     * 根据实体类获取表名.
     *
     * @param clazz 实体类
     * @param <T>   实体类型
     * @return 数据库表名
     * @since 1.0.0
     */
    <T> String getTableName(Class<T> clazz);
}
