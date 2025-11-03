package io.github.luminion.mybatis.provider;

import io.github.luminion.mybatis.core.MethodReference;

/**
 * 根据getter获取数据库字段名
 * @author luminion
 */
@FunctionalInterface
public interface GetterColumnProvider {

    <T, R> String getGetterColumnName(MethodReference<T, R> getter);
}
