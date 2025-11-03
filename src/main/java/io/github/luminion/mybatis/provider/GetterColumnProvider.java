package io.github.luminion.mybatis.provider;

import io.github.luminion.mybatis.core.MethodRefence;

/**
 * 根据getter获取数据库字段名
 * @author luminion
 */
@FunctionalInterface
public interface GetterColumnProvider {

    <T, R> String getGetterColumnName(MethodRefence<T, R> getter);
}
