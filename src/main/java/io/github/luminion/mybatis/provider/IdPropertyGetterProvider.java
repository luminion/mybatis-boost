package io.github.luminion.mybatis.provider;

import io.github.luminion.mybatis.core.MethodRefence;

/**
 * 用于获取指定类的ID字段属性名
 *
 * @author luminion
 */
@FunctionalInterface
public interface IdPropertyGetterProvider {

    <T, R> MethodRefence<T, R> getIdPropertyGetter(Class<T> clazz);
}
