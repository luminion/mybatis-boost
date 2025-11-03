package io.github.luminion.mybatis.provider;

import io.github.luminion.mybatis.core.MethodRefence;

/**
 * 根据getter获取属性名
 * @author luminion
 */
@FunctionalInterface
public interface GetterPropertyProvider {
    
    <T, R> String getGetterPropertyName(MethodRefence<T, R> getter);
}
