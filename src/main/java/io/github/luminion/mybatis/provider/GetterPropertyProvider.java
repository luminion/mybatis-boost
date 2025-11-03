package io.github.luminion.mybatis.provider;

import io.github.luminion.mybatis.core.MethodReference;

/**
 * 根据getter获取属性名
 * @author luminion
 */
@FunctionalInterface
public interface GetterPropertyProvider {
    
    <T, R> String getGetterPropertyName(MethodReference<T, R> getter);
}
