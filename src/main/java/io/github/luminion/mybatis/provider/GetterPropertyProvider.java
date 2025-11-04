package io.github.luminion.mybatis.provider;

import io.github.luminion.mybatis.core.MethodReference;

/**
 * Getter 属性提供者接口.
 * <p>
 * 用于从 getter 方法引用中解析出对应的属性名.
 *
 * @author luminion
 * @since 1.0.0
 */
@FunctionalInterface
public interface GetterPropertyProvider {

    /**
     * 从 getter 方法引用中获取属性名.
     *
     * @param getter getter 方法引用
     * @param <T>    实体类型
     * @param <R>    属性类型
     * @return 属性名
     * @since 1.0.0
     */
    <T, R> String getGetterPropertyName(MethodReference<T, R> getter);
}
