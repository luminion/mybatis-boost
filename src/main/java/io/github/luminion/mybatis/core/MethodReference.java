package io.github.luminion.mybatis.core;

import java.io.Serializable;
import java.util.function.Function;

/**
 * 代表某个方法引用
 *
 * @author luminion
 */
@FunctionalInterface
public interface MethodReference<T, R> extends Function<T, R>, Serializable {

}
