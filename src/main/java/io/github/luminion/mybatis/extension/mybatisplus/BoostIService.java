package io.github.luminion.mybatis.extension.mybatisplus;

import io.github.luminion.mybatis.core.BoostEngine;

/**
 * 针对 Mybatis-Plus 的 IService 扩展接口.
 * <p>
 * 集成了 {@link BoostEngine} 的能力, 为 Mybatis-Plus 的 Service 层提供 VO 查询功能.
 *
 * @param <T> 实体类型
 * @param <V> VO 类型
 * @author luminion
 * @since 1.0.0
 */
public interface BoostIService<T, V> extends BoostEngine<T, V, String> {
}
