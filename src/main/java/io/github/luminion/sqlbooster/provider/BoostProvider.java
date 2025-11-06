package io.github.luminion.sqlbooster.provider;

import org.springframework.core.Ordered;

/**
 * Mybatis-Boost 的核心提供者接口.
 * <p>
 * 聚合了所有特定功能的 Provider 接口, 并定义了排序规则.
 *
 * @author luminion
 * @since 1.0.0
 */
public interface BoostProvider extends Comparable<BoostProvider>, Ordered,
        TableNameProvider, IdPropertyProvider, GetterPropertyProvider, PropertyToColumnAliasMapProvider {

    /**
     * 比较两个 {@link BoostProvider} 的顺序.
     * <p>
     * 默认实现是根据 {@link #getOrder()} 的值进行降序排列.
     *
     * @param o 要比较的对象
     * @return 比较结果
     * @since 1.0.0
     */
    @Override
    default int compareTo(BoostProvider o) {
        return o.getOrder() - this.getOrder();
    }
}
