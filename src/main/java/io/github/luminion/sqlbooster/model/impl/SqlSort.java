package io.github.luminion.sqlbooster.model.impl;

import io.github.luminion.sqlbooster.model.api.ISqlSort;
import lombok.*;

/**
 * SQL 排序实体类.
 * <p>
 * 实现了 {@link ISqlSort} 接口, 用于表示 SQL 查询中的排序规则.
 *
 * @author luminion
 * @since 1.0.0
 */

@Getter
@ToString
@EqualsAndHashCode
@NoArgsConstructor
@AllArgsConstructor
public class SqlSort implements ISqlSort {

    /**
     * 排序字段.
     */
    protected String field;

    /**
     * 是否为降序排列.
     */
    protected boolean desc;

    /**
     * 从 {@link ISqlSort} 实例创建 {@link SqlSort} 实例.
     *
     * @param sort SQL 排序接口实例
     * @return {@link SqlSort} SQL 排序实体实例
     * @since 1.0.0
     */
    public static SqlSort of(ISqlSort sort) {
        return new SqlSort(sort.getField(), sort.isDesc());
    }

}