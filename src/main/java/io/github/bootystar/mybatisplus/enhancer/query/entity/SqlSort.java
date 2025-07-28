package io.github.bootystar.mybatisplus.enhancer.query.entity;

import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlSort;
import lombok.*;

/**
 * 排序参数
 *
 * @author bootystar
 */

@Getter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class SqlSort implements ISqlSort {

    protected String field;

    protected boolean desc;

    public static SqlSort of(ISqlSort sort) {
        return new SqlSort(sort.getField(), sort.isDesc());
    }

}
