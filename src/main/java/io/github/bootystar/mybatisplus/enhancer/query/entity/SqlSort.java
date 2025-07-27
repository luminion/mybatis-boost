package io.github.bootystar.mybatisplus.enhancer.query.entity;

import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlSort;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 排序参数
 *
 * @author bootystar
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class SqlSort implements ISqlSort {

    protected String field;
    
    protected boolean desc;

    public static SqlSort of(ISqlSort sort) {
        return new SqlSort(sort.getField(), sort.isDesc());
    }

}
