package io.github.bootystar.mybatisplus.enhancer.sql.entity;

import io.github.bootystar.mybatisplus.enhancer.sql.base.SqlSort;
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
public class SqlSortG implements SqlSort {

    /**
     * 属性名
     */
    protected String field;

    /**
     * 是否倒序(非必填,默认false)
     */
    protected boolean desc;

    public static SqlSortG of(SqlSort sort) {
        if (sort instanceof SqlSortG) return (SqlSortG) sort;
        return new SqlSortG(sort.getField(), sort.isDesc());
    }

}
