package io.github.bootystar.mybatisplus.enhance.query.general;

import io.github.bootystar.mybatisplus.enhance.query.ISqlSort;
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
public class SortG implements ISqlSort {

    /**
     * 字段/属性名
     */
    protected String field;

    /**
     * 是否为倒序
     */
    protected boolean desc;

    public static SortG of(ISqlSort sort) {
        if (sort instanceof SortG) return (SortG) sort;
        return new SortG(sort.getField(), sort.isDesc());
    }

}
