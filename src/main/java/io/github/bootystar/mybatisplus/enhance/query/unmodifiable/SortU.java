package io.github.bootystar.mybatisplus.enhance.query.unmodifiable;

import io.github.bootystar.mybatisplus.enhance.query.ISqlSort;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.ToString;

/**
 * @author bootystar
 */
@Getter
@AllArgsConstructor
@EqualsAndHashCode
@ToString
public class SortU implements ISqlSort {
    /**
     * 字段
     */
    protected String field;

    /**
     * 是否倒序(默认为false)
     */
    protected boolean desc;

}
