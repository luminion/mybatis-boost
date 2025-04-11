package io.github.bootystar.mybatisplus.enhancer.query.unmodifiable;

import io.github.bootystar.mybatisplus.enhancer.query.SqlSort;
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
public class SqlSortU implements SqlSort {

    protected String field;

    protected boolean desc;

}
