package io.github.bootystar.mybatisplus.enhance.query.general;

import io.github.bootystar.mybatisplus.enhance.query.SqlEntity;
import lombok.Getter;
import lombok.Setter;

import java.util.LinkedHashSet;

/**
 * 条件树
 *
 * @author bootystar
 */
@Setter
@Getter
public class SqlEntityG extends SqlTreeG implements SqlEntity {

    /**
     * 排序条件列表
     */
    protected LinkedHashSet<SqlSortG> sorts;

}
