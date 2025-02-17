package io.github.bootystar.mybatisplus.enhance.query.general;

import io.github.bootystar.mybatisplus.enhance.query.SqlTree;
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
public class SqlTreeG implements SqlTree {

    /**
     * 查询条件列表
     */
    protected LinkedHashSet<SqlConditionG> conditions;

    /**
     * 子条件树
     */
    protected SqlTreeG child;

}
