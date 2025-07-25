package io.github.bootystar.mybatisplus.enhancer.sql.entity;

import io.github.bootystar.mybatisplus.enhancer.sql.base.SqlConditionTree;
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
public class SqlTreeG implements SqlConditionTree {

    /**
     * 查询条件列表
     */
    protected LinkedHashSet<SqlConditionG> conditions;

    /**
     * 子条件树
     */
    protected SqlTreeG child;

}
