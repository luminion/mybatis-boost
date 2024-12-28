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
     * 子条件
     * (满足父条件后的值才会筛选子条件)
     */
    protected SqlTreeG child;

}
