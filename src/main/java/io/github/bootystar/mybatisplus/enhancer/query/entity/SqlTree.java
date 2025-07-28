package io.github.bootystar.mybatisplus.enhancer.query.entity;

import io.github.bootystar.mybatisplus.enhancer.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlCondition;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlTree;
import lombok.*;

import java.util.Collection;
import java.util.LinkedHashSet;

/**
 * 条件树
 *
 * @author bootystar
 */
@Getter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class SqlTree implements ISqlTree {
    /**
     * 条件列表
     */
    protected Collection<ISqlCondition> conditions;
    /**
     * 条件列表中条件的连接符
     */
    protected String symbol;
    /**
     * 子条件
     */
    protected SqlTree child;

    {
        this.conditions = new LinkedHashSet<>();
        symbol = SqlKeyword.AND.keyword;
    }

    public void setChild(SqlTree sqlTree) {
        if (child != null) {
            throw new IllegalArgumentException("child already exists");
        }
        this.child = sqlTree;
    }

    public SqlTree getLowestChild() {
        SqlTree current = this;
        while (current.getChild() != null) {
            current = current.getChild();
        }
        return current;
    }

}
