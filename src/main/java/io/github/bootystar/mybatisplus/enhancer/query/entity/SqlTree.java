package io.github.bootystar.mybatisplus.enhancer.query.entity;

import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlCondition;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlTree;
import lombok.*;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
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
    }


}
