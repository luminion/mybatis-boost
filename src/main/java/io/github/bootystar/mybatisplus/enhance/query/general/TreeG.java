package io.github.bootystar.mybatisplus.enhance.query.general;

import io.github.bootystar.mybatisplus.enhance.query.ISqlTree;
import lombok.Getter;
import lombok.Setter;

import java.util.*;

/**
 * 条件树
 *
 * @author bootystar
 */
@Setter
@Getter
public class TreeG implements ISqlTree {

    /**
     * 条件
     */
    protected LinkedHashSet<ConditionG> conditions;

    /**
     * 子条件
     * (满足父条件后的值才会筛选子条件)
     */
    protected TreeG child;

    /**
     * 排序条件列表
     */
    protected LinkedHashSet<SortG> sorts;

}
