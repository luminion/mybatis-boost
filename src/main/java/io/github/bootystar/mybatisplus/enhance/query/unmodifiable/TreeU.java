package io.github.bootystar.mybatisplus.enhance.query.unmodifiable;

import io.github.bootystar.mybatisplus.enhance.query.ISqlTree;
import lombok.Getter;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;

/**
 * @author bootystar
 */
@Getter
public class TreeU implements ISqlTree {

    protected Collection<ConditionU> conditions;

    protected TreeU child;

    protected Collection<SortU> sorts;

    public TreeU(Collection<ConditionU> conditions, Collection<SortU> sorts, TreeU child) {
        this.conditions = conditions == null ? null : Collections.unmodifiableCollection(new LinkedHashSet<>(conditions));
        this.sorts = sorts == null ? null : Collections.unmodifiableCollection(new LinkedHashSet<>(sorts));
        this.child = child;
    }
}
