package io.github.bootystar.mybatisplus.enhancer.query.unmodifiable;

import io.github.bootystar.mybatisplus.enhancer.query.SqlEntity;
import lombok.Getter;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;

/**
 * @author bootystar
 */
@Getter
public class SqlEntityU extends SqlTreeU implements SqlEntity {

    protected Collection<SqlSortU> sorts;

    public SqlEntityU(Collection<SqlConditionU> conditions, SqlTreeU child, Collection<SqlSortU> sorts) {
        super(conditions, child);
        this.sorts = sorts == null ? null : Collections.unmodifiableCollection(new LinkedHashSet<>(sorts));
    }

}
