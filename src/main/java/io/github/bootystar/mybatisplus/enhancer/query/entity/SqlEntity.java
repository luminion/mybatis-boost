package io.github.bootystar.mybatisplus.enhancer.query.entity;

import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlEntity;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlSort;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlTree;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.Collection;
import java.util.LinkedHashSet;

/**
 * 可排序的条件树
 *
 * @author bootystar
 */
@Getter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class SqlEntity extends SqlTree implements ISqlEntity {

    /**
     * 排序字段列表
     */
    protected Collection<ISqlSort> sorts;

    {
        this.sorts = new LinkedHashSet<>();
    }

    @Override
    protected SqlTree addChild(ISqlTree sqlTree) {
        if (sqlTree==null){
            return this;
        }
        if (sqlTree instanceof SqlEntity){
            SqlEntity sqlEntity = (SqlEntity) sqlTree;
            this.sorts.addAll(sqlEntity.getSorts());
        }
        return super.addChild(sqlTree);
    }
}
