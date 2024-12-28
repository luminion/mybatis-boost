package io.github.bootystar.mybatisplus.enhance.query.general;

import io.github.bootystar.mybatisplus.enhance.query.ISqlEntity;
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
public class EntityG extends TreeG implements ISqlEntity {

    /**
     * 排序条件列表
     */
    protected LinkedHashSet<SortG> sorts;

}
