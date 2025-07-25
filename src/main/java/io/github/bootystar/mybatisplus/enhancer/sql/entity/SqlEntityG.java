package io.github.bootystar.mybatisplus.enhancer.sql.entity;

import io.github.bootystar.mybatisplus.enhancer.sql.base.SqlEntity;
import lombok.Getter;
import lombok.Setter;

import java.util.LinkedHashSet;
import java.util.Map;

/**
 * 条件树
 *
 * @author bootystar
 */
@Setter
@Getter
public class SqlEntityG extends SqlTreeG implements SqlEntity {

    /**
     * 排序条件列表
     */
    protected LinkedHashSet<SqlSortG> sorts;

    /**
     * 未自动映射的条件列表
     */
    protected Map<String,Object> map;

}
