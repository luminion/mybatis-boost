package io.github.luminion.mybatis.entity;

import lombok.Data;

import java.util.List;

/**
 * 通用分页对象.
 *
 * @param <T> 分页记录的类型
 * @author luminion
 * @since 1.0.0
 */
@Data
public class Page<T> {
    /**
     * 当前页码.
     */
    private Long pageNum;
    /**
     * 每页数量.
     */
    private Long pageSize;
    /**
     * 总记录数.
     */
    private Long total;
    /**
     * 总页数.
     */
    private Long pages;
    /**
     * 当前页的记录列表.
     */
    private List<T> records;
}
