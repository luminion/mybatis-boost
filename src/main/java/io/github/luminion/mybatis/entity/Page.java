package io.github.luminion.mybatis.entity;

import lombok.Data;

import java.util.List;

/**
 * @author luminion
 */
@Data
public class Page<T> {
    private Long pageNum;
    private Long pageSize;
    private Long total;
    private Long pages;
    private List<T> records;
}
