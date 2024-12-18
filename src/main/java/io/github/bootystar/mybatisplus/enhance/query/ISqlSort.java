package io.github.bootystar.mybatisplus.enhance.query;

/**
 * SQL排序
 * @author bootystar
 */
public interface ISqlSort {

    /**
     * 字段/属性名
     *
     * @return {@link String }
     */
    String getField();

    /**
     * 是否为倒序
     *
     * @return boolean
     */
    boolean isDesc();

}
