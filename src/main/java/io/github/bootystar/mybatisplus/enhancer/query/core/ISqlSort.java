package io.github.bootystar.mybatisplus.enhancer.query.core;

/**
 * 排序
 * @author bootystar
 */
public interface ISqlSort {

    /**
     * 属性名
     *
     * @return {@link String }
     */
    String getField();

    /**
     * 是否倒序
     *
     * @return boolean
     */
    boolean isDesc();

}
