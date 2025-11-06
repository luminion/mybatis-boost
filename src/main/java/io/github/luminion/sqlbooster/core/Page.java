package io.github.luminion.sqlbooster.core;

import java.util.List;

/**
 * 统一分页接口,定义了分页对象的核心能力
 * @param <T> 记录的类型
 * @author luminion
 */
public interface Page<T> {
    /**
     * 获取记录列表
     *
     * @return 记录列表
     */
    List<T> getRecords();

    /**
     * 获取总记录数
     *
     * @return 总记录数
     */
    long getTotal();

    /**
     * 获取当前页码
     *
     * @return 当前页码
     */
    long getCurrent();

    /**
     * 获取每页数量
     *
     * @return 每页数量
     */
    long getSize();

    /**
     * 获取总页数
     *
     * @return 总页数
     */
    default long getPages() {
        if (getSize() == 0) {
            return 0L;
        }
        long pages = getTotal() / getSize();
        if (getTotal() % getSize() != 0) {
            pages++;
        }
        return pages;
    }
    
    /**
     * 转换记录列表的类型
     *
     * @param clazz 目标类型
     * @return 转换后的记录列表
     */
    <R> Page<R> convertRecords(Class<R> clazz);
}
