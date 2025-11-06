package io.github.luminion.sqlbooster.model.helper;

import io.github.luminion.sqlbooster.core.BoosterCore;
import io.github.luminion.sqlbooster.core.P;

import java.util.List;
import java.util.Optional;

/**
 * 具备扩展查询功能的 SQL 构建助手.
 * <p>
 * 封装了 {@link BoosterCore} 和 {@link ISqlHelper}, 提供了方便的链式调用查询方法.
 *
 * @param <T> 实体类型
 * @param <V> VO 类型
 * @author luminion
 * @since 1.0.0
 */
public class BoostSqlHelper<T, V> {
    private final BoosterCore<T, V> boosterCore;
    private final ISqlHelper<T> sqlHelper;

    /**
     * 构造一个新的 {@link BoostSqlHelper} 实例.
     *
     * @param boosterCore {@link BoosterCore} 实例
     * @since 1.0.0
     */
    public BoostSqlHelper(BoosterCore<T, V> boosterCore) {
        this.sqlHelper = SqlHelper.of(boosterCore);
        this.boosterCore = boosterCore;
    }

    /**
     * 查询并返回结果列表中的第一个 VO 对象.
     *
     * @return VO 对象, 如果不存在则返回 null
     * @since 1.0.0
     */
    public V first() {
        return boosterCore.voFirst(this.sqlHelper);
    }

    /**
     * 查询并返回结果列表中的第一个 VO 对象, 并转换为指定类型.
     *
     * @param voType 目标 VO 类型
     * @return 转换后的 VO 对象, 如果不存在则返回 null
     * @since 1.0.0
     */
    public V first(Class<V> voType) {
        return boosterCore.voFirst(this.sqlHelper, voType);
    }

    /**
     * 查询并返回一个包含第一个 VO 对象的 {@link Optional}.
     *
     * @return 包含 VO 对象的 Optional, 如果不存在则为空
     * @since 1.0.0
     */
    public Optional<V> firstOpt() {
        return boosterCore.voFirstOpt(this.sqlHelper);
    }

    /**
     * 查询并返回唯一的 VO 对象.
     *
     * @return VO 对象, 如果不存在则返回 null
     * @since 1.0.0
     */
    public V unique() {
        return boosterCore.voUnique(this.sqlHelper);
    }

    /**
     * 查询并返回唯一的 VO 对象, 并转换为指定类型.
     *
     * @param voType 目标 VO 类型
     * @return 转换后的 VO 对象, 如果不存在则返回 null
     * @since 1.0.0
     */
    public V unique(Class<V> voType) {
        return boosterCore.voUnique(this.sqlHelper, voType);
    }

    /**
     * 查询并返回一个包含唯一 VO 对象的 {@link Optional}.
     *
     * @return 包含 VO 对象的 Optional, 如果不存在则为空
     * @since 1.0.0
     */
    public Optional<V> uniqueOpt() {
        return boosterCore.voUniqueOpt(this.sqlHelper);
    }

    /**
     * 查询并返回 VO 对象列表.
     *
     * @return VO 对象列表
     * @since 1.0.0
     */
    public List<V> list() {
        return boosterCore.voList(this.sqlHelper);
    }

    /**
     * 查询并返回指定类型的 VO 对象列表.
     *
     * @param voType 目标 VO 类型
     * @return 转换后的 VO 对象列表
     * @since 1.0.0
     */
    public <R> List<R> list(Class<R> voType) {
        return boosterCore.voList(this.sqlHelper, voType);
    }

    /**
     * 分页查询 VO 对象.
     *
     * @param pageNum  当前页码
     * @param pageSize 每页大小
     * @return 分页结果对象
     * @since 1.0.0
     */
    public P<V> page(int pageNum, int pageSize) {
        return boosterCore.voPage(this.sqlHelper, pageNum, pageSize);
    }

    /**
     * 分页查询 VO 对象.
     *
     * @param pageNum  当前页码
     * @param pageSize 每页大小
     * @return 分页结果对象
     * @since 1.0.0
     */
    public P<V> page(long pageNum, long pageSize) {
        return boosterCore.voPage(this.sqlHelper, pageNum, pageSize);
    }

    /**
     * 分页查询 VO 对象, 并转换为指定类型.
     *
     * @param pageNum  当前页码
     * @param pageSize 每页大小
     * @param voType   目标 VO 类型
     * @return 分页结果对象
     * @since 1.0.0
     */
    public <R> P<R> page(int pageNum, int pageSize, Class<R> voType) {
        return boosterCore.voPage(this.sqlHelper, pageNum, pageSize, voType);
    }

    /**
     * 分页查询 VO 对象, 并转换为指定类型.
     *
     * @param pageNum  当前页码
     * @param pageSize 每页大小
     * @param voType   目标 VO 类型
     * @return 分页结果对象
     * @since 1.0.0
     */
    public <R> P<R> page(long pageNum, long pageSize, Class<R> voType) {
        return boosterCore.voPage(this.sqlHelper, pageNum, pageSize, voType);
    }


}