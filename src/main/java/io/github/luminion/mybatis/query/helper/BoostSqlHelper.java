package io.github.luminion.mybatis.query.helper;

import io.github.luminion.mybatis.core.BoostCore;

import java.util.List;
import java.util.Optional;

/**
 * 具备扩展查询功能的SqlHelper
 * 提供链式调用及查询
 *
 * @param <T> 实体类型
 * @param <V> VO类型
 * @author luminion
 */
public class BoostSqlHelper<T, V, P> {
    private final BoostCore<T, V, P> boostCore;
    private final ISqlHelper<T> sqlHelper;

    /**
     * 构造方法
     *
     * @param sqlHelper SQL助手
     * @param boostCore 扩展查询接口
     */
    public BoostSqlHelper(BoostCore<T, V, P> boostCore) {
        this.sqlHelper = SqlHelper.of(boostCore);
        this.boostCore = boostCore;
    }

    /**
     * 查询结果中的第一个VO对象
     *
     * @return VO对象
     */
    public V first() {
        return boostCore.voFirst(this.sqlHelper);
    }
    
    public V first(Class<V> voType) {
        return boostCore.voFirst(this.sqlHelper, voType);
    }
    
    public Optional<V> firstOpt() {
        return boostCore.voFirstOpt(this.sqlHelper);
    }

    /**
     * 查询唯一VO对象
     *
     * @return VO对象
     */
    public V unique() {
        return boostCore.voUnique(this.sqlHelper);
    }
    
    public V unique(Class<V> voType) {
        return boostCore.voUnique(this.sqlHelper, voType);
    }
    
    public Optional<V> uniqueOpt() {
        return boostCore.voUniqueOpt(this.sqlHelper);
    }

    /**
     * 查询VO对象列表
     *
     * @return VO对象列表
     */
    public List<V> list() {
        return boostCore.voList(this.sqlHelper);
    }
    
    public List<V> list(Class<V> voType) {
        return boostCore.voList(this.sqlHelper, voType);
    }
    
    /**
     * 分页查询VO对象
     *
     * @param pageNum  当前页码
     * @param pageSize 每页大小
     * @return 分页结果
     */
    public P page(int pageNum, int pageSize) {
        return boostCore.voPage(this.sqlHelper, pageNum, pageSize);
    }

    public P page(int pageNum, int pageSize, Class<V> voType) {
        return boostCore.voPage(this.sqlHelper, pageNum, pageSize, voType);
    }

    public P page(long pageNum, long pageSize) {
        return boostCore.voPage(this.sqlHelper, pageNum, pageSize);
    }
    
    public P page(long pageNum, long pageSize, Class<V> voType) {
        return boostCore.voPage(this.sqlHelper, pageNum, pageSize, voType);
    }


}