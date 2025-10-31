package io.github.luminion.mybatisplus.enhancer.core;


import io.github.luminion.mybatisplus.enhancer.query.core.ISqlEntity;
import org.apache.ibatis.exceptions.TooManyResultsException;

import java.io.Serializable;
import java.util.*;

/**
 * VO查询能力接口
 * <p>提供灵活的视图对象查询和类型转换功能</p>
 * <p>所有方法以vo开头，避免与MyBatis-Plus等框架方法冲突</p>
 *
 * <h3>方法命名规则：</h3>
 * <ul>
 *   <li>{@code voById} - 根据ID查询</li>
 *   <li>{@code voByIds} - 根据ID集合批量查询</li>
 *   <li>{@code voFirst} - 查询第一条（有多条返回第一条）</li>
 *   <li>{@code voUnique} - 查询唯一记录（多条抛异常）</li>
 *   <li>{@code voSingle} - 查询单条（voFirst的别名）</li>
 *   <li>{@code voList} - 查询列表</li>
 * </ul>
 *
 * @param <T> 实体类型（Entity）
 * @param <V> 默认视图对象类型（Default VO）
 * @author luminion
 * @since 1.0.0
 */
public interface BoostCore<T, V> {

    /**
     * 转化为Entity实体对象
     *
     * @return {@link Class} 实体类
     */
    T toEntity(Object source);

    /**
     * 转换为VO对象
     *
     * @param source 源对象
     * @return {@link V} VO对象
     */
    V toVo(Object source);

    // ==================== 根据ID查询 ====================

    /**
     * 根据ID查询VO对象
     *
     * @param id 主键ID
     * @return VO对象，不存在返回null
     */
    V voById(Serializable id);

    /**
     * 根据ID查询并转换为指定VO类型
     *
     * @param id     主键ID
     * @param voType 目标VO类型
     * @return VO对象，不存在返回null
     */
    <R> R voById(Serializable id, Class<R> voType);

    /**
     * 根据ID查询VO对象（返回Optional）
     *
     * @param id 主键ID
     * @return Optional包装的VO对象
     */
    Optional<V> voByIdOpt(Serializable id);

    /**
     * 根据ID查询并转换为指定VO类型（返回Optional）
     *
     * @param id     主键ID
     * @param voType 目标VO类型
     * @return Optional包装的VO对象
     */
    <R> Optional<R> voByIdOpt(Serializable id, Class<R> voType);

    // ==================== 批量ID查询 ====================

    /**
     * 根据ID集合批量查询VO对象
     *
     * @param ids ID集合
     * @return VO对象列表，无结果返回空列表
     */
    List<V> voListByIds(Collection<? extends Serializable> ids);

    /**
     * 根据ID集合批量查询并转换为指定VO类型
     *
     * @param ids    ID集合
     * @param voType 目标VO类型
     * @return VO对象列表，无结果返回空列表
     */
    <R> List<R> voListByIds(Collection<? extends Serializable> ids, Class<R> voType);

    // ==================== 查询第一条（voFirst） ====================

    /**
     * 根据条件查询第一个VO对象
     * <p>如果查询结果有多条，返回第一条</p>
     * <p>如果无结果，返回null</p>
     *
     * @param criteria 查询条件
     * @return VO对象，不存在返回null
     */
    V voFirst(ISqlEntity<T> criteria);

    /**
     * 根据条件查询第一个VO对象并转换类型
     *
     * @param criteria 查询条件
     * @param voType   目标VO类型
     * @return VO对象，不存在返回null
     */
    <R> R voFirst(ISqlEntity<T> criteria, Class<R> voType);

    /**
     * 根据条件查询第一个VO对象（返回Optional）
     *
     * @param criteria 查询条件
     * @return Optional包装的VO对象
     */
    Optional<V> voFirstOpt(ISqlEntity<T> criteria);

    /**
     * 根据条件查询第一个VO对象并转换类型（返回Optional）
     *
     * @param criteria 查询条件
     * @param voType   目标VO类型
     * @return Optional包装的VO对象
     */
    <R> Optional<R> voFirstOpt(ISqlEntity<T> criteria, Class<R> voType);

    // ==================== 查询唯一记录（voUnique） ====================

    /**
     * 根据条件查询唯一VO对象
     * <p><b>注意：如果查询结果超过1条，将抛出异常</b></p>
     * <p>如果无结果，返回null</p>
     *
     * @param criteria 查询条件
     * @return VO对象，不存在返回null
     */
    V voUnique(ISqlEntity<T> criteria);

    /**
     * 根据条件查询唯一VO对象并转换类型
     * <p><b>注意：如果查询结果超过1条，将抛出异常</b></p>
     *
     * @param criteria 查询条件
     * @param voType   目标VO类型
     * @return VO对象，不存在返回null
     */
    <R> R voUnique(ISqlEntity<T> criteria, Class<R> voType);

    /**
     * 根据条件查询唯一VO对象（返回Optional）
     *
     * @param criteria 查询条件
     * @return Optional包装的VO对象
     */
    Optional<V> voUniqueOpt(ISqlEntity<T> criteria);

    /**
     * 根据条件查询唯一VO对象并转换类型（返回Optional）
     *
     * @param criteria 查询条件
     * @param voType   目标VO类型
     * @return Optional包装的VO对象
     */
    <R> Optional<R> voUniqueOpt(ISqlEntity<T> criteria, Class<R> voType);

    // ==================== 查询列表 ====================

    /**
     * 查询所有VO对象
     *
     * @return VO对象列表，无结果返回空列表
     */
    List<V> voList();

    /**
     * 根据条件查询VO对象列表
     *
     * @param criteria 查询条件
     * @return VO对象列表，无结果返回空列表
     */
    List<V> voList(ISqlEntity<T> criteria);

    /**
     * 根据条件查询VO对象列表并转换类型
     *
     * @param criteria 查询条件
     * @param voType   目标VO类型
     * @return VO对象列表，无结果返回空列表
     */
    <R> List<R> voList(ISqlEntity<T> criteria, Class<R> voType);

    // ==================== 分页查询 由子分页实现 ====================

 

}