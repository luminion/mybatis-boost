package io.github.bootystar.mybatisplus.enhancer.query.helper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.EnhancedQuery;
import io.github.bootystar.mybatisplus.enhancer.util.MybatisPlusReflectUtil;

import java.util.List;

/**
 * SQL助手包装器
 * <p>
 * 提供对EnhancedQuery接口的包装，简化VO查询操作，支持链式调用
 *
 * @param <T> 实体类型
 * @param <V> VO类型
 * @author bootystar
 */
@SuppressWarnings("unused")
public class SqlHelperWrapper<T, V> extends SqlHelper<T> {

    /**
     * 增强查询接口实例
     */
    private final EnhancedQuery<V> enhancedQuery;

    /**
     * 构造函数，使用IService实例初始化
     *
     * @param baseService 实现IService和EnhancedQuery接口的服务实例
     */
    public <S extends IService<T> & EnhancedQuery<V>> SqlHelperWrapper(S baseService) {
        this.enhancedQuery = baseService;
        this.entityClass = baseService.getEntityClass();
    }

    /**
     * 构造函数，使用BaseMapper实例初始化
     *
     * @param baseMapper 实现BaseMapper和EnhancedQuery接口的Mapper实例
     */
    public <S extends BaseMapper<T> & EnhancedQuery<V>> SqlHelperWrapper(S baseMapper) {
        this.enhancedQuery = baseMapper;
    }

    /**
     * 查询单个VO对象
     *
     * @return VO对象
     */
    public V one() {
        return enhancedQuery.voByDTO(this);
    }

    /**
     * 查询VO对象列表
     *
     * @return VO对象列表
     */
    public List<V> list() {
        return enhancedQuery.voList(this);
    }

    /**
     * 分页查询VO对象
     *
     * @param current 当前页码
     * @param size    每页大小
     * @return 分页结果
     */
    public IPage<V> page(Long current, Long size) {
        return enhancedQuery.voPage(this, current, size);
    }


}