package io.github.bootystar.mybatisplus.enhancer;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlEntity;
import io.github.bootystar.mybatisplus.enhancer.query.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhancer.query.helper.processor.FieldSuffixProcessor;
import io.github.bootystar.mybatisplus.enhancer.util.MybatisPlusReflectUtil;

import java.util.List;

/**
 * 动态Mapper接口
 * <p>
 * 提供动态SQL查询功能，支持通过条件对象进行VO查询
 *
 * @param <V> VO类型
 * @author bootystar
 */
public interface DynamicMapper<V> extends EnhancedQuery<V> {

    /**
     * 通过XML进行VO查询
     *
     * @param s    SQL实体
     * @param page 分页对象
     * @return {@link List} VO对象列表
     */
    List<V> voQueryByXml(ISqlEntity s, IPage<V> page);

    /**
     * VO查询
     *
     * @param param 查询参数
     * @param page  分页对象
     * @return {@link List} VO对象列表
     */
    @Override
    default List<V> voQuery(Object param, IPage<V> page) {
        Class<?> entityClass = MybatisPlusReflectUtil.resolveTypeArguments(getClass(), BaseMapper.class)[0];
        return voQueryByXml(SqlHelper.of(entityClass).with(param).process(FieldSuffixProcessor.of()::process), page);
    }

}