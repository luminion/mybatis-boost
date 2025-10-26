package io.github.luminion.mybatisplus.enhancer;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.luminion.mybatisplus.enhancer.core.EnhancedExcel;
import io.github.luminion.mybatisplus.enhancer.core.EnhancedIService;
import io.github.luminion.mybatisplus.enhancer.core.EnhancedQuery;
import io.github.luminion.mybatisplus.enhancer.util.CastUtil;

import java.util.List;
import java.util.Objects;

/**
 * 动态服务接口
 * <p>
 * 提供动态SQL查询功能，整合IService和EnhancedQuery接口
 *
 * @param <V> VO类型
 * @author luminion
 */
public interface EnhancedService<V> extends EnhancedIService, EnhancedQuery<V>, EnhancedExcel {

    /**
     * VO查询
     *
     * @param param 查询参数
     * @param page 分页对象
     * @return {@link List} VO对象列表
     */
    @Override
    @SuppressWarnings({"unchecked", "rawtypes"})
    default List<V> voQuery(Object param, IPage<V> page) {
        IService iService = CastUtil.cast(this, IService.class);
        EnhancedMapper flexibleViewMapper = CastUtil.cast(iService.getBaseMapper(), EnhancedMapper.class);
        if (!Objects.equals(flexibleViewMapper.getVOClass(), getVOClass())) {
            throw new IllegalStateException("baseMapper has different vo class: " + flexibleViewMapper.getVOClass().getName());
        }
        return flexibleViewMapper.voQuery(param, page);
    }
    
}