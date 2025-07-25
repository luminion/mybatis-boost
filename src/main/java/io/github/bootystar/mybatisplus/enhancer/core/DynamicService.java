package io.github.bootystar.mybatisplus.enhancer.core;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.core.base.EnhancedExcel;
import io.github.bootystar.mybatisplus.enhancer.core.base.EnhancedIService;
import io.github.bootystar.mybatisplus.enhancer.core.base.EnhancedQuery;
import io.github.bootystar.mybatisplus.enhancer.util.CastHelper;

import java.util.List;
import java.util.Objects;

/**
 * @author bootystar
 */
public interface DynamicService<V> extends EnhancedIService, EnhancedQuery<V>, EnhancedExcel {

    @Override
    @SuppressWarnings({"unchecked", "rawtypes"})
    default List<V> voSelect(Object s, IPage<V> page) {
        IService iService = CastHelper.cast(this, IService.class);
        DynamicMapper dynamicMapper = CastHelper.cast(iService.getBaseMapper(), DynamicMapper.class);
        if (!Objects.equals(dynamicMapper.getVOClass(), getVOClass())) {
            throw new IllegalStateException("baseMapper has different vo class: " + dynamicMapper.getVOClass().getName());
        }
        return dynamicMapper.voSelect(s, page);
    }
    
}
