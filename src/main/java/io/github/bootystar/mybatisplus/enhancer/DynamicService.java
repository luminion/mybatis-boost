package io.github.bootystar.mybatisplus.enhancer;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.util.CastUtil;

import java.util.List;
import java.util.Objects;

/**
 * @author bootystar
 */
public interface DynamicService<V> extends EnhancedIService, EnhancedQuery<V>, EnhancedExcel {

    @Override
    @SuppressWarnings({"unchecked", "rawtypes"})
    default List<V> voQuery(Object s, IPage<V> page) {
        IService iService = CastUtil.cast(this, IService.class);
        DynamicMapper dynamicMapper = CastUtil.cast(iService.getBaseMapper(), DynamicMapper.class);
        if (!Objects.equals(dynamicMapper.voClass(), voClass())) {
            throw new IllegalStateException("baseMapper has different vo class: " + dynamicMapper.voClass().getName());
        }
        return dynamicMapper.voQuery(s, page);
    }
    
}
