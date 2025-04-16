package io.github.bootystar.mybatisplus.enhancer.core.base;

import com.baomidou.mybatisplus.core.metadata.TableInfo;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.util.CastHelper;
import io.github.bootystar.mybatisplus.enhancer.util.MybatisPlusReflectHelper;


/**
 * @author bootystar
 */
@SuppressWarnings("unused")
public interface EnhancedIService {

    @SuppressWarnings("unchecked")
    default <R> R toId(Object source) {
        IService<?> iService = CastHelper.cast(this,IService.class);
        TableInfo tableInfo = TableInfoHelper.getTableInfo(iService.getEntityClass());
        if (tableInfo == null) return null;
        String keyProperty = tableInfo.getKeyProperty();
        if (keyProperty == null) return null;
        return (R) tableInfo.getPropertyValue(source, keyProperty);
    }

    @SuppressWarnings({"rawtypes", "unchecked"})
    default <R> R insertByDTO(Object s) {
        IService iService = CastHelper.cast(this,IService.class);
        Object entity = MybatisPlusReflectHelper.toTarget(s, iService.getEntityClass());
        iService.save(entity);
        return toId(entity);
    }

    @SuppressWarnings({"rawtypes", "unchecked"})
    default boolean updateByDTO(Object s) {
        IService iService = CastHelper.cast(this,IService.class);
        Object entity = MybatisPlusReflectHelper.toTarget(s, iService.getEntityClass());
        return iService.updateById(entity);
    }


}
