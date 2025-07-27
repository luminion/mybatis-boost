package io.github.bootystar.mybatisplus.enhancer;

import com.baomidou.mybatisplus.core.metadata.TableInfo;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.util.CastUtil;
import io.github.bootystar.mybatisplus.enhancer.util.MybatisPlusReflectUtil;


/**
 * @author bootystar
 */
@SuppressWarnings("unused")
public interface EnhancedIService {
//public interface EnhancedIService<T> extends IService<T> {

//    default T toEntity(Object source) {
//        return MybatisPlusReflectHelper.toTarget(source, getEntityClass());
//    }
    
    default Object toId(Object source) {
        IService<?> iService = CastUtil.cast(this,IService.class);
        TableInfo tableInfo = TableInfoHelper.getTableInfo(iService.getEntityClass());
        if (tableInfo == null) return null;
        String keyProperty = tableInfo.getKeyProperty();
        if (keyProperty == null) return null;
        return tableInfo.getPropertyValue(source, keyProperty);
    }

    @SuppressWarnings({"rawtypes", "unchecked"})
    default Object insertByDTO(Object s) {
        IService iService = CastUtil.cast(this,IService.class);
        Object entity = MybatisPlusReflectUtil.toTarget(s, iService.getEntityClass());
        iService.save(entity);
        return toId(entity);
    }

    @SuppressWarnings({"rawtypes", "unchecked"})
    default boolean updateByDTO(Object s) {
        IService iService = CastUtil.cast(this,IService.class);
        Object entity = MybatisPlusReflectUtil.toTarget(s, iService.getEntityClass());
        return iService.updateById(entity);
    }


}
