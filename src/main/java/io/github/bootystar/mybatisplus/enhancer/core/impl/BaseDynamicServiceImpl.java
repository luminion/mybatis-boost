package io.github.bootystar.mybatisplus.enhancer.core.impl;

import com.alibaba.excel.EasyExcel;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.metadata.TableInfo;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.baomidou.mybatisplus.extension.service.IService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import io.github.bootystar.mybatisplus.enhancer.core.DynamicMapper;
import io.github.bootystar.mybatisplus.enhancer.core.DynamicService;
import io.github.bootystar.mybatisplus.enhancer.helper.SqlHelper;
import io.github.bootystar.mybatisplus.enhancer.helper.unmodifiable.DynamicSqlSqlHelper;
import io.github.bootystar.mybatisplus.enhancer.util.MybatisPlusReflectHelper;

import java.io.InputStream;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 动态查询实现
 *
 * @author bootystar
 */
public abstract class BaseDynamicServiceImpl<M extends BaseMapper<T> & DynamicMapper<V>, T, V> extends ServiceImpl<M, T> implements DynamicService<V> {
    
    public T toEntity(Object source) {
        return MybatisPlusReflectHelper.toTarget(source, getEntityClass());
    }
    
    public Object toId(T source) {
        TableInfo tableInfo = TableInfoHelper.getTableInfo(getEntityClass());
        if (tableInfo == null) return null;
        String keyProperty = tableInfo.getKeyProperty();
        if (keyProperty == null) return null;
        return tableInfo.getPropertyValue(source, keyProperty);
    }
    
    public Object insertByDTO(Object s) {
        T entity = toEntity(s);
        save(entity);
        return toId(entity);
    }
    
    public boolean updateByDTO(Object s) {
        return updateById(toEntity(s));
    }

    @Override
    public int excelImport(InputStream is, Class<?> clazz) {
        List<?> dataList = EasyExcel.read(is).head(clazz).sheet().doReadSync();
        if (dataList == null || dataList.isEmpty()) return 0;
        List<T> entityList = dataList.stream().map(this::toEntity).collect(Collectors.toList());
        saveBatch(entityList);
        return entityList.size();
    }

}
