package io.github.bootystar.mybatisplus.enhancer.core;

import cn.idev.excel.FastExcel;
import cn.idev.excel.write.style.column.LongestMatchColumnWidthStyleStrategy;
import com.baomidou.mybatisplus.core.metadata.TableInfo;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.util.ExcelHelper;
import io.github.bootystar.mybatisplus.enhancer.util.MybatisPlusReflectHelper;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author bootystar
 */
public interface EnhancedIService<T> extends IService<T> {
    
    default T toEntity(Object source) {
        return MybatisPlusReflectHelper.toTarget(source, getEntityClass());
    }

    default Object toId(T source) {
        TableInfo tableInfo = TableInfoHelper.getTableInfo(getEntityClass());
        if (tableInfo == null) return null;
        String keyProperty = tableInfo.getKeyProperty();
        if (keyProperty == null) return null;
        return tableInfo.getPropertyValue(source, keyProperty);
    }

    default Object insertByDTO(Object s) {
        T entity = toEntity(s);
        save(entity);
        return toId(entity);
    }

    default boolean updateByDTO(Object s) {
        return updateById(toEntity(s));
    }

    default void excelTemplate(OutputStream os, Class<?> clazz) {
        ExcelHelper.write(os, clazz)
                .registerWriteHandler(new LongestMatchColumnWidthStyleStrategy())
                .sheet()
                .doWrite(Collections.emptyList());
    }
    
    default int excelImport(InputStream is, Class<?> clazz) {
        List<?> dataList = FastExcel.read(is).head(clazz).sheet().doReadSync();
        if (dataList == null || dataList.isEmpty()) return 0;
        List<T> entityList = dataList.stream().map(this::toEntity).collect(Collectors.toList());
        saveBatch(entityList);
        return entityList.size();
    }
}
