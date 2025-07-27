package io.github.bootystar.mybatisplus.enhancer;

import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.util.CastUtil;
import io.github.bootystar.mybatisplus.enhancer.util.ExcelUtil;
import io.github.bootystar.mybatisplus.enhancer.util.ReflectUtil;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author bootystar
 */
@SuppressWarnings("unused")
public interface EnhancedExcel {

    default void excelTemplate(OutputStream os, Class<?> clazz) {
        ExcelUtil.write(os, clazz, Collections.emptyList());
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    default int excelImport(InputStream is, Class<?> clazz) {
        List<?> dataList = ExcelUtil.read(is, clazz);
        if (dataList == null || dataList.isEmpty()) return 0;
        IService iService = CastUtil.cast(this, IService.class);
        List<?> entityList = dataList.stream()
                .map(e -> ReflectUtil.toTarget(e, iService.getEntityClass()))
                .collect(Collectors.toList());
        iService.saveBatch(entityList);
        return entityList.size();
    }

    default void excelExport(Object s, OutputStream os, Class<?> clazz, String... includeFields) {
        excelExport(s, os, clazz, 1L, -1L, includeFields);
    }

    @SuppressWarnings("rawtypes")
    default void excelExport(Object s, OutputStream os, Class<?> clazz, Long current, Long size, String... includeFields) {
        EnhancedQuery enhancedQuery = CastUtil.cast(this, EnhancedQuery.class);
        List<?> voList = enhancedQuery.voPage(s, current, size).getRecords();
        ExcelUtil.write(os, clazz, voList, includeFields);
    }

}
