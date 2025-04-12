package io.github.bootystar.mybatisplus.enhancer.core;

import com.alibaba.excel.write.style.column.LongestMatchColumnWidthStyleStrategy;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.metadata.TableInfo;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhancer.helper.SqlHelperWrapper;
import io.github.bootystar.mybatisplus.enhancer.query.general.SqlConditionG;
import io.github.bootystar.mybatisplus.enhancer.util.ExcelHelper;
import io.github.bootystar.mybatisplus.enhancer.util.MybatisPlusReflectHelper;
import org.apache.ibatis.exceptions.TooManyResultsException;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * service
 *
 * @author bootystar
 */
@SuppressWarnings("unused")
public interface DynamicService<V> {

    @SuppressWarnings("unchecked")
    default Class<V> voClass() {
        return (Class<V>) MybatisPlusReflectHelper.resolveTypeArguments(getClass(), DynamicService.class)[0];
    }
    
    default V toVO(Object source) {
        return MybatisPlusReflectHelper.toTarget(source, voClass());
    }

    List<V> doSelect(Object s, IPage<V> page);

    default V voById(Serializable id) {
        if (id == null) throw new IllegalArgumentException("id can't be null");
        Class<?> clazz =  MybatisPlusReflectHelper.resolveTypeArguments(getClass(), IService.class)[0];
        TableInfo tableInfo = TableInfoHelper.getTableInfo(clazz);
        if (tableInfo == null) throw new IllegalArgumentException("there is no id field in entity");
        String keyProperty = tableInfo.getKeyProperty();
        if (keyProperty == null) throw new IllegalArgumentException("there is no id field in entity");
        SqlConditionG condition = new SqlConditionG(keyProperty, SqlKeyword.EQ.keyword, id);
        return voByDTO(condition);
    }

    default <R> R voById(Serializable id, Class<R> clazz) {
        return MybatisPlusReflectHelper.toTarget(voById(id), clazz);
    }

    default V voByDTO(Object s) {
        List<V> vs = listByDTO(s);
        if (vs == null || vs.isEmpty()) return null;
        if (vs.size() > 1) throw new TooManyResultsException("error query => required one but found " + vs.size());
        return vs.get(0);
    }

    default <R> R voByDTO(Object s, Class<R> clazz) {
        return MybatisPlusReflectHelper.toTarget(voByDTO(s), clazz);
    }

    default List<V> listByDTO() {
        return doSelect(null, null);
    }

    default List<V> listByDTO(Object s) {
        return doSelect(s, null);
    }

    default <R> List<R> listByDTO(Object s, Class<R> clazz) {
        return listByDTO(s).stream()
                .map(e -> MybatisPlusReflectHelper.toTarget(e, clazz))
                .collect(Collectors.toList());
    }

    default IPage<V> pageByDTO(Object s, Long current, Long size) {
        if (current == null || current < 1) current = 1L;
        if (size == null) size = 10L;
        IPage<V> page = new Page<>(current, size);
        List<V> vs = doSelect(s, page);
        page.setRecords(vs);
        return page;
    }

    @SuppressWarnings("unchecked")
    default <R> IPage<R> pageByDTO(Object s, Long current, Long size, Class<R> clazz) {
        IPage<R> vp = (IPage<R>) pageByDTO(s, current, size);
        vp.setRecords(
                vp.getRecords().stream()
                        .map(e -> MybatisPlusReflectHelper.toTarget(e, clazz))
                        .collect(Collectors.toList())
        );
        return vp;
    }

    default void excelTemplate(OutputStream os, Class<?> clazz) {
        ExcelHelper.write(os, clazz)
                .registerWriteHandler(new LongestMatchColumnWidthStyleStrategy())
                .sheet()
                .doWrite(Collections.emptyList());
    }

    int excelImport(InputStream is, Class<?> clazz);
//    default int excelImport(InputStream is, Class<?> clazz) {
//        List<?> dataList = EasyExcel.read(is).head(clazz).sheet().doReadSync();
//        if (dataList == null || dataList.isEmpty()) return 0;
//        List<T> entityList = dataList.stream().map(this::toEntity).collect(Collectors.toList());
//        saveBatch(entityList);
//        return entityList.size();
//    }

    default void excelExport(Object s, OutputStream os, Class<?> clazz, String... includeFields) {
        excelExport(s, os, clazz, 1L, -1L, includeFields);
    }

    default void excelExport(Object s, OutputStream os, Class<?> clazz, Long current, Long size, String... includeFields) {
        List<V> voList = pageByDTO(s, current, size).getRecords();
        ExcelHelper.write(os, clazz)
                .includeColumnFieldNames(Arrays.asList(includeFields))
                .registerWriteHandler(new LongestMatchColumnWidthStyleStrategy())
                .sheet()
                .doWrite(voList);
    }
    

}
