package io.github.bootystar.mybatisplus.enhancer;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.metadata.TableInfo;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhancer.query.entity.SqlCondition;
import io.github.bootystar.mybatisplus.enhancer.util.MybatisPlusReflectUtil;
import org.apache.ibatis.exceptions.TooManyResultsException;

import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;

/**
 * service
 *
 * @author bootystar
 */
@SuppressWarnings("unused")
public interface EnhancedQuery<V> {

    List<V> voQuery(Object s, IPage<V> page);

    @SuppressWarnings("unchecked")
    default Class<V> voClass() {
        return (Class<V>) MybatisPlusReflectUtil.resolveTypeArguments(getClass(), EnhancedQuery.class)[0];
    }

    default V toVO(Object source) {
        return MybatisPlusReflectUtil.toTarget(source, voClass());
    }

    default V voById(Serializable id) {
        if (id == null) throw new IllegalArgumentException("id can't be null");
        Class<?> clazz =  MybatisPlusReflectUtil.resolveTypeArguments(getClass(), IService.class)[0];
        TableInfo tableInfo = TableInfoHelper.getTableInfo(clazz);
        if (tableInfo == null) throw new IllegalArgumentException("there is no id field in entity");
        String keyProperty = tableInfo.getKeyProperty();
        if (keyProperty == null) throw new IllegalArgumentException("there is no id field in entity");
        SqlCondition condition = new SqlCondition(keyProperty, SqlKeyword.EQ.keyword, id);
        return voByDTO(condition);
    }

    default <R> R voById(Serializable id, Class<R> clazz) {
        return MybatisPlusReflectUtil.toTarget(voById(id), clazz);
    }

    default V voByDTO(Object s) {
        List<V> vs = voList(s);
        if (vs == null || vs.isEmpty()) return null;
        if (vs.size() > 1) throw new TooManyResultsException("error query => required one but found " + vs.size());
        return vs.get(0);
    }

    default <R> R voByDTO(Object s, Class<R> clazz) {
        return MybatisPlusReflectUtil.toTarget(voByDTO(s), clazz);
    }

    default List<V> voList() {
        return voQuery(null, null);
    }

    default List<V> voList(Object s) {
        return voQuery(s, null);
    }

    default <R> List<R> voList(Object s, Class<R> clazz) {
        return voList(s).stream()
                .map(e -> MybatisPlusReflectUtil.toTarget(e, clazz))
                .collect(Collectors.toList());
    }

    default IPage<V> voPage(Long current, Long size) {
        if (current == null || current < 1) current = 1L;
        if (size == null) size = 10L;
        IPage<V> page = new Page<>(current, size);
        List<V> vs = voQuery(null, page);
        page.setRecords(vs);
        return page;
    }
    
    default IPage<V> voPage(Object s, Long current, Long size) {
        if (current == null || current < 1) current = 1L;
        if (size == null) size = 10L;
        IPage<V> page = new Page<>(current, size);
        List<V> vs = voQuery(s, page);
        page.setRecords(vs);
        return page;
    }

    @SuppressWarnings("unchecked")
    default <R> IPage<R> voPage(Object s, Long current, Long size, Class<R> clazz) {
        IPage<R> vp = (IPage<R>) voPage(s, current, size);
        vp.setRecords(
                vp.getRecords().stream()
                        .map(e -> MybatisPlusReflectUtil.toTarget(e, clazz))
                        .collect(Collectors.toList())
        );
        return vp;
    }
    

    

}
