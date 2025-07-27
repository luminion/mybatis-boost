package io.github.bootystar.mybatisplus.enhancer.query.helper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.EnhancedQuery;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlCondition;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlTree;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlEntity;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlSort;
import io.github.bootystar.mybatisplus.enhancer.query.entity.SqlCondition;
import io.github.bootystar.mybatisplus.enhancer.query.entity.SqlSort;
import io.github.bootystar.mybatisplus.enhancer.util.ReflectUtil;

import java.util.Collection;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * sql助手
 *
 * @author bootystar
 */
@SuppressWarnings("unused")
public class SqlHelper<T> extends SqlHelperBase<T, SqlHelper<T>> {

    /**
     * 返回指定泛型的sql助手
     *
     * @return {@link SqlHelper }
     */
    public static <T> SqlHelper<T> of() {
        return new SqlHelper<>();
    }

    /**
     * 返回指定类泛型的sql助手
     *
     * @param clazz 实体类
     * @return {@link SqlHelper }
     */
    public static <T> SqlHelper<T> of(Class<T> clazz) {
        return new SqlHelper<>();
    }

    /**
     * 根据指定对象字段映射等于条件
     *
     * @param s s
     * @return {@link SqlHelper}
     */
    @SuppressWarnings("unchecked")
    public static <T> SqlHelper<T> of(Object s) {
        if (s == null) {
            return new SqlHelper<>();
        }
        if (s instanceof SqlHelper<?>) {
            return (SqlHelper<T>) s;
        }
        if (s instanceof ISqlTree) {
            return ofSqlTree((ISqlTree) s, true);
        }
        SqlHelper<T> helper = new SqlHelper<>();
        if (s instanceof ISqlCondition) {
            helper.addSqlCondition((ISqlCondition) s);
            return helper;
        }
        if (s instanceof ISqlSort) {
            helper.addSqlSort((ISqlSort) s);
            return helper;
        }
        Map<?, ?> map = ReflectUtil.objectToMap(s);
        for (Map.Entry<?, ?> next : map.entrySet()) {
            Object key = next.getKey();
            Object value = next.getValue();
            SqlCondition condition = new SqlCondition(key.toString(), value);
            helper.addSqlCondition(condition);
        }
        return helper;
    }

    /**
     * 根据SqlTree生成sql助手
     *
     * @param tree      树
     * @param copySorts 是否复制排序
     * @return {@link SqlHelper}
     */
    protected static <T> SqlHelper<T> ofSqlTree(ISqlTree tree, boolean copySorts) {
        if (tree == null) {
            return new SqlHelper<>();
        }
        SqlHelper<T> helper = new SqlHelper<>();
        Collection<? extends ISqlCondition> conditions1 = tree.getConditions();
        if (conditions1 != null && !conditions1.isEmpty()) {
            helper.getConditions().addAll(conditions1.stream().map(SqlCondition::of).collect(Collectors.toList()));
        }
        if (copySorts) {
            if (tree instanceof ISqlEntity) {
                Collection<? extends ISqlSort> treeSorts = ((ISqlEntity) tree).getSorts();
                if (treeSorts != null && !treeSorts.isEmpty()) {
                    helper.getSorts().addAll(treeSorts.stream().map(SqlSort::of).collect(Collectors.toList()));
                }
            }
        }
        ISqlTree child = tree.getChild();
        if (child != null) {
            helper.setChild(ofSqlTree(child, false));
        }
        return helper;
    }

    /**
     * 包装sql助手, 添加指定服务的查询方法
     *
     * @param baseService 基础服务
     * @return {@link SqlHelperWrapper }
     */
    public <V, S extends IService<T> & EnhancedQuery<V>> SqlHelperWrapper<T, V> wrap(S baseService) {
        return new SqlHelperWrapper<>(baseService).withSqlTree(this);
    }

    public <V, S extends BaseMapper<T> & EnhancedQuery<V>> SqlHelperWrapper<T, V> wrap(S baseMapper) {
        return new SqlHelperWrapper<>(baseMapper).withSqlTree(this);
    }

}
