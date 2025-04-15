package io.github.bootystar.mybatisplus.enhancer.helper;

import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.core.DynamicQueryService;
import io.github.bootystar.mybatisplus.enhancer.query.SqlCondition;
import io.github.bootystar.mybatisplus.enhancer.query.SqlEntity;
import io.github.bootystar.mybatisplus.enhancer.query.SqlSort;
import io.github.bootystar.mybatisplus.enhancer.query.SqlTree;
import io.github.bootystar.mybatisplus.enhancer.query.general.SqlConditionG;
import io.github.bootystar.mybatisplus.enhancer.query.general.SqlSortG;
import io.github.bootystar.mybatisplus.enhancer.util.ReflectHelper;

import java.util.Collection;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * sql助手
 *
 * @author bootystar
 */
@SuppressWarnings("unused")
public class SqlHelper<T> extends AbstractSqlHelper<T, SqlHelper<T>> {


    /**
     * 返回指定泛型的sql助手
     *
     * @return {@link SqlHelper }
     */
    public static <T> SqlHelper<T> of() {
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
        if (s instanceof SqlTree) {
            return ofSqlTree((SqlTree) s, true);
        }
        SqlHelper<T> helper = new SqlHelper<>();
        if (s instanceof SqlCondition) {
            helper.condition((SqlCondition) s);
        }
        if (s instanceof SqlSort) {
            helper.sort((SqlSort) s);
        }
        Map<?, ?> map = ReflectHelper.objectToMap(s);
        for (Map.Entry<?, ?> next : map.entrySet()) {
            Object key = next.getKey();
            Object value = next.getValue();
            SqlConditionG condition = new SqlConditionG(key.toString(), value);
            helper.condition(condition);
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
    protected static <T> SqlHelper<T> ofSqlTree(SqlTree tree, boolean copySorts) {
        if (tree == null) {
            return new SqlHelper<>();
        }
        SqlHelper<T> helper = new SqlHelper<>();
        Collection<? extends SqlCondition> conditions1 = tree.getConditions();
        if (conditions1 != null && !conditions1.isEmpty()) {
            helper.getConditions().addAll(conditions1.stream().map(SqlConditionG::of).collect(Collectors.toList()));
        }
        if (copySorts) {
            if (tree instanceof SqlEntity) {
                Collection<? extends SqlSort> treeSorts = ((SqlEntity) tree).getSorts();
                if (treeSorts != null && !treeSorts.isEmpty()) {
                    helper.getSorts().addAll(treeSorts.stream().map(SqlSortG::of).collect(Collectors.toList()));
                }
            }
        }
        SqlTree child = tree.getChild();
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
    public <V, S extends IService<T> & DynamicQueryService<V>> SqlHelperWrapper<T, V, S> wrap(S baseService) {
        return new SqlHelperWrapper<>(baseService).with(this);
    }

}
