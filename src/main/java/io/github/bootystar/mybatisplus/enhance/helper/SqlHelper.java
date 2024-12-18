package io.github.bootystar.mybatisplus.enhance.helper;

import io.github.bootystar.mybatisplus.enhance.core.DynamicService;
import io.github.bootystar.mybatisplus.enhance.query.ISqlCondition;
import io.github.bootystar.mybatisplus.enhance.query.ISqlSort;
import io.github.bootystar.mybatisplus.enhance.query.ISqlTree;
import io.github.bootystar.mybatisplus.enhance.query.general.ConditionG;
import io.github.bootystar.mybatisplus.enhance.query.general.SortG;
import io.github.bootystar.mybatisplus.util.ReflectHelper;

import java.util.Collection;
import java.util.Map;
import java.util.Objects;
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
     * @return {@link SqlHelper }<{@link T }>
     * @author bootystar
     */
    public static <T> SqlHelper<T> of() {
        return new SqlHelper<>();
    }

    /**
     * 根据指定对象字段映射等于条件
     *
     * @param s s
     * @return {@link SqlHelper<T> }
     * @author bootystar
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
            helper.condition((ISqlCondition) s);
        }
        if (s instanceof ISqlSort) {
            helper.sort((ISqlSort) s);
        }
        Map<?, ?> map = ReflectHelper.objectToMap(s);
        for (Map.Entry<?, ?> next : map.entrySet()) {
            Object key = next.getKey();
            Object value = next.getValue();
            ConditionG condition = new ConditionG(key.toString(), value);
            helper.condition(condition);
        }
        return helper;
    }

    /**
     * 根据SqlTree生成sql助手
     *
     * @param tree      树
     * @param copySorts 是否复制排序
     * @return {@link SqlHelper<T> }
     * @author bootystar
     */
    protected static <T> SqlHelper<T> ofSqlTree(ISqlTree tree, boolean copySorts) {
        if (tree == null) {
            return new SqlHelper<>();
        }
        SqlHelper<T> helper = new SqlHelper<>();
        Collection<? extends ISqlCondition> conditions1 = tree.getConditions();
        if (conditions1 != null && !conditions1.isEmpty()) {
            helper.getConditions().addAll(conditions1.stream().map(ConditionG::of).collect(Collectors.toList()));
        }
        if (copySorts) {
            Collection<? extends ISqlSort> treeSorts = tree.getSorts();
            if (treeSorts != null && !treeSorts.isEmpty()) {
                helper.getSorts().addAll(treeSorts.stream().map(SortG::of).collect(Collectors.toList()));
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
     * @return {@link SqlHelperWrapper }<{@link T },{@link V }>
     * @author bootystar
     */
    public <V> SqlHelperWrapper<T, V> wrap(DynamicService<T, V> baseService) {
        return new SqlHelperWrapper<>(baseService).with(this);
    }

}
