package io.github.bootystar.mybatisplus.enhancer.query.helper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.EnhancedQuery;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlCondition;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlEntity;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlSort;
import io.github.bootystar.mybatisplus.enhancer.query.core.ISqlTree;
import io.github.bootystar.mybatisplus.enhancer.query.entity.SqlCondition;
import io.github.bootystar.mybatisplus.enhancer.query.entity.SqlEntity;
import io.github.bootystar.mybatisplus.enhancer.query.entity.SqlSort;
import io.github.bootystar.mybatisplus.enhancer.query.entity.SqlTree;
import io.github.bootystar.mybatisplus.enhancer.util.ReflectUtil;

import java.util.Collection;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author bootystar
 */
@SuppressWarnings("unused")
public abstract class AbstractSqlHelper<T, S extends AbstractSqlHelper<T, S>> extends SqlEntity<T> implements ISqlHelperLambda<T,S> {
    

    /**
     * 根据指定对象字段映射等于条件
     *
     * @param s s
     * @return {@link SqlHelper}
     */
    @SuppressWarnings("unchecked")
    public S withObject(Object s) {
        if (s == null) {
            return (S) this;
        }
        if (s instanceof ISqlTree) {
            return ofSqlTree((ISqlTree) s, true);
        }
        if (s instanceof ISqlCondition) {
        }
        if (s instanceof ISqlSort) {
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
