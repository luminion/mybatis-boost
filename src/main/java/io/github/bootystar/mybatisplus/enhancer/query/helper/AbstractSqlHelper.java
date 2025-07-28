package io.github.bootystar.mybatisplus.enhancer.query.helper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.service.IService;
import io.github.bootystar.mybatisplus.enhancer.EnhancedQuery;
import io.github.bootystar.mybatisplus.enhancer.enums.SqlKeyword;
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
import java.util.Iterator;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author bootystar
 */
@SuppressWarnings({"unused", "unchecked"})
public abstract class AbstractSqlHelper<T, S extends AbstractSqlHelper<T, S>> extends SqlEntity<T> implements ISqlHelperLambda<T, S> {

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
            return this.withSqlTree(((ISqlTree) s));
        }
        if (s instanceof ISqlCondition) {
            return this.withSqlCondition((ISqlCondition) s);
        }
        if (s instanceof ISqlSort) {
            return this.withSqlSort((ISqlSort) s);
        }
        Map<?, ?> map = ReflectUtil.objectToMap(s);
        return this.withMap(map);
    }

    protected S withSqlTree(ISqlTree sqlTree) {
        if (sqlTree == null) {
            return (S) this;
        }
        SqlTree child = this.getLowestChild();
        for (ISqlTree node : sqlTree) {
            String symbol1 = node.getSymbol();
            if (SqlKeyword.OR.keyword.equals(symbol1)) {
                SqlTree newChild = new SqlTree(node.getConditions(), SqlKeyword.OR.keyword, null);
                child.setChild(newChild);
                child = newChild;
            } else {
                this.getConditions().addAll(node.getConditions());
            }
        }
        return (S) this;
    }

    protected S withSqlCondition(ISqlCondition sqlCondition) {
        if (sqlCondition == null) {
            return (S) this;
        }
        this.getConditions().add(sqlCondition);
        return (S) this;
    }

    protected S withSqlSort(ISqlSort sqlSort) {
        if (sqlSort == null) {
            return (S) this;
        }
        this.getSorts().add(sqlSort);
        return (S) this;
    }

    protected S withMap(Map<?, ?> map) {
        if (map == null) {
            return (S) this;
        }
        for (Map.Entry<?, ?> next : map.entrySet()) {
            Object key = next.getKey();
            Object value = next.getValue();
            SqlCondition condition = new SqlCondition(key.toString(), value);
            this.getConditions().add(condition);
        }
        return (S) this;
    }

}
