package io.github.bootystar.mybatisplus.enhance.helper;

import com.baomidou.mybatisplus.core.toolkit.LambdaUtils;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import io.github.bootystar.mybatisplus.enhance.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhance.query.ISqlCondition;
import io.github.bootystar.mybatisplus.enhance.query.ISqlSort;
import io.github.bootystar.mybatisplus.enhance.query.ISqlTree;
import io.github.bootystar.mybatisplus.enhance.query.general.ConditionG;
import io.github.bootystar.mybatisplus.enhance.query.general.SortG;
import io.github.bootystar.mybatisplus.enhance.query.general.TreeG;
import org.apache.ibatis.reflection.property.PropertyNamer;

import java.util.Collection;
import java.util.LinkedHashSet;

/**
 * sql助手抽象类
 *
 * @author bootystar
 */
@SuppressWarnings("unused")
public abstract class AbstractSqlHelper<T, Child extends AbstractSqlHelper<T, Child>> extends TreeG {
    protected boolean orNext;

    {
        this.conditions = new LinkedHashSet<>(4);
        this.sorts = new LinkedHashSet<>(4);
    }

    protected String getFieldName(SFunction<T, ?> getter) {
        return PropertyNamer.methodToProperty(LambdaUtils.extract(getter).getImplMethodName());
    }

    @SuppressWarnings("unchecked")
    protected Child returnValue() {
        return (Child) this;
    }

    /**
     * 将所有已有条件设置为子条件
     * <p>
     * 仅操作条件, 不会影响排序
     * <p>
     * 运行方法后,若未添加新条件, 可能会抛出{@link io.github.bootystar.mybatisplus.enhance.expception.ParamMappingException}异常
     *
     * @return {@link Child }
     * @author bootystar
     */
    public Child requiredNext() {
        TreeG oldTree = this.getChild();
        TreeG newTree = new TreeG();
        this.setChild(newTree);
        newTree.setChild(oldTree);
        newTree.setConditions(this.getConditions());
        this.conditions = new LinkedHashSet<>();
        return returnValue();
    }

    /**
     * 将下一个条件的链接符号设置为or
     *
     * @return {@link Child }
     * @author bootystar
     */
    public Child or() {
        this.orNext = true;
        return returnValue();
    }

    protected boolean isOrNext() {
        boolean orNext1 = this.orNext;
        this.orNext = false;
        return orNext1;
    }

    /**
     * 等于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link Child }
     * @author bootystar
     */
    public <R> Child eq(SFunction<T, R> getter, R value) {
        this.condition(new ConditionG(this.getFieldName(getter), SqlKeyword.EQ.keyword, value));
        return returnValue();
    }

    /**
     * 不等于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link Child }<{@link T }>
     * @author bootystar
     */
    public <R> Child ne(SFunction<T, R> getter, R value) {
        this.condition(new ConditionG(this.getFieldName(getter), SqlKeyword.NE.keyword, value));
        return returnValue();
    }

    /**
     * 大于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link Child }
     * @author bootystar
     */
    public <R> Child gt(SFunction<T, R> getter, R value) {
        this.condition(new ConditionG(this.getFieldName(getter), SqlKeyword.GT.keyword, value));
        return returnValue();
    }

    /**
     * 大于等于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link Child }
     * @author bootystar
     */
    public <R> Child ge(SFunction<T, R> getter, R value) {
        this.condition(new ConditionG(this.getFieldName(getter), SqlKeyword.GE.keyword, value));
        return returnValue();
    }

    /**
     * 小于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link Child }
     * @author bootystar
     */
    public <R> Child lt(SFunction<T, R> getter, R value) {
        this.condition(new ConditionG(this.getFieldName(getter), SqlKeyword.LT.keyword, value));
        return returnValue();
    }

    /**
     * 小于等于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link Child }
     * @author bootystar
     */
    public <R> Child le(SFunction<T, R> getter, R value) {
        this.condition(new ConditionG(this.getFieldName(getter), SqlKeyword.LE.keyword, value));
        return returnValue();
    }

    /**
     * 模糊查询
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link Child }
     * @author bootystar
     */
    public <R> Child like(SFunction<T, R> getter, R value) {
        this.condition(new ConditionG(this.getFieldName(getter), SqlKeyword.LIKE.keyword, value));
        return returnValue();
    }

    /**
     * 不模糊查询
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link Child }
     * @author bootystar
     */
    public <R> Child notLike(SFunction<T, R> getter, R value) {
        this.condition(new ConditionG(this.getFieldName(getter), SqlKeyword.NOT_LIKE.keyword, value));
        return returnValue();
    }

    /**
     * in查询
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link Child }
     * @author bootystar
     */
    public <R> Child in(SFunction<T, R> getter, Collection<? extends R> value) {
        this.condition(new ConditionG(this.getFieldName(getter), SqlKeyword.IN.keyword, value));
        return returnValue();
    }

    /**
     * notin查询
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return {@link Child }
     * @author bootystar
     */
    public <R> Child notIn(SFunction<T, R> getter, Collection<? extends R> value) {
        this.condition(new ConditionG(this.getFieldName(getter), SqlKeyword.NOT_IN.keyword, value));
        return returnValue();
    }

    /**
     * 指定字段为空
     *
     * @param getter 对象getter方法
     * @return {@link Child }
     * @author bootystar
     */
    public Child isNull(SFunction<T, ?> getter) {
        this.condition(new ConditionG(this.getFieldName(getter), SqlKeyword.IS_NULL.keyword, null));
        return returnValue();
    }

    /**
     * 指定字段不为空
     *
     * @param getter 对象getter方法
     * @return {@link Child }
     * @author bootystar
     */
    public Child isNotNull(SFunction<T, ?> getter) {
        this.condition(new ConditionG(this.getFieldName(getter), SqlKeyword.IS_NOT_NULL.keyword, null));
        return returnValue();
    }

    public Child orderByAsc(SFunction<T, ?> getter) {
        this.sort(new SortG(this.getFieldName(getter), false));
        return returnValue();
    }

    public Child orderByDesc(SFunction<T, ?> getter) {
        this.sort(new SortG(this.getFieldName(getter), true));
        return returnValue();
    }

    /**
     * 添加一般条件
     * <p>
     * 和现有条件同等优先级
     *
     * @param condition 条件
     * @return {@link Child }
     * @author bootystar
     */
    public Child condition(ISqlCondition condition) {
        if (condition == null) {
            return returnValue();
        }
        ConditionG conditionG = ConditionG.of(condition);
        conditionG.setOr(this.isOrNext());
        this.getConditions().add(conditionG);
        return returnValue();
    }

    /**
     * 添加排序
     *
     * @param sort 排序
     * @return {@link Child }
     * @author bootystar
     */
    public Child sort(ISqlSort sort) {
        if (sort == null) {
            return returnValue();
        }
        this.getSorts().add(SortG.of(sort));
        return returnValue();
    }

    /**
     * 添加指定sql树的所有条件(包括其子条件)
     * <p>
     * 条件的连接符取决于原条件, 不受{@link #or()}方法影响
     *
     * @param sqlTree sql树
     * @return {@link Child }
     * @author bootystar
     */
    public Child with(ISqlTree sqlTree) {
        if (sqlTree == null || sqlTree.getConditions() == null || sqlTree.getConditions().isEmpty()) {
            return returnValue();
        }
        this.getConditions().addAll(SqlHelper.of(sqlTree).getConditions());
        if (sqlTree.getChild() != null) {
            return withChild(sqlTree.getChild());
        }
        return returnValue();
    }

    /**
     * 将指定的sql树所有条件(包括其子条件)作为子条件添加
     * <p>
     * 条件的连接符取决于原条件, 不受{@link #or()}方法影响
     *
     * @param sqlTree sql树
     * @return {@link Child }
     * @author bootystar
     */
    public Child withChild(ISqlTree sqlTree) {
        TreeG tree = this;
        while (tree.getChild() != null) {
            tree = tree.getChild();
        }
        tree.setChild(SqlHelper.of(sqlTree));
        return returnValue();
    }


}
