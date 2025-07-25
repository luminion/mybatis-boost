package io.github.bootystar.mybatisplus.enhancer.helper;

import com.baomidou.mybatisplus.core.toolkit.LambdaUtils;
import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import io.github.bootystar.mybatisplus.enhancer.enums.SqlKeyword;
import io.github.bootystar.mybatisplus.enhancer.expception.ParamMappingException;
import io.github.bootystar.mybatisplus.enhancer.sql.SqlCondition;
import io.github.bootystar.mybatisplus.enhancer.sql.SqlEntity;
import io.github.bootystar.mybatisplus.enhancer.sql.SqlSort;
import io.github.bootystar.mybatisplus.enhancer.sql.SqlTree;
import io.github.bootystar.mybatisplus.enhancer.sql.general.SqlConditionG;
import io.github.bootystar.mybatisplus.enhancer.sql.general.SqlEntityG;
import io.github.bootystar.mybatisplus.enhancer.sql.general.SqlSortG;
import io.github.bootystar.mybatisplus.enhancer.sql.general.SqlTreeG;
import org.apache.ibatis.reflection.property.PropertyNamer;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.stream.Collectors;

/**
 * sql助手lambda方法
 *
 * @author bootystar
 */
@SuppressWarnings("unused")
public abstract class LambdaSqlHelper<T, Child extends LambdaSqlHelper<T, Child>> extends SqlEntityG {
    protected transient boolean orNext;

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
     * 添加一般条件
     * <p>
     * 和现有条件同等优先级
     *
     * @param condition 条件
     * @return this
     */
    public Child condition(SqlCondition condition) {
        if (condition == null) {
            return returnValue();
        }
        SqlConditionG conditionG = SqlConditionG.of(condition);
        conditionG.setOr(this.isOrNext());
        this.getConditions().add(conditionG);
        return returnValue();
    }

    /**
     * 添加排序
     *
     * @param sort 排序
     * @return this
     */
    public Child sort(SqlSort sort) {
        if (sort == null) {
            return returnValue();
        }
        this.getSorts().add(SqlSortG.of(sort));
        return returnValue();
    }

    /**
     * 添加指定sql树的所有条件(包括其子条件)
     * <p>
     * 条件的连接符取决于原条件, 不受{@link #or()}方法影响
     *
     * @param sqlTree sql树
     * @return this
     */
    public Child with(SqlTree sqlTree) {
        if (sqlTree == null || sqlTree.getConditions() == null || sqlTree.getConditions().isEmpty()) {
            return returnValue();
        }
        this.getConditions().addAll(SqlHelper.of(sqlTree).getConditions());
        if (sqlTree instanceof SqlEntity){
            Collection<? extends SqlSort> treeSorts = ((SqlEntity) sqlTree).getSorts();
            if (treeSorts != null && !treeSorts.isEmpty()) {
                this.getSorts().addAll(treeSorts.stream().map(SqlSortG::of).collect(Collectors.toList()));
            }
        }
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
     * @return this
     */
    public Child withChild(SqlTree sqlTree) {
        SqlTreeG tree = this;
        while (tree.getChild() != null) {
            tree = tree.getChild();
        }
        tree.setChild(SqlHelper.of(sqlTree));
        return returnValue();
    }
    
    /**
     * 将所有已有条件设置为子条件
     * <p>
     * 仅操作条件, 不会影响排序
     * <p>
     * 运行方法后,若未添加新条件, 可能会抛出{@link ParamMappingException}异常
     *
     * @return this
     */
    public Child requiredNext() {
        SqlTreeG oldTree = this.getChild();
        SqlTreeG newTree = new SqlTreeG();
        this.setChild(newTree);
        newTree.setChild(oldTree);
        newTree.setConditions(this.getConditions());
        this.conditions = new LinkedHashSet<>();
        return returnValue();
    }

    /**
     * 将下一个条件的链接符号设置为or
     *
     * @return this
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
     * @return this
     */
    public <R> Child eq(SFunction<T, R> getter, R value) {
        this.condition(new SqlConditionG(this.getFieldName(getter), SqlKeyword.EQ.keyword, value));
        return returnValue();
    }

    /**
     * 不等于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    public <R> Child ne(SFunction<T, R> getter, R value) {
        this.condition(new SqlConditionG(this.getFieldName(getter), SqlKeyword.NE.keyword, value));
        return returnValue();
    }

    /**
     * 大于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    public <R> Child gt(SFunction<T, R> getter, R value) {
        this.condition(new SqlConditionG(this.getFieldName(getter), SqlKeyword.GT.keyword, value));
        return returnValue();
    }

    /**
     * 大于等于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    public <R> Child ge(SFunction<T, R> getter, R value) {
        this.condition(new SqlConditionG(this.getFieldName(getter), SqlKeyword.GE.keyword, value));
        return returnValue();
    }

    /**
     * 小于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    public <R> Child lt(SFunction<T, R> getter, R value) {
        this.condition(new SqlConditionG(this.getFieldName(getter), SqlKeyword.LT.keyword, value));
        return returnValue();
    }

    /**
     * 小于等于
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     * 
     */
    public <R> Child le(SFunction<T, R> getter, R value) {
        this.condition(new SqlConditionG(this.getFieldName(getter), SqlKeyword.LE.keyword, value));
        return returnValue();
    }
    
    /**
     * 模糊查询
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     * 
     */
    public <R> Child like(SFunction<T, R> getter, R value) {
        this.condition(new SqlConditionG(this.getFieldName(getter), SqlKeyword.LIKE.keyword, value));
        return returnValue();
    }

    /**
     * 不模糊查询
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    public <R> Child notLike(SFunction<T, R> getter, R value) {
        this.condition(new SqlConditionG(this.getFieldName(getter), SqlKeyword.NOT_LIKE.keyword, value));
        return returnValue();
    }

    /**
     * in查询
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     * 
     */
    public <R> Child in(SFunction<T, R> getter, Collection<? extends R> value) {
        this.condition(new SqlConditionG(this.getFieldName(getter), SqlKeyword.IN.keyword, value));
        return returnValue();
    }

    /**
     * notin查询
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    public <R> Child notIn(SFunction<T, R> getter, Collection<? extends R> value) {
        this.condition(new SqlConditionG(this.getFieldName(getter), SqlKeyword.NOT_IN.keyword, value));
        return returnValue();
    }

    /**
     * 指定字段为空
     *
     * @param getter 对象getter方法
     * @return this
     * 
     */
    public Child isNull(SFunction<T, ?> getter) {
        this.condition(new SqlConditionG(this.getFieldName(getter), SqlKeyword.IS_NULL.keyword, null));
        return returnValue();
    }

    /**
     * 指定字段不为空
     *
     * @param getter 对象getter方法
     * @return this
     */
    public Child isNotNull(SFunction<T, ?> getter) {
        this.condition(new SqlConditionG(this.getFieldName(getter), SqlKeyword.IS_NOT_NULL.keyword, null));
        return returnValue();
    }

    public Child orderByAsc(SFunction<T, ?> getter) {
        this.sort(new SqlSortG(this.getFieldName(getter), false));
        return returnValue();
    }

    public Child orderByDesc(SFunction<T, ?> getter) {
        this.sort(new SqlSortG(this.getFieldName(getter), true));
        return returnValue();
    }

    /**
     * 位运算,具有指定值对应的位码
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    public <R> Child bitwiseWith(SFunction<T, R> getter, R value) {
        this.condition(new SqlConditionG(this.getFieldName(getter), SqlKeyword.BITWISE_WITH.keyword, value));
        return returnValue();
    }

    /**
     * 位运算,不具有指定值对应的位码
     *
     * @param getter 对象getter方法
     * @param value  值
     * @return this
     */
    public <R> Child bitwiseWithout(SFunction<T, R> getter, R value) {
        this.condition(new SqlConditionG(this.getFieldName(getter), SqlKeyword.BITWISE_WITHOUT.keyword, value));
        return returnValue();
    }

}