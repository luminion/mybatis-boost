package io.github.luminion.mybatis.query.core;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * SQL 条件树接口.
 * <p>
 * 定义了用于构建复杂 SQL 查询的树形结构. 每个节点可以包含一组条件和逻辑连接符, 并可以拥有一个子树.
 *
 * @author luminion
 * @since 1.0.0
 */
public interface ISqlTree extends Iterable<ISqlTree> {

    /**
     * 获取当前节点的条件列表.
     *
     * @return 条件列表
     * @since 1.0.0
     */
    Collection<ISqlCondition> getConditions();

    /**
     * 获取用于连接当前节点条件的逻辑连接符 (例如 AND, OR).
     *
     * @return 连接符
     * @since 1.0.0
     */
    String getConnector();

    /**
     * 获取子条件树.
     *
     * @return 子条件树, 可能为 null
     * @since 1.0.0
     */
    ISqlTree getChild();

    /**
     * 返回一个用于遍历 SQL 树节点的迭代器.
     *
     * @return 迭代器
     * @since 1.0.0
     */
    @Override
    @SuppressWarnings("all")
    default Iterator<ISqlTree> iterator() {
        return new Itr(this);
    }

    /**
     * SQL 树的迭代器实现.
     * @since 1.0.0
     */
    class Itr implements Iterator<ISqlTree> {

        private ISqlTree current;

        /**
         * 构造一个从指定根节点开始的迭代器.
         *
         * @param root 树的根节点
         * @since 1.0.0
         */
        public Itr(ISqlTree root) {
            current = root;
        }

        /**
         * {@inheritDoc}
         * @since 1.0.0
         */
        @Override
        public boolean hasNext() {
            return current != null;
        }

        /**
         * {@inheritDoc}
         * @since 1.0.0
         */
        @Override
        public ISqlTree next() {
            if (current == null) {
                throw new NoSuchElementException();
            }
            ISqlTree result = current;
            current = current.getChild();
            return result;
        }
    }

}