package io.github.bootystar.mybatisplus.enhancer.query.core;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * SQL树
 * @author bootystar
 */
public interface ISqlTree extends Iterable<ISqlTree> {

    /**
     * 条件列表
     *
     * @return 条件列表
     */
    Collection<ISqlCondition> getConditions();

    /**
     * 用于连接本层级条件的符号
     * 
     * @return 符号 
     */
    String getSymbol();

    /**
     * 子条件
     *
     * @return {@link ISqlTree }
     */
    ISqlTree getChild();
    
    

    @Override
    @SuppressWarnings("all")
    default Iterator<ISqlTree> iterator() {
        return new Itr(this);
    }

    class Itr implements Iterator<ISqlTree> {

        private ISqlTree current;

        public Itr(ISqlTree root) {
            current = root;
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

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
