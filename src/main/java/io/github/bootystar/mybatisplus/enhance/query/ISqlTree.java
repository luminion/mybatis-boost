package io.github.bootystar.mybatisplus.enhance.query;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * SQL树
 * @author bootystar
 */
public interface ISqlTree extends Iterable<ISqlTree> {

    Collection<? extends ISqlCondition> getConditions();

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
