package io.github.bootystar.mybatisplus.enhancer.query;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * SQL树
 * @author bootystar
 */
public interface SqlTree extends Iterable<SqlTree> {

    Collection<? extends SqlCondition> getConditions();

    SqlTree getChild();

    @Override
    @SuppressWarnings("all")
    default Iterator<SqlTree> iterator() {
        return new Itr(this);
    }

    class Itr implements Iterator<SqlTree> {

        private SqlTree current;

        public Itr(SqlTree root) {
            current = root;
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public SqlTree next() {
            if (current == null) {
                throw new NoSuchElementException();
            }
            SqlTree result = current;
            current = current.getChild();
            return result;
        }
    }

}
