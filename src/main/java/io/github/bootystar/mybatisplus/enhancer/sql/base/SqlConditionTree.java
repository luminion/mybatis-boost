package io.github.bootystar.mybatisplus.enhancer.sql.base;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * SQL树
 * @author bootystar
 */
public interface SqlConditionTree extends Iterable<SqlConditionTree> {

    Collection<? extends SqlCondition> getConditions();

    SqlConditionTree getChild();

    @Override
    @SuppressWarnings("all")
    default Iterator<SqlConditionTree> iterator() {
        return new Itr(this);
    }

    class Itr implements Iterator<SqlConditionTree> {

        private SqlConditionTree current;

        public Itr(SqlConditionTree root) {
            current = root;
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public SqlConditionTree next() {
            if (current == null) {
                throw new NoSuchElementException();
            }
            SqlConditionTree result = current;
            current = current.getChild();
            return result;
        }
    }

}
