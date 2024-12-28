package io.github.bootystar.mybatisplus.enhance.query;

import java.util.Collection;

/**
 * @author bootystar
 */
public interface ISqlEntity extends ISqlTree {

    Collection<? extends ISqlSort> getSorts();

}
