package io.github.bootystar.mybatisplus.enhancer.query.core;

import java.util.Collection;

/**
 * @author bootystar
 */
public interface ISqlEntity extends ISqlTree {

    Collection<ISqlSort> getSorts();

}
