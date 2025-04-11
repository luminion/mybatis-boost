package io.github.bootystar.mybatisplus.enhancer.query;

import java.util.Collection;

/**
 * @author bootystar
 */
public interface SqlEntity extends SqlTree {

    Collection<? extends SqlSort> getSorts();

}
