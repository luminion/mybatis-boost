package io.github.bootystar.mybatisplus.enhance.core;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;

import java.util.List;


/**
 * mapper
 *
 * @author bootystar
 */
public interface DynamicMapper<T, V, S> extends BaseMapper<T> {

    List<V> listByDTO(S s, IPage<V> page);

}
