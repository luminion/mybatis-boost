package com.example.test.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.example.test.entity.SysUser;
import com.example.test.vo.SysUserVO;
import io.github.bootystar.mybatisplus.enhancer.core.DynamicMapper;
import org.apache.ibatis.annotations.Mapper;

import java.util.List;

/**
 * 用户Mapper
 *
 * @author bootystar
 */
@Mapper
public interface SysUserMapper extends BaseMapper<SysUser>, DynamicMapper<SysUserVO> {

}
