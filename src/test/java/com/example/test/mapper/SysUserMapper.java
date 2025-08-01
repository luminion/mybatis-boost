package com.example.test.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.example.test.entity.SysUser;
import com.example.test.vo.SysUserVO;
import io.github.bootystar.mybatisplus.enhancer.EnhancedMapper;
import org.apache.ibatis.annotations.Mapper;

/**
 * 用户Mapper
 *
 * @author bootystar
 */
@Mapper
public interface SysUserMapper extends BaseMapper<SysUser>, EnhancedMapper<SysUserVO> {

}
