package com.example.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.example.entity.SysUser;
import com.example.vo.SysUserVO;
import io.github.luminion.sqlbooster.extension.mybatisplus.BoosterMpMapper;
import org.apache.ibatis.annotations.Mapper;

/**
 * 用户Mapper
 *
 * @author bootystar
 */
@Mapper
public interface SysUserMapper extends BaseMapper<SysUser>,
        BoosterMpMapper<SysUser, SysUserVO> {
    

}
