package com.example.test.impl;

import com.baomidou.dynamic.datasource.annotation.DS;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.example.test.entity.SysUser;
import com.example.test.mapper.SysUserMapper;
import com.example.test.vo.SysUserVO;
import io.github.bootystar.mybatisplus.enhancer.core.support.DynamicSqlService;
import org.springframework.stereotype.Service;

/**
 * 用户服务实现类
 *
 * @author bootystar
 */
@Service
@DS("postgresql")
public class SysUserDynamicSqlService extends ServiceImpl<SysUserMapper, SysUser> implements DynamicSqlService<SysUserVO> {
    


}
